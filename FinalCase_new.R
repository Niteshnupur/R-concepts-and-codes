# setwd("/Users/lalitsachan/Desktop/March onwards/CBAP with R/Data/")

# importing data

alldata=read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\Data\\adult.csv",stringsAsFactors = F,header = F)

names(alldata)

head(alldata)


names(alldata)=c("age","workclass","fnlwgt","education","education.num","marital.status",
                 "occupation","relationship","race","sex","capital.gain","capital.loss","hpw",
                 "native.country","target")

head(alldata)
alldata$target <- trimws(alldata$target)
#alldata$target <- gsub('\\s+','', alldata$target)

names(prop.table(table(alldata$target)))


alldata$target= (alldata$target==">50K") + 0



prop.table(table(alldata$target))

library(dplyr)

glimpse(alldata)

quantile(alldata$age)
alldata$age_cat <- ifelse(alldata$age <= 28, 1, ifelse(alldata$age <= 37, 2, ifelse(alldata$age <= 48, 3, 4)))

alldata$age

alldata$age_cat



quantile(alldata$age)
cut(alldata$age, breaks= c(-Inf,28, 37, 48, Inf), labels=c("<28",2,3,4))
Hmisc::cut2

## categorize it!!!

summary(alldata$age)

library(ggplot2)
ggplot(data=alldata, aes(x = age)) + geom_density()

table(alldata$age_cat, alldata$target)



alldata$age_cat <- NULL

table(alldata$workclass)

round(prop.table(table(alldata$workclass,alldata$target),1),2)

table(alldata$workclass)

## groups = 
# 1 :"Never-worked","Without-pay" 
# 2 : "Self-emp-not-inc","State-gov","Local-gov"
# 3 : "?" , 
# 4 :"Self-emp-inc", 
# 5 :"Private" , 
# 6 :"Federal-gov"

alldata$workclass <- trimws(alldata$workclass)

only_Chars <- sapply(alldata, is.character)
only_Chars <- only_Chars[only_Chars==T]

alldata[,only_Chars] <-  lapply(alldata[,only_Chars], function(x)trimws(x))

unique(alldata$workclass)

str(alldata)


alldata= alldata %>%
  mutate(wc1 = as.numeric(workclass == "Self-emp-not-inc"),
         wc2=as.numeric(workclass %in% c("State-gov","Local-gov")),
         wc3=as.numeric(workclass=="?"),
         wc4=as.numeric(workclass=="Self-emp-inc"),
         wc5=as.numeric(workclass=="Private"),
         wc6=as.numeric(workclass=="Federal-gov")
         ) %>%
  select(-workclass)

glimpse(alldata)

table(alldata$education)

round(prop.table(table(alldata$education,alldata$target),1),2)

#groups 
# 1 : "10th","11th","12th","1st-4th","5th-6th","7th-8th","9th","Preschool"
# 2 : "Some-college","HS-grad" 
# 3 :"Assoc-voc","Assoc-acdm" 
# 4 :"Prof-school","Doctorate"
# 5 : "Bachelors"
tofix <- names(alldata)[sapply(alldata, is.character)]

## fixing the leading blanks in the data using lapply

alldata[,tofix] <- lapply(alldata[,tofix], trimws)

alldata=alldata %>%
  mutate(ed2=as.numeric(education %in% c("Some-college","HS-grad")),
         ed3=as.numeric(education %in% c("Assoc-voc","Assoc-acdm")),
         ed4=as.numeric(education %in% c("Prof-school","Doctorate")),
         ed5=as.numeric(education=="Bachelors")) %>%
  select(-education)

glimpse(alldata)

table(alldata$marital.status)

round(prop.table(table(alldata$marital.status,alldata$target),1),2)

alldata=alldata %>%
  mutate(ms1=as.numeric(marital.status %in% c("Married-AF-spouse","Married-civ-spouse"))) %>%
  select(-marital.status)

glimpse(alldata)

table(alldata$occupation)

round(prop.table(table(alldata$occupation,alldata$target),1),1)

# 1 : " Exec-managerial" , 
# 2 :" Prof-specialty" , 
# 3 :" Protective-serv"," Sales"," Tech-support"
# 4 :" Transport-moving"," Craft-repair" 
# 5 :" ?" , " Adm-clerical"," Armed-Forces"," Farming-fishing"," Handlers-cleaners"," Machine-op-inspct"
# 6 :" Other-service"," Priv-house-serv"

alldata=alldata %>%
  mutate(oc1=as.numeric(occupation %in% c("Exec-managerial")),
         oc2=as.numeric(occupation %in% c("Prof-specialty")),
         oc3=as.numeric(occupation %in% c("Protective-serv","Sales","Tech-support")),
         oc4=as.numeric(occupation %in% c("Transport-moving","Craft-repair")),
         oc6=as.numeric(occupation %in% c("Other-service","Priv-house-serv"))
         ) %>%
  select(-occupation)


glimpse(alldata)

table(alldata$relationship)

round(prop.table(table(alldata$relationship,alldata$target),1),1)

alldata=alldata %>%
  mutate(rl1=as.numeric(relationship %in% c("Not-in-family","Unmarried","Other-relative","Own-child"))
         ) %>%
  select(-relationship)

glimpse(alldata)

table(alldata$race)

round(prop.table(table(alldata$race,alldata$target),1),1)

alldata=alldata %>%
  mutate(race1=as.numeric(race %in% c("White","Asian-Pac-Islander"))
  ) %>%
  select(-race)

glimpse(alldata)

table(alldata$sex)

alldata=alldata %>%
  mutate(sexF=as.numeric(sex=="Female")) %>%
  select(-sex)


table(alldata$native.country)
round(prop.table(table(alldata$native.country,alldata$target),1),1)
## we are leaving this for you to explore

alldata=select(alldata,-native.country)
str(alldata)
### breaking data into train , test

set.seed(2)
s=sample(1:nrow(alldata),0.75*nrow(alldata))

train=alldata[s,]
test=alldata[-s,]

train$age <- as.numeric(train$age)
train$fnlwgt <- as.numeric(train$fnlwgt)
train$education.num <- as.numeric(train$education.num)
train$capital.gain <- as.numeric(train$capital.gain)
train$capital.loss <- as.numeric(train$capital.loss)
train$hpw <- as.numeric(train$hpw)
train$target <- as.numeric(train$target)


test$age <- as.numeric(test$age)
test$fnlwgt <- as.numeric(test$fnlwgt)
test$education.num <- as.numeric(test$education.num)
test$capital.gain <- as.numeric(test$capital.gain)
test$capital.loss <- as.numeric(test$capital.loss)
test$hpw <- as.numeric(test$hpw)
test$target <- as.numeric(test$target)

## develope logistic regression model for train

# take care of vif first 
library(car)
vif_train=lm(target~ .,data=train)

lapply(alldata, function(x)all(x == 0))

glimpse(train)
alias_dt <- alias(vif_train)
names(alias_dt)
alias_dt['Complete']
names(train)
rownames(data.frame(alias_dt$Complete))

alias_dt[['Complete']]
alias_dt$Complete
names(alias_dt)

# var_aliased_table <- data.frame(t(alias_dt$Complete))
names(var_aliased_table)



## creating the formula by removing the aliased variables
## There is none in our case

form <-
  paste0('target ~', paste0(setdiff(names(train), c('target','wc5', 'rl1', 'fnlwgt'
  )), collapse = ' + '))
form <- as.formula(form)

## removing the vif here step by step
sort(vif(vif_train),decreasing = T)

vif_train=lm(target~.-wc5,data=train)
sort(vif(vif_train),decreasing = T)

vif_train=lm(target~.-wc5-rl1,data=train)
sort(vif(vif_train),decreasing = T)
class(train$target)
train$target <- as.factor(train$target)

fit_train = glm(target ~ . - wc5 - rl1, data = train, family = "binomial")

fit_train=step(fit_train)
## final fit formula that we are going to use
# form <- as.formula(paste0("target ~" , paste0(setdiff(names(fit_train$coefficients), c('capital.gain','(Intercept)')) , collapse="+")))

form <- as.formula("target ~ age + education.num + capital.gain + capital.loss + 
                     hpw +  ed3 + ed5 + ms1 + oc1 + oc2 + oc3 + 
                     oc4 + oc6 + race1")
fit_train = glm(target ~  age + education.num  + capital.loss + hpw  + ed3 + ed5 + ms1 + oc1 + oc2 + oc3 + 
                  oc4 + oc6 + race1 , data = train, family = "binomial")

summary(fit_train)


## predicting the response
score.train=predict(fit_train,train,type = "response")
real=train$target

# finding cutoff based on KS from train data

cutoffs=round(seq(0,1,length=100),3)
cutoff_data=data.frame(cutoff=999,KS=999)

for(cutoff in cutoffs){
  pred=as.numeric(score.train>cutoff)
  TP=sum(real==pred & real==1)
  TN=sum(real==pred & real==0)
  FP=sum(real!=pred & real==0)
  FN=sum(real!=pred & real==1)
  P=TP+FN
  N=TN+FP
  KS=(TP/P)-(FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}

cutoff_data=cutoff_data[-1,]
KS.cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]




# performance of this logistic model on the test data
## test performance
test$score=predict(fit_train,newdata=test,type="response")

# test$hard_class <- (test$score > .222)+0

test$predicted=as.numeric(test$score>KS.cutoff)

## train performance
train$score=predict(fit_train,newdata=train,type="response")
train$predicted=as.numeric(train$score>KS.cutoff)

table(test$target,test$predicted)

## comparing auc from train and test
caTools::colAUC(test$predicted, test$target, plotROC = TRUE)
caTools::colAUC(train$predicted, train$target, plotROC = TRUE)
# Performance
# Acc=(4816+1654)/8141=0.795
# Sn=1654/1963 =0.843
# Sp=4816/6178 =0.779
## DTree
library(tree)

train$target=as.factor(train$target)
single.tree=tree(target~.,data=train)


cv.single.tree=cv.tree(single.tree,FUN=prune.misclass)

plot(cv.single.tree$size,cv.single.tree$dev,type='b')

pruned.tree=prune.misclass(single.tree,best=5)

plot(pruned.tree)
text(pruned.tree,pretty = 0)

target.tree=predict(pruned.tree,newdata=test,type="class")

caTools::colAUC(as.numeric(target.tree), as.numeric(test$target ))

table(test$target,target.tree)
# Performance
Acc=(5862+1025)/8141=0.846
Sn=1025/1963= 0.522
Sp=5862/6178=0.949
## Random Forest
library(gbm)
library(randomForest)
rf=randomForest(form,data=train)
summary(rf)

test.rf=predict(rf,newdata=test)

## Variable importance
caret::varImp(rf)
## OR use below command
varImpPlot(rf)

table(test$target,test.rf)
caTools::colAUC(as.numeric(test.rf), as.numeric(test$target ))
# Performance
Acc=(1228+5796)/8141 =0.863
Sn=1228/1963 =0.626
Sp=5796/6178 =0.938
# AUC ROC for logistic

library(pROC)
roccurve = roc(train$target ~ score.train)
plot(roccurve)
auc(roccurve)

## data.table 
## Use setDT to convert a data.frame to data.table
## for iris data setDT will not work as its an inbuilt data
## hence using data.table command here

library(data.table)
ir <- data.table(iris)
head(ir)

ir[Sepal.Length > 4.5, sum(Sepal.Width),.(Species)]
