library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)

## Regression Tree

## Same DataPrep Steps As earlier module
## Copying As is from linear regression

ld_train=read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\loan_data_train.csv",stringsAsFactors = F)
ld_test= read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\loan_data_test.csv",stringsAsFactors = F)

ld_test$Interest.Rate=NA

ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)

head(ld_all)

# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL

library(dplyr)

ld_all=ld_all %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
         )

ld_all=ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)

ld_all=ld_all %>% 
  mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
         lp_11=as.numeric(Loan.Purpose %in% c("major_purchase","medical","car")),
         lp_12=as.numeric(Loan.Purpose %in% c("vacation","wedding","home_improvement")),
         lp_13=as.numeric(Loan.Purpose %in% c("other","small_business","credit_card")),
         lp_14=as.numeric(Loan.Purpose %in% c("debt_consolidation","house","moving"))) %>% 
  select(-Loan.Purpose)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

ld_all=CreateDummies(ld_all ,"State",100)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)

library(tidyr)

ld_all=ld_all %>% 
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)

ld_all=CreateDummies(ld_all,"Employment.Length",100)

ld_all=ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[ld_all$data=='train',col],na.rm=T)
  }
  
}

## separate train and test

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)

##

set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]

### Building A Deecison Tree
### A decision tree can be used both for classification and regression, given the tree is CART
### CART are not susceptible to multicollinearity,  outlier or missing, you can safely ignore them
### CART can also do recursive partition
### CART only does binary splits

### Remember a full grown complicated Decision tree is
### high on variance low on bias
### These types of trees are also very specific
### A stump (depth = 1) decision tree on the other hand 
### low on varaince high on bias and little bit more generic
### A single decision tree always overfit, as overfit is due to the high variance,
### Underfitting happens due to high bias due to a more generic approach
### below is the case where we not pruning, we are only growing the tree
### so its a case of high variance, low bias. If you however take a stump tree(depth=1) then you will have
### High Bias and low variance.

ld.tree=tree(Interest.Rate~.-ID,data=ld_train1)

## Tree in text format

ld.tree

## Visual Format

plot(ld.tree)
text(ld.tree)

## Performance on validation set
train.IR = predict(ld.tree, newdata=ld_train1)

ld_train1$train_pred <- train.IR

head(ld_train1[,c('train_pred', 'Interest.Rate')])

val.IR=predict(ld.tree,newdata = ld_train2)

ld_train2$test_pred <- val.IR

head(ld_train2[,c('test_pred', 'Interest.Rate')])



rmse_val=((val.IR)-(ld_train2$Interest.Rate))^2 %>% mean() %>% sqrt()
rmse_val

(2.32-2.20)/2.2

rmse_train=((train.IR)-(ld_train1$Interest.Rate))^2 %>% mean() %>% sqrt()
rmse_train


## Making final model on entire training
## and prediction on test/production data

ld.tree.final=tree(Interest.Rate~.-ID,data=ld_train)
test.pred=predict(ld.tree.final,newdata=ld_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)

## Classification tree
## Data prep same as earlier module

rg_train=read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\rg_train.csv",stringsAsFactors = FALSE)
rg_test =read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\rg_test.csv",stringsAsFactors = FALSE)

rg_test$Revenue.Grid=NA

rg_train$data='train'
rg_test$data='test'

rg=rbind(rg_train,rg_test)

rg = rg %>%
  mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
         children=as.numeric(children))

rg=rg %>%
mutate(a1=as.numeric(substr(age_band,1,2)),
       a2=as.numeric(substr(age_band,4,5)),
      age=ifelse(substr(age_band,1,2)=="71",71,ifelse(age_band=="Unknown",NA,0.5*(a1+a2)))
       ) %>%
  select(-a1,-a2,-age_band)

cat_cols=c("status","occupation","occupation_partner","home_status",
           "family_income","self_employed",
           "self_employed_partner","TVarea","gender","region")

for(cat in cat_cols){
  rg=CreateDummies(rg,cat,50)
}

rg=rg %>%
  select(-post_code,-post_area)

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)

for(col in names(rg)){
  
  if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
    
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)

## For classification tree we'll need to convert response to factor type

rg$Revenue.Grid=as.factor(rg$Revenue.Grid)

## Rest of the data prep steps


for(col in names(rg)){
  
  if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
    
    rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
  }
  
}

rg_train=rg %>% filter(data=='train') %>% select(-data)
rg_test=rg %>% filter(data=='test') %>% select (-data,-Revenue.Grid)

set.seed(2)
s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
rg_train1=rg_train[s,]
rg_train2=rg_train[-s,]

## building tree 


required_vars <-
  c(
    #  "children",
    #  "family_income_LT_30000GT_EQ_27500",
    #  "TVarea_ScottishTV",
    #  "self_employed_partner_No",
    #  "Medical.Insurance",
    "Balance.Transfer",
    #  "Average.A.C.Balance",
    "Home.Loan",
    "Term.Deposit",
    "Life.Insurance",
    "Investment.Tax.Saving.Bond",
    "Average.Credit.Card.Transaction"
    #    "Personal.Loan"
    #    "Online.Purchase.Amount"
  )

form <-
  as.formula(paste0('Revenue.Grid ~ ', paste0(required_vars, collapse = ' + ')))



rg.tree=tree(form , rg_train1)
 
## Performance on validation set

val.score=predict(rg.tree,newdata = rg_train2,type='vector')[,1]
pROC::roc(rg_train2$Revenue.Grid,val.score)$auc

train.score=predict(rg.tree,newdata = rg_train1,type='vector')[,1]
pROC::roc(rg_train1$Revenue.Grid,train.score)$auc



## build model on entire data

# rg.tree.final=tree(Revenue.Grid~.-REF_NO-Investment.in.Commudity
#                   -Investment.in.Derivative-Investment.in.Equity
#                   -region_SouthEast-TVarea_Central-occupation_Professional
#                   -family_income_GT_EQ_35000-region_Scotland-Portfolio.Balance,
#                   data=rg_train)
# 
## Probability score prediciton on test/production data

test.score=predict(rg.tree.final,newdata=rg_test,type='vector')[,1]
write.csv(test.score,"mysubmission.csv",row.names = F)

## For hardclass prediciton we'll need to find a cutoff on score
## Process is same as for logistic regression

train.score=predict(rg.tree.final,newdata=rg_train,type='vector')[,1]
real=rg_train$Revenue.Grid

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

## Once we know the cutoff we can use it to convert test score to 
## hard classes

test.predicted=as.numeric(test.score>my_cutoff)
write.csv(test.predicted,"proper_submission_file_name.csv",row.names = F)

## Regression Random Forest with Parameter Tuning
## In Random Forest : we have a bootstrapped sampled data
## Random Forest also randomises the number of variables using mtry
## Being a Bagging technique they suffer sometimes High on bias(not always) even though they are full grown trees,
## most of the time RF does a decent job in lowering the bias and variance
## Random Forest can work both with classification and regression
## mtry: Number of varialbes usually taken in a range of square root of 
## total number of variables
## ntree: Total number of treees that you want to grow
## nodesize: Mininmum number of terminal nodes can be till 8(high value can make overfitting)
## maxnodes: Maximum number of terminal nodes it will always be greater than nodesize and can be till 32(Again high value can lead to overfitting).

## function for getting all possible combinations : expand.grid
## Creating a grid search data here for random Forest to Run upon

params=list(mtry=c(5,10),ntree=c(100,500),
                 maxnodes=c(15,20),nodesize=(c(2,5)))
expand.grid(params)

## paramter values that we want to try out

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))

expand.grid(param)
## Function for selecting random subset of params

## Sampling of Grid search to create randomized grid search
subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=50
my_params=subset_paras(param,num_trials)
dim(my_params)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999




for(i in 1:num_trials){
  # print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  ## cvTuning gives lot of info we are only interested in minimising the
  ## error (variance) for regression, maximising the auc in case of
  ## binary classification, see the classification code for cvTuning
  
  k=cvTuning(randomForest,Interest.Rate~.-ID,
             data =ld_train1,
             tuning =params,
             folds = cvFolds(nrow(ld_train1), K=10, type = "random"),
             seed =2
             )
  score.this=k$cv[,2]
  print(score.this)
  if(score.this<myerror){
    # print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    # print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  # print('DONE')
  # uncomment the line above to keep track of progress
}

best_params

## from another run following values were obtained

# myerror=1.870957
# best_params=data.frame(mtry=20,
#                       ntree=200,
#                       maxnodes=50,
#                       nodesize=10)

## Final model with obtained best parameters

ld.rf.final=randomForest(Interest.Rate~.-ID,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ld_train)

test.pred=predict(ld.rf.final,newdata = ld_test)

ld_train1$train_pred <- NULL

ld.rf_tr=randomForest(Interest.Rate~.-ID,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ld_train1)

train.pred = predict(ld.rf_tr, newdata = ld_train1)
test.pred = predict(ld.rf_tr, newdata = ld_train2)

errors=ld_train2$Interest.Rate-test.pred
errors**2 %>% mean() %>% sqrt()

errors=ld_train1$Interest.Rate-train.pred
errors**2 %>% mean() %>% sqrt()



write.csv(test.pred,"mysubmission.csv",row.names = F)


## Variable IMportance plot to see which all variables are important
varImpPlot(ld.rf.final)

d=importance(ld.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(IncNodePurity))

## Varimp Plot

varImpPlot(ld.rf.final)

## Partial Dependence Plot

var='fico'

pred.resp = predict(ld.rf.final,newdata=ld_train)
myvar = ld_train[,var]

trend.data=data.frame(Response=pred.resp,myvar=myvar)

trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_smooth()

## Classfication with Random Forest

ci_train=read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\Data\\census_income.csv",
                  stringsAsFactors =F)
glimpse(ci_train)

ci_train=ci_train %>% select(-education)
ci_train$Y=as.numeric(ci_train$Y==" >50K")
ci_train$Y=as.factor(ci_train$Y)

cat_var=names(ci_train)[sapply(ci_train,is.character)]


for(var in cat_var){
  ci_train=CreateDummies(ci_train,var,500)
}

param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
           )

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  #print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Y~., 
             data =ci_train,
             tuning =params,
             folds = cvFolds(nrow(ci_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
             )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  #print('DONE')
  # uncomment the line above to keep track of progress
}

## Values obtained from an earlier run 

# myauc=0.8945485
# best_params=data.frame(mtry=5,
#                        ntree=500,
#                        maxnodes=100,
#                        nodesize=10)

## Model on the entire training data

ci.rf.final=randomForest(Y~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ci_train
                         )

## Follow the same process as earlier for prediction on
## test production data

# test.score=predict(ci.rf.final,newdata = ci_test,type='prob')[,1]
# write.csv(test.score,'mysubmission.csv',row.names = F)     

ci.rf.final

## Variable IMportance

d=importance(ci.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

## Varimp Plot

varImpPlot(ci.rf.final)

## Partial Dependence Plot

var='age'

pred.resp = predict(ci.rf.final,newdata=ci_train,type='prob')[,1]
myvar = ci_train[,var]

trend.data=data.frame(Response=pred.resp,myvar=myvar)

trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_smooth()                    
