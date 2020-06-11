

#####  we accessing the files from directory




##### reading the rg_train file from directory

rg_train = read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\rg_train.csv", stringsAsFactors = FALSE)

rg_train

head(rg_train)



##### reading the rg_test file from directory

rg_test = read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\rg_test.csv", stringsAsFactors = FALSE)

rg_test

head(rg_test)



### for showing what type of data is present (for ex :- char or int)

str(rg_train)

str(rg_test)


###### for showing the how many rows and coloumn are present

dim(rg_train)

dim(rg_test)


names(rg_train)
names(rg_test)

# setdiff(c(1,2,3,4), c(3,3,4,5))

setdiff(names(rg_train), names(rg_test))


##### adding the one more coloumn rg_test$Revenue.Grid=NA cz this is dependant vairable
#####          ( adding in test coloumn )

rg_test$Revenue.Grid=NA


##### now check the dimension

dim(rg_test)


##### showing the content of revenue grid and their unique values

table(rg_train$Revenue.Grid)




#### adding the one more coloumn in the train data set as name data

rg_train$data='train'

### for adding the one more coloumn in the test data set as the name data

rg_test$data='test'


######### for showing the variables name


names(rg_train)

names(rg_test)


##### for combining both the data set we use rbind function

rg=rbind(rg_train,rg_test)

rg

head(rg)

dim(rg)


##### for showing the rg$children

table(rg$children)


####### Removing the unnecessary symbols to convert it to numeric
## Remember , try to get as much as numeric columns , they are more powerful

library(dplyr)

rg = rg %>%
  mutate(children = ifelse(children == "Zero", 0, substr(children, 1, 1)),
         children = as.numeric(children))



rg=rg %>% 
  mutate(children = ifelse(children == "Zero" , 0 , substr(children , 1 , 1)) , 
         children = as.numeric(children))


##### again we are checking the  rg$children for we did changes in that

table(rg$children)


##### using glimpse for showing data which type it is

glimpse(rg)



######### for showing the rg$age_band

table(rg$age_band)



####  converting the age_band in numeric and taking the average also

rg=rg %>%
  mutate(a1=as.numeric(substr(age_band,1,2)),
         a2=as.numeric(substr(age_band,4,5)),
         age=ifelse(substr(age_band,1,2)=="71",71,
                    ifelse(age_band=="Unknown",NA,0.5*(a1+a2)))
  ) %>%
  select(-a1,-a2,-age_band)


######## summary

summary(rg$age)


rg= rg %>% 
  mutate(a1 = as.numeric(substr(age_band , 1 , 2)),
         a2 = as.numeric(substr(age_band , 4 , 5)),
         age = ifelse(substr(age_band , 1 , 2) == "71" , 71,
        
                 ifelse(age_band == "Unknown" , NA , 0.5*(a1+a2)))) %>% 
  select(-a1 , -a2 , -age_band)


########### summary

summary(rg$age)

glimpse(rg)


######## with another method


unlist(lapply(strsplit(c('25-35', '35-45'), split = '-'), function(x)
  (mean(as.numeric(x)))))


##### we could have done something similar for variable family income also


#### for showing the unique values for entire data set

lapply(rg , function(x)  unique(x))

lapply(rg , function(x) length( unique(x)))



#### for calculating which coloumns are in charecter form

### getting the answer in true or false format
lapply(rg , function(x) is.character(x))


### getting the answer in 1 or 0 format
lapply(rg , function(x) sum(is.character(x)))



### using sapply()
names(rg)[sapply(rg , function(x) is.character(x))]


str(rg)
#### this are the variables name which are in character form
## [1] "status"                "occupation"           
## [3] "occupation_partner"    "home_status"          
## [5] "family_income"         "self_employed"        
## [7] "self_employed_partner" "TVarea"               
## [9] "post_code"             "post_area"            
## [11] "gender"                "region"               
## [13] "data"



## for showing the values of data coloumn

table(rg$data)

## we'll exclude column named data as it simply represent which dataset the observation is from

cat_cols1 = c (
  
  "status"    ,            
  "occupation",
  "occupation_partner",    
  "home_status" ,
  "family_income",         
  "self_employed",
  "self_employed_partner", 
  "TVarea",
  "gender" ,               
  "region"
  
  )


cat_cols1


#### now we are creating the dummy values of this variables


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var]) ## getting the table for the variable(any categorical variable)
  t=t[t>freq_cutoff] ## cutoff is the frequency of occurance of a variable default is 0 , but ideally it should be atleast 15-20% of actual data,
  ## so here whatever categories which are less than that cut off frequencies are dropped(no dummes are created for them)
  t=sort(t) ## sort the data
  categories=names(t)[-1] ## pick everything but exclude the first as it has lowest frequency: REmember its n-1
  
  for( cat in categories){
    name=paste(var,cat,sep="_") ## Inside the for loop create a name separated by name of variable and category separeted by "_" underscore
    name=gsub(" ","",name) ## replace any spaces if there is found in categoreis of variables
    name=gsub("-","_",name) ## replace any dash if found in categories to underscropes: e.g. 'Cat-1', 'Cat-2' will be 'Cat_1', 'Cat_2'
    name=gsub("\\?","Q",name) ## any question mark is converted to 'Q'
    name=gsub("<","LT_",name) ## Less than sign is converted to LT_
    name=gsub("\\+","",name) ## + sign is removed
    name=gsub("\\/","_",name) ## "/" is replaced with "_"
    name=gsub(">","GT_",name) ## ">" is replaced with 'GT_'
    name=gsub("=","EQ_",name) ## '=' is replaced with 'EQ_'
    name=gsub(",","",name) ##  ',' is replaced with ''
    data[,name]=as.numeric(data[,var]==cat) ## changing to numeric type
  }
  
  data[,var]=NULL
  return(data)
}



## picking all the character columns and creating dummies

cat_cols1

for(cat in cat_cols1){
  rg=CreateDummies(rg,cat,50)
}


for(cat in cat_cols1){
  
  rg=CreateDummies(rg , cat , 50)
}


head(rg)

dim(rg)

glimpse(rg)



## Removing the garbage columns which contains a lot of categories
## so we are removing post code amd post area by using select function

rg=rg %>%
  select(-post_code,-post_area)

rg = rg %>% 
  select(-post_code , -post_area)

names(rg)


### for showing the there is any character coloumn is present

## we are getting value in the true or false format
sapply(rg , function(x) is.character(x))

## we are getting the value in 0 or 1 format
sapply(rg , function(x) sum(is.character(x)))

## we are getting the overall value
sum(sapply(rg , function(x) is.character(x)))

## only one variable is in character format
# that tells us that there are no character columns remaining [ 1 comes for column 'data']



##### for showing the unique with values of revenue.grid

table(rg$Revenue.Grid)


rg$Revenue.Grid = rg$Revenue.Grid==1
rg$Revenue.Grid

## converting to 1, 0 of dependent variable
## now we are converting the  true or false format into 0 or 1 format
## on the dependant variable

rg$Revenue.Grid=(rg$Revenue.Grid==1)+0
rg$Revenue.Grid


# (x > 10)*20 + (x <= 10)*30

table(rg$Revenue.Grid)


##### for finding the how many missing values are present in entire data

##### we are getting the values in true or false format
lapply(rg , function(x) is.na(x))


##### we are getting the values in 0 and 1 format
lapply(rg , function(x) sum(is.na(x)))


### for calculating the values of rg$children and rg$Revenue.Grid

table(rg$children, rg$Revenue.Grid)



x <- dt %>% 
  group_by(Type) %>% 
  summarise('mean'= mean(Price)) 

# aggregate( Price ~ Type, data = dt, mean)



901936 + 392
## Rules, which needs to be understood before you do any imputation on missing or outlier
## always first choose the train data to impute , get the value of mean of train data for each column
## then, impute the missing for both train and test with the same data of which you saved earlier
## why we use train data mean to impute for test? because for unseen data you don't know the mean hence we assume the train data as true representation of population
## hence we impute our any test or out of time data with train data info
## also this apporach brings more consitency

## In our case we haven't done so , because we don't have a lot of missing values, we only have one column with 55 missing info
## so for a small data, this generic approach will work

## converting the missing to the mean of a column

## Imputation using for loop:
# for(col in names(rg)){
#   
#   if(sum(is.na(rg[,col]))>0 & !(col %in% c("data","Revenue.Grid"))){
#     
#     rg[is.na(rg[,col]),col]=mean(rg[rg$data=='train',col],na.rm=T)
#   }
#   
# }





## Imputaion using lapply
## doing the above with our new approach using lapply

replace_mean <- function(x){
  x[is.na(x)] <- mean(x, na.rm=T)
  return(x)
}


replace_mean(c(1,2,3,NA,4,5,NA))


##########

x = c(-4,-1,-2,0,1,2,3, 4,5, 6,7,9,10,11,12,13,14,15,16,17,23,100)

mean(x)
sd(x)



## function for removing outlier

replace_outlier = function(x) {
  right_side = mean(x) + 3*sd(x)
  left_side = mean(x) - 3*sd(x)
  x = ifelse(x >  right_side , right_side , x )
  x = ifelse(x < left_side , left_side , x )

  return(x)  
        }

x = c(-4,-1,-2,0,1,2,3, 4,5, 6,7,9,10,11,12,13,14,15,16,17,23,100)

replace_outlier(x)


replace_outlier <- function(x){
  
  right_side = mean(x) + 3*sd(x)
  left_side = mean(x) - 3*sd(x)
  x <- ifelse(x > right_side, right_side,x)
  x <- ifelse(x < left_side, left_side, x)
  return(x)
}

replace_outlier(x)





## replacing the mean by using the replace_mean function that we created before


## we are getting the all the variables
rg[names(rg)]



### we are using the setdiff function for removing the ("data" , "Revenue.Grid") from entire data (names(rg))
## and using lapply function for replacing the mean by using the replace_mean function on the remaning data


## using setdiff function for removing the ("data" , "Revenue.Grid") 

rg[,setdiff(names(rg) , c( "data" , "Revenue.Grid"))]



#### using lapply function

rg[,setdiff(names(rg) , c("data" , "Revenue.Grid"))] = 
  
  lapply(rg[,setdiff(names(rg) , c("data" , "Revenue.Grid"))] , replace_mean)


summary(rg$c)


### for showing the content of the variables

glimpse(rg)

## rg[, setdiff(names(rg), c('data', 'Revenue.Grid'))] <-
##  lapply(rg[, setdiff(names(rg), c('data', 'Revenue.Grid'))], replace_mean)



## now we are splitting the data set again for the modeling


##### separate train data set by using filter function and removing the data coloumn

##### we are creating the rg_train and rg_test data set from entire data set(rg)

rg_train = rg %>% 
  filter(data == "train") %>% 
  select(-data)


head(rg_train)


rg_train=rg %>% 
  filter(data=='train') %>% 
  select(-data)


head(rg_train)



## separate test data set by using filter function and removing the data coloumn that contains the test value
####  and removing the revenue.grid coloumn also


### the value of Revenue.Grid recieved from train data set will be imposed(apply) on test data set


rg_test = rg %>% 
  filter(data == "test") %>% 
  select(-data , -Revenue.Grid)


head(rg_test)


rg_test=rg %>% 
  filter(data=='test') %>% 
  select (-data,-Revenue.Grid)

head(rg_test)

names(rg_test)


###### splitting the data 80%-20% (we are performing the sepration on rg_train data)

### once the rg_train data set get seprated into 80% - 20% , 
###       again we are seprating the rg_train data set in to parts
###       (1) rg_train1       (2) rg_train2


set.seed(2)

s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
s


rg_train1 = rg_train[s , ]
rg_train1


rg_train2 = rg_train[-s , ]
rg_train2

 
#### now we are checking the diemesion

dim(rg_train)

dim(rg_train1)

dim(rg_train2)




library(car)


## creating the formula for whole model (we are using rg_train1 data)

## taking all the value as independent variable except Revenue.Grid and REF_NO

names(rg_train1)


### now we are removing the revenue.grid and ref_no from rg_train1 data set

form = 
     as.formula(paste0("Revenue.Grid  ~  "  ,  
                
               paste0(setdiff(names(rg_train1) , 
                       
                         c("Revenue.Grid" , "REF_NO")) , collapse = "+")))  
    
  


form <-
  as.formula(paste0('Revenue.Grid ~ ', paste0(setdiff(
    names(rg_train1), c('Revenue.Grid', 'REF_NO')
  ), collapse = ' + ')))





## For VIF calculation; we create a basic linear model
## lm = linear model
### we creating the linera model (lm)


for_vif=lm(form,data=rg_train1)
for_vif


### now calculating the vif

vif(for_vif)

##### now we are sorting the vif in decreasing order
sort(vif(for_vif) , decreasing = T)

## find the first value of vif, if you found it to be greater than 4 then use the below code

sort(vif(for_vif),decreasing = T)[1:5]



## k is the vif value initialized to a very high value

k = 100000000

## appended_dropped is a sentinal value which will change with k so that we don't run into infinite loop
appended_dropped = c('Revenue.Grid')

## loop will run untill all the values have vif lower than 4
while(k > 4){
  for_vif=lm(form,data=rg_train1) ## first a linear model for understanding the linear combination
  k <- sort(vif(for_vif),decreasing = T)[1] ## get the value of vif for highest value
  if (k <= 4){
    break
  }
  var_dropped <- names(k) ## get the name of the variable which has highest value
  print(k)
  appended_dropped <- c(var_dropped, appended_dropped) ## update the sentinal value with the new variable which needs to be dropped
  
  form <-
    as.formula(paste0('Revenue.Grid ~ ', paste0(setdiff(
      names(rg_train1), c('Revenue.Grid', 'REF_NO', appended_dropped)
    ), collapse = ' + '))) ## update the formula everytime
}



## Always remember VIF is an iterative process for variable removal. Never ever use it at once
## VIF doesn't depend on dependent variable, Even if you change the dependent variable the information will remain the same 


str(rg)


## chanign the dependent variable to factor

rg_train1$Revenue.Grid

rg_train1$Revenue.Grid = as.factor(rg_train1$Revenue.Grid)

table(rg_train1$Revenue.Grid)

str(rg_train1)


nchar(form)



## creating the logistic model by using the glm function


fited=glm(form,data=rg_train1,family = "binomial")
fited


## run the stepwise function for calculating the AIC

fited=step(fit)

# this might take 5-6 minutes to finish 


formula(fited)



### syntax of glm = glm(form , data , family)

glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
      Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
      Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
      Home.Loan + Online.Purchase.Amount + occupation_Unknown + 
      home_status_RentPrivately + family_income_LT_25000GT_EQ_22500 + 
      family_income_LT_30000GT_EQ_27500 + family_income_LT_27500GT_EQ_25000 + 
      self_employed_partner_No + TVarea_ScottishTV + TVarea_Meridian , 
    data= rg_train1, family = 'binomial')




glm(Revenue.Grid ~ Average.Credit.Card.Transaction +  Balance.Transfer + 
      Term.Deposit +  Life.Insurance + Medical.Insurance + Average.A.C.Balance +
      Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond +
      Home.Loan +  occupation_Unknown + home_status_RentPrivately + 
      family_income_LT_25000GT_EQ_22500 +  family_income_LT_30000GT_EQ_27500 +
      family_income_LT_27500GT_EQ_25000 +  self_employed_partner_No + TVarea_ScottishTV +
      TVarea_Meridian 
    , data = rg_train1 , family = "binomial")




glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
      Term.Deposit +  Life.Insurance +  Medical.Insurance + Average.A.C.Balance  
       , data = rg_train1 , family = "binomial")




## glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + Balance.Transfer + 
##      Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + Personal.Loan , data=rg_train1, family='binomial')




## get all the variables required using the last step of stepwise



form <-
  as.formula("
    Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
      Term.Deposit +  Life.Insurance +  Medical.Insurance + Average.A.C.Balance "
  )
  
form

fited=glm(form,data=rg_train1,family='binomial')
fited


summary(fited)

## Run the logistic
## if you find any warning of 'fitted probabilites of 1/0' 
## It is due to complete/quasi separation
## you need to remove the variable which is behaving similar to your dependent variable
## there is no shortcut, but a basic relationship using table can be gathered to remove them
## There is another way to find out(mentioned below):
## alternatively, with the given set of variable run it by picking one variable at a time and adding it to the equation,and running the logistic regression
## so an example would be y ~ x1 in first step, then y ~ x1 + x2 in second step, then y ~ x1 + x2 + x3, if adding x3 results to 1/0 fitted probabilites then x3 
## is not right variable, hence you drop it.
## drop that variable. Carry the above approach untill you are finished with all the variables



### for saving the file

saveRDS(file='log_fit_class.RDS', fited)


### for opening the file

fited = readRDS(file='log_fit_class.RDS')



library(caTools)

## caTools to get the ROC:

## Run it to determine the ROC Plot

## Install caTools library


caTools::colAUC(predict(fited, rg_train1, type = 'response'), 
                rg_train1$Revenue.Grid, plotROC = TRUE)

caTools::colAUC(predict(fited, rg_train2, type = 'response'), 
                rg_train2$Revenue.Grid, plotROC = TRUE)




#### performance of score model on validation data

# install.packages('pROC')

library(pROC)


## scoring the test(validation) data
### scoring the test(validation) data by using the rg_train2
### cz we are using rg_train2 data for testing(validation)
### so we are scoring on rg_train2 data

val.score = predict(fited,newdata = rg_train2,type='response')
val.score


## scoring the train (using rg_train1 data)

train.score = predict(fited, newdata=rg_train1, type='response')
train.score


#comparing the auc for train and test

auc(roc(rg_train2$Revenue.Grid,val.score))

auc(roc(rg_train1$Revenue.Grid,train.score))


round(0.7405333 - 0.714892 , 2)

# so the tentative score performance of logistic regression is going to be around 0.95
# now lets build the model on entire training data

# code given below is result of multiple iterations
## final model for glm with given set of variables,
## the final variables are result of step wise
## bivariate analysis of data(relationship b/w dependent variable an indepndent variable)

# now if we needed to submit probability scores for the test data we can do at this point

##final score to be submitted
test.prob.score= predict(fited,newdata = rg_test,type='response')
write.csv(test.prob.score,"proper_submission_file_name.csv",row.names = F)

# however if we need to submit hard classes, we'll need to determine cutoff score

## scoring the train2 and train1
rg_train1$score <- predict(fited, newdata=rg_train1,type = 'response')
rg_train2$score <- predict(fited, newdata=rg_train2, type = 'response')

head(rg_train1)

## dplyr::ntile
## Decile wise information for doing decile wise analysis
rg_train1$decile <- ntile(rg_train1$score, 10)


2 + 3 +
5 +8

x <- rg_train1 %>% 
  group_by(decile) %>% 
  summarise(counts = n(), event_sum = sum(as.numeric(as.character(Revenue.Grid))),'min'= min(score),'max' = max(score)) %>%
  arrange(desc(decile)) %>% 
  data.frame()
x

## This output to be pasted on excel sheet shared earlier, only two columns are required here 
## the counts and event_sum
## you need to reverse sort the data using decile and copy and paste these columns
## in the given excel sheet
write.csv(file='X.csv',x, row.names = F)

## For K-S we use below code
## determine the score using predict
train.score = predict(fited, newdata = rg_train1, type = 'response')
train.score



## get the real value using Revenue.Grid of rg_train1
real=rg_train1$Revenue.Grid

length(real)


## get 999 values of probabilities score for which you want to test TP, FP, FN and TN
cutoffs=seq(0.001,0.999,0.001)
length(cutoffs)

## Create a data frame with initialised garbage values
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

## iterating the loop for all the 999 probabilities
for(cutoff in cutoffs){
  ## determine the prediction for each cut off here
  predicted=as.numeric(train.score>cutoff)
  
  ## fill the value of TP, FP, FN and TN

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
  ## KS is the cutoff
  KS=(TP/P)-(FP/N)
  
  F5=(26*precision*recall)/((25*precision)+recall)
  ## F.1 score is maximum at 1 and min at 0
  ## A value of F.1 closer to 1 is good
  ## In case of low event rate model, F.1 closer to 1 is great
  ## F.1 score captures both precision and recall hence it is very useful in case of low event rate model
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  ## Binding the data
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

View(cutoff_data)

## removing the garbage column

cutoff_data=cutoff_data[-1,]

## getting the row where maximum value of KS is there
cutoff_data[cutoff_data$KS == max(cutoff_data$KS),]

View(cutoff_data)


## use the cut off to get the hard class value

rg_train1$predicted_Class <- (rg_train1$score > .13)+0

rg_train1$predicted_Class

table(rg_train1$predicted_Class)


library(caret)

## Draw the confusion matrix:
## Draw it for both train and test, compare them to see if they both behave similarly

confusionMatrix(factor(rg_train1$predicted_Class),
                rg_train1$Revenue.Grid,
                positive = '1')



# #### visualise how these measures move across cutoffs
# library(ggplot2)
# ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()
# 
# library(tidyr)
# 
# cutoff_long=cutoff_data %>% 
#   gather(Measure,Value,Sn:M)
# 
# ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()
# 
# 
# my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
# 
# my_cutoff

# now that we have our cutoff we can convert score to hard classes

## In case you want to submit hard class probability
test.predicted=as.numeric(test.prob.score>my_cutoff)
write.csv(test.predicted,"proper_submission_file_name.csv",row.names = F)

### Reduce


Reduce(`+`, iris[,1:4])

iris$Sepal.Length + iris$Sepal.Width + iris$Petal.Length + iris$Petal.Width

do.call(`+`, iris[,1:4])




df <- data.frame(x = 1:5, y = 2:6, z = 3:7, a1 = c(1:4,NA))
df


Reduce(`+`, df)
plus <- function(x, y){
  if(is.na(x) & is.na(y)){
    return(NA)
  } else if(is.na(x) & !is.na(y)){
    return(y)
  } else if(!is.na(x) & is.na(y)){
    return(x)
  } else{
    return(x + y)
  }
}

plus_vect <- Vectorize(plus)

Reduce('plus_vect', df)

df <- data.frame(pattern = c('a', 'b', 'c', 'd'), 
                 replacement = c('A', 'B', 'C', 'D'),
                 value = c('cat', 'bat', 'chaman', 'dumb'))

df

Vectorize(gsub)(df$pattern, df$replacement, df$value)
mapply(gsub, df$pattern, df$replacement, df$value)




######################################################







