
library(tidyverse)

library(ggplot2)



ld_train=read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\loan_data_train.csv",stringsAsFactors = F)

ld_train


### for showing what type of data is present (for ex :- char or int)

str(ld_train)

### for showing the how many rows and coloumns are present 

dim(ld_train)

# samp <- sample(1:nrow(ld_train), nrow(ld_train)*.70)
# ld_tt <- ld_train[samp,]
# ld_te <- ld_train[-samp,]
 
# dim(ld_train)
# dim(ld_tt)
# dim(ld_te)

# View(head(ld_train))
# dim(ld_train)


ld_test= read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\loan_data_test.csv",stringsAsFactors = F)

ld_test



### for showing what type of data is present (for ex :- char or int)

str(ld_test)


### for showing the names of variables and how many variables are present 

names(ld_train)

names(ld_test)

variable.names(ld_test)



# setdiff(c(1,2,3,4), c(3,3,4,5))

setdiff(names(ld_train), names(ld_test))


### for showing the how many rows and coloumns are present 

dim(ld_test)
dim(ld_train)



#### adding the one more coloumn Intrest.Rate=NA cz this is the dependant variable


ld_test$Interest.Rate=NA

variable.names(ld_test)


### for combining both the data set we using rbind function

ld_all = rbind(ld_train , ld_test)
ld_all

head(ld_all)

dim(ld_all)


## The Unknown Interest Rate is initialised to NA
## This is what we are interested in Predicting, This is why we 
## are doing this excercise, We got to know the Interest Rate given all the independent variables

ld_test$Interest.Rate=NA

#### adding the one more coloumn in the train data set

ld_train$data='train'


### for adding the one more coloumn in the test data set

ld_test$data='test'

### we are doing rbind again cz we added new coloumns in the previous step

ld_all=rbind(ld_train , ld_test)

head(ld_all)

dim(ld_all)



## REason to drop Amount.Funded.By.Investors,

## You can ignore it also, we can drop it while calculating VIF

## Correlation test is used to evaluate the association between two or more variables.

## If the p-value is < 5%, then the correlation between x and y is significant.

## Correlation coefficient can be computed using the functions cor() or cor.test()

## check the test result

cor.test(as.numeric(ld_all$Amount.Funded.By.Investors), as.numeric(ld_all$Amount.Requested))

plot(ld_all$Amount.Funded.By.Investors, ld_all$Amount.Requested)


# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL

head(ld_all)


library(dplyr)

glimpse(ld_all)



## REmoving the percent sign from the Intrest.Rate and converting chars to nums
## converting the Interest.Rate , Debt.To.Income.Ratio , Open.CREDIT.Lines , 
##          Amount.Requested , Revolving.CREDIT.Balance into nums
## we using mutate function here for creating new coloumns as their same name


ld_all=ld_all %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
         )


head(ld_all)


## seeing how many missing values are present in these variables

sapply(ld_all[,c('Interest.Rate', 'Debt.To.Income.Ratio',
         'Open.CREDIT.Lines', 'Amount.Requested',
         'Revolving.CREDIT.Balance')], function(x)sum(is.na(x)))


### for showing how many missing values are present in entire data set

fmiss =  function(x) sum(is.na(x))
lapply(ld_all , fmiss)

## showing the loan length

table(ld_all$Loan.Length)

glimpse(ld_all)


## Making a dummy variable using Loan.Length == '36 months'

## see the result and look at ll_36 coloumn for 60 months = 0 and 36 months = 1


ld_all=ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)

head(ld_all)


### for calculating the intrest rate according to loan purpose


round(tapply(ld_all$Interest.Rate,ld_all$Loan.Purpose,mean,na.rm=T))


tapply(ld_all$Interest.Rate , ld_all$Loan.Purpose , mean ,na.rm=T)


#### for round figure we use

round(tapply(ld_all$Interest.Rate , ld_all$Loan.Purpose , mean , na.rm=T))


#### for calculating the intrest rate according to the monthaly income

round(tapply(ld_all$Interest.Rate , ld_all$Monthly.Income , mean , na.rm = T))



## Trying to to see with what loan purpose the average of interest rate in training data

## club the loan purpose wherever similar average interest rate 

### use group by for loan purpose and use arrange for descending order for mens

head(ld_all)

ld_all %>% 
  group_by(Loan.Purpose) %>% 
  summarise('means'= mean(Interest.Rate, na.rm = T)) %>% 
  arrange(desc(means))


ld_all %>% 
  group_by(Loan.Purpose) %>% 
  summarise("means" = mean(Interest.Rate , na.rm = T)) %>% 
  arrange(desc(means))


mean(ld_all$Interest.Rate, na.rm = T)
# 10
# 11
# 12
# 13
# 14

ld_all=ld_all %>% 
  mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
         lp_11=as.numeric(Loan.Purpose %in% c("major_purchase","medical","car")),
         lp_12=as.numeric(Loan.Purpose %in% c("vacation","wedding","home_improvement")),
         lp_13=as.numeric(Loan.Purpose %in% c("other","small_business","credit_card")),
         lp_14=as.numeric(Loan.Purpose %in% c("debt_consolidation","house","moving"))) %>% 
  select(-Loan.Purpose)

head(ld_all)



## Function to create the dummies, 

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
    
    data[,name]=as.numeric(data[,var]==cat) ## finally converting everyting to numeric
  }
  
  data[,var]=NULL
  return(data)
}
# 
# ld_all %>% 
#   mutate(State_CA = (State == 'CA')+0,
#          State_NY = (State == 'NY')+0,
#          State_TX = (State == 'TX')+0,
#          State_FL = (State == 'FL')+0)



### for showing the la_all$state data with unique values

table(ld_all$State)


### sorting this table

sort(table(ld_all$State))


### calculating the prop.table

prop.table(sort(table(ld_all$State)))


####### for making the round values

round(prop.table(sort(table(ld_all$State)))*100)



round(prop.table(sort(table(ld_all$State)))*100)


### for showing the unique values #####

unique(ld_all$State)

### for showing the variables with the type

glimpse(ld_all)



### creating the dummies by using the function [line no = 248  ] for state and home.ownership

ld_all=CreateDummies(ld_all ,"State",100)


ld_all=CreateDummies(ld_all,"Home.Ownership",100)

head(ld_all)


library(tidyverse)






## for showing the unique values from fico.range

unique(ld_all$FICO.Range)


library(tidyr)

f1 = as.numeric(gsub('([0-9]+)-([0-9]+)', '\\1',ld_all$FICO.Range))
f1

f2 = as.numeric(gsub('([0-9]+)-([0-9]+)', '\\2',ld_all$FICO.Range))
f2


(f1+f2)/2

head(ld_all)


##### this method also we can use 

df <- data.frame(do.call('rbind', strsplit(ld_all$FICO.Range, split = "-")))


X1 <- as.numeric(df$X1)
X1

X2 <- as.numeric(df$X2)
X2

(X1+X2)/2




## taking the average of f1 and f2 by splitting the f1-f2

ld_all=ld_all %>% 
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)



###### creating the dummy values for employment.length

ld_all=CreateDummies(ld_all,"Employment.Length",100)



## NA values
## understaning how many missing values

sapply(ld_all,function(x) sum(is.na(x)))

### for removing the NA values from ID

ld_all=ld_all[!(is.na(ld_all$ID)),]

ld_all=ld_all[!(is.na(ld_all$ID)),]

ld_all=ld_all[!(is.na(ld_all$ID)),]


## Use the function in case you are unable to understand the for loop

vect <- c(1,2,3,NA, 10, 11, 12,NA)
missing_Fn_replace <- function(x){
  ## get the x and find all the missing records,
  ## replace the missing records with mean
  x[is.na(x)] <- mean(x, na.rm=TRUE)
  ## finally return the entire x , which has replaced mean as well
  return(x)
}

missing_Fn_replace(c(1,2,3,NA, 10, 11, 12,NA))


## get all the numeric items
## use sapply cz it only works on the coloumn
## is.numeric is the function for showing which coloumns are numeric

numeric = sapply(ld_all, is.numeric)
numeric


# ld_all_old <- ld_all



## replace numeric items by replacing the mean using sapply or lapply
## we are replacing the numeric items by replacing the mean by using sapply or laapply

ld_all[,numeric] <- sapply(ld_all[,numeric], missing_Fn_replace)


ld_all[,numeric] = sapply(ld_all[,numeric], missing_Fn_replace)


head(ld_all)

access(vect)


#### from line no 398

missing_Fn_replace(vect)



## separate train data set by using filter function and removing the data coloumn

ld_train=ld_all %>% 
  filter(data=='train') %>% 
  select(-data)



### seprate test data set by using filter function and removing the data coloumn ,
###             and intrest.rate coloumn


ld_test=ld_all %>% 
  filter(data=='test') %>% 
  select(-data,-Interest.Rate)




#### now we are using the train data set from ld_train for model

## Make a train set from ld_train for model validation


set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
s

ld_train1=ld_train[s,]
ld_train1

ld_train2=ld_train[-s,]
ld_train2


dim(ld_train1)

"Interest.Rate"

as.formula(paste0("Interest.Rate ~ ",
                  paste0(setdiff(
                    names(ld_train), c('ID', "Interest.Rate")
                  ), collapse = " + ")))

## This function makes formulas using given  dependent variables

## and removing the non required independent variables

formula_function <- function(x, non_required_ivs, dv) {
  vector_ivs <- setdiff(names(x), c(non_required_ivs, dv))
  ## finding the vecotor for which we want to make formula(only containing independent variables)
  ## as.formula to convert the string to a formula containing plus and tilde sign
  ## first paste0 will concatenate the 'dv' to independent variables separated by plus sign
  ## the last paste0 will concatenate all the independent variables separated by plus
  as.formula(paste0(dv , ' ~ ', paste0(vector_ivs, collapse = ' + ')))
  
}

formula = formula_function(ld_train1,c('ID'),'Interest.Rate')

formula




### what is fit and why we are using..??

#### A well-fitting regression model results in predicted values close to the observed data values. 
#### ... Three statistics are used in Ordinary Least Squares (OLS) regression to evaluate model fit: 
####  R-squared, the overall F-test, and the Root Mean Square Error (RMSE)


### The Model Fit table provides fit statistics calculated across all of the models. 
### It provides a concise summary of how well the models, with reestimated parameters, fit the data. 
### For each statistic, the table provides the mean, standard error (SE), minimum, and maximum value across all models.

fit=lm(formula,data=ld_train1)

fit

summary(fit)



### what is VIF..?

####  Variance inflation fVariance inflation factor (VIF) is a measure of the amount of multicollinearity 
###                              in a set of multiple regression variables. 
### ... This ratio is calculated for each independent variable. 
### A high VIF indicates that the associated independent variable is highly collinear with the other variables in the model



### what is high VIF...?

### A VIF can be computed for each predictor in a predictive model. 
### A value of 1 means that the predictor is not correlated with other variables. 
### The higher the value, the greater the correlation of the variable with other variables. ... 
### If one variable has a high VIF it means that other variables must also have high VIFs.


library(car)

# we'll take vif cutoff as 5


## using vif function for fit

vif(fit)


## now use sort function 

sort(vif(fit))


##### using sort function with decreasing for descending value

sort(vif(fit),decreasing = T)


### using formula_function for removing the non-required variables

formula <- formula_function(ld_train1,c('ID', 'Interest.Rate', 'lp_14'),'Interest.Rate')

fit=lm(formula,data=ld_train1)
fit

summary(fit)


### x is any garbage value but should be greater than 2 as it has to enter in the loop
## Ojbect_floating is a sentinel value which gets updated, intialise with the dependent variable
### object_floating is always works with dependant vairable


x =  1000

object_floating = 'Interest.Rate'




fit=lm(formula,data=ld_train1)
fit

summary(fit)



while(x > 2){
  fit=lm(formula,data=ld_train1) ## Every loop will create a new fit
  sorted <- sort(vif(fit),decreasing = T) ## sort the vif value of fit , pick the first one
  x <- sorted[1] ## pick the first one
  print(names(x)) ## print on console 
  object_floating <-c(names(x), object_floating) ## update the object_floating whenever there is any variable having vif greater than 2
  formula <- formula_function(ld_train1,c('ID', 'Interest.Rate', object_floating),'Interest.Rate') 
  ## update the formula by removing the variables which has vif greater than 2
}



# p-value take the cutoff .05

### using fit for fitting the data cz new changes that we did

fit = lm(formula, data= ld_train1)
fit

summary(fit)

### using formula for showing all the detailed of the variables that present

formula

###### for calculating the AIC we have to use the fit=step(fit)

fit=step(fit)

##### for saving the file

saveRDS(file='fit.RDS', fit)


## AIC score 

summary(fit)

formula(fit)

fit=lm(Interest.Rate ~ Monthly.Income + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + 
         ll_36 + lp_11 + State_FL + State_TX + Home.Ownership_RENT + 
         fico ,
       data=ld_train1)


fit

summary(fit)


fit$coefficients


fit$residuals


#######


val.pred=predict(fit,newdata=ld_train1)

errors=ld_train1$Interest.Rate-val.pred

errors**2 %>% mean() %>% sqrt()


val.pred1=predict(fit,newdata=ld_train2)

errors1=ld_train2$Interest.Rate-val.pred1

errors1**2 %>% mean() %>% sqrt()




### model for predcition on the entire data

fit.final=fit=lm(Interest.Rate ~ .-ID,
                 data=ld_train)

fit.final=step(fit.final)

summary(fit.final)

test.pred=predict(fit.final,newdata=ld_test)

write.csv(test.pred,"submision1.csv",row.names = F)

### 

plot(fit.final,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

#### 

output=summary(fit.final)
names(output)

output$coefficients[,4]


##
d1=data.frame(x=1:4,y=c("a","a","b","b"))
d2=data.frame(x=10:13,y=c("a","a","b","c"))

d3=rbind(d1,d2)
d3


