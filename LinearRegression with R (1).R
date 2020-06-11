
ld_train=read.csv("./Data/loan_data_train.csv",stringsAsFactors = F)


# samp <- sample(1:nrow(ld_train), nrow(ld_train)*.70)
# ld_tt <- ld_train[samp,]
# ld_te <- ld_train[-samp,]
 
# dim(ld_train)
# dim(ld_tt)
# dim(ld_te)

# View(head(ld_train))
# dim(ld_train)

ld_test= read.csv("./Data/loan_data_test.csv",stringsAsFactors = F)


names(ld_train)
names(ld_test)

# setdiff(c(1,2,3,4), c(3,3,4,5))
setdiff(names(ld_train), names(ld_test))

dim(ld_train)
dim(ld_test)
## The Unknown Interest Rate is initialised to NA
## This is what we are interested in Predicting, This is why we 
## are doing this excercise, We got to know the Interest Rate given all the independent variables
ld_test$Interest.Rate=NA

ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)
dim(ld_all)

## REason to drop Amount.Funded.By.Investors,
## You can ignore it also, we can drop it while calculating VIF
cor.test(as.numeric(ld_all$Amount.Funded.By.Investors), as.numeric(ld_all$Amount.Requested))
plot(ld_all$Amount.Funded.By.Investors, ld_all$Amount.Requested)

# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL

head(ld_all)

library(dplyr)

glimpse(ld_all)

## REmoving the percent sign and converting chars to nums

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



fmiss <-  function(x) sum(is.na(x))
lapply(ld_all , fmiss)

table(ld_all$Loan.Length)

glimpse(ld_all)

## Making a dummy variable using Loan.Length == '36 months'
ld_all=ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)

round(tapply(ld_all$Interest.Rate,ld_all$Loan.Purpose,mean,na.rm=T))

## Trying to to see with what loan purpose the average of interest rate in training data
## club the loan purpose wherever similar average interest rate 
ld_all %>% 
  group_by(Loan.Purpose) %>% 
  summarise('means'= mean(Interest.Rate, na.rm = T)) %>% 
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

CreateDummies(mtcars, 'cyl' )

# 
# ld_all %>% 
#   mutate(State_CA = (State == 'CA')+0,
#          State_NY = (State == 'NY')+0,
#          State_TX = (State == 'TX')+0,
#          State_FL = (State == 'FL')+0)

table(ld_all$State)

round(prop.table(sort(table(ld_all$State)))*100)


glimpse(ld_all)

unique(ld_all$State)

ld_all=CreateDummies(ld_all ,"State",100)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)

head(ld_all)
# model.matrix(~ ld_all$FICO.Range)

unique(ld_all$FICO.Range)
library(tidyr)

f1 = as.numeric(gsub('([0-9]+)-([0-9]+)', '\\1',ld_all$FICO.Range))
f2 = as.numeric(gsub('([0-9]+)-([0-9]+)', '\\2',ld_all$FICO.Range))

(f1+f2)/2

df <-
  data.frame(do.call('rbind', strsplit(ld_all$FICO.Range, split = "-")))
X1 <- as.numeric(df$X1)
X2 <- as.numeric(df$X2)

(X1+X2)/2

## taking the average of f1 and f2 by splitting the f1-f2
ld_all=ld_all %>% 
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)

ld_all=CreateDummies(ld_all,"Employment.Length",100)

## NA values
## understaning how many missing values
sapply(ld_all,function(x) sum(is.na(x)))

ld_all=ld_all[!(is.na(ld_all$ID)),]


## replaceing the missing values with the mean you can 
## use a function instead like one below 
for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[,col],na.rm=T)
  }
  
}

## Use the function in case you are unable to understand the for loop

vect <- data.frame(x = c(1,2,3,NA, 10, 11, 12,NA))

vect$x[is.na(vect$x)] <- mean(vect$x, na.rm=T)

missing_Fn_replace <- function(x){
  ## get the x and find all the missing records,
  ## replace the missing records with mean
  x[is.na(x)] <- mean(x, na.rm=TRUE)
  ## finally return the entire x , which has replaced mean as well
  return(x)
}

## get all the numeric items
numeric <- sapply(ld_all, is.numeric)

# ld_all_old <- ld_all
## replace numeric items by replacing the mean using sapply or lapply
ld_all[,numeric] <- sapply(ld_all[,numeric], missing_Fn_replace)

View(head(ld_all))

access(vect)
missing_Fn_replace(vect)
## separate train and test
ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)

##
## Make a train set from ld_train for model validation
set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]

dim(ld_train)
dim(ld_train1)
dim(ld_train2)

names(ld_train)

"Interest.Rate"

as.formula(paste0("Interest.Rate ~ ",
                  paste0(setdiff(
                    names(ld_train), c('ID', "Interest.Rate")
                  ), collapse = " + ")))

## This function makes formulas using given the dependent variables
## and removing the non required independent variables

formula_function <- function(x, non_required_ivs, dv) {
  vector_ivs <- setdiff(names(x), c(non_required_ivs, dv))
  ## finding the vecotor for which we want to make formula(only containing independent variables)
  ## as.formula to convert the string to a formula containing plus and tilde sign
  ## first paste0 will concatenate the 'dv' to independent variables separated by plus sign
  ## the last paste0 will concatenate all the independent variables separated by plus
  return(as.formula(paste0(dv , ' ~ ', paste0(vector_ivs, collapse = ' + '))))
  
}

formula <- formula_function(ld_train1, c('ID', 'Interest.Rate'),'Interest.Rate')
# 
# fit=lm(formula,data=ld_train1)
# 
# library(car)
# ## we'll take vif cutoff as 5
# 
# sort(vif(fit),decreasing = T)
# 
# formula <-
#   formula_function(ld_train1, c('ID', 'Interest.Rate', 'lp_14'), 'Interest.Rate')
# 
# fit=lm(formula,data=ld_train1)
# sort(vif(fit),decreasing = T)
### x is any garbage value but should be greater than 2 as it has to enter in the loop


## Ojbect_floating is a sentinel value which gets updated, intialise with the dependent variable

formula <- formula_function(ld_train1, c('ID', 'Interest.Rate'),'Interest.Rate')
x <-  1000
object_floating <- 'Interest.Rate'



while(x > 4){
  fit=lm(formula,data=ld_train1) ## Every loop will create a new fit
  sorted <- sort(vif(fit),decreasing = T) ## sort the vif value of fit , pick the first one
  x <- sorted[1] ## pick the first one
  print(names(x)) ## print on console 
  object_floating <-c(names(x), object_floating) ## update the object_floating whenever there is any variable having vif greater than 2
  formula <- formula_function(ld_train1,c('ID', 'Interest.Rate', object_floating),'Interest.Rate') 
  ## update the formula by removing the variables which has vif greater than 2
}


# p-value take the cutoff .05
fit <- lm(formula, data= ld_train1)

summary(fit)

fit=step(fit)

saveRDS(file='fit.RDS', fit)

## AIC score 

summary(fit)

formula(fit)

Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
  Inquiries.in.the.Last.6.Months + ll_36 + lp_13 + State_FL + 
  State_TX + Home.Ownership_RENT + fico + Employment.Length_5years


fit=lm(Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
         Inquiries.in.the.Last.6.Months + ll_36 + lp_13 + State_FL + 
         State_TX + Home.Ownership_RENT + fico + Employment.Length_5years,
       data=ld_train1)

saveRDS(file='fit_linear_reg.RDS', fit)

summary(fit)
fit$coefficients
fit$residuals
###

val.pred=predict(fit,newdata=ld_train2)

errors=ld_train2$Interest.Rate-val.pred

errors**2 %>% mean() %>% sqrt()

val.pred1=predict(fit,newdata=ld_train1)

errors1=ld_train1$Interest.Rate-val.pred1

errors1**2 %>% mean() %>% sqrt()



### model for predcition on the entire data

# fit.final=fit=lm(Interest.Rate ~ .-ID,
#                  data=ld_train)
# 
# fit.final=step(fit.final)
# 
# summary(fit.final)
# 
test.pred=predict(fit,newdata=ld_test)
# 
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


