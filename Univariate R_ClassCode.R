
## ------------------------------------------------------------------------
mean(mtcars$mpg)
median(mtcars$mpg)
min(mtcars$mpg)
max(mtcars$mpg)
var(mtcars$mpg)
sd(mtcars$mpg)
# IQR(mtcars$mpg)
range(mtcars$mpg)

# this is not an exhaustive list


## ------------------------------------------------------------------------
summary(mtcars$mpg)

summary(mtcars)
summary(df)

## ------------------------------------------------------------------------
# Accessing Summary function's results
s=summary(mtcars$mpg)
s
class(s)

names(s)


## ------------------------------------------------------------------------
s["1st Qu."]
s["3rd Qu."]
## ------------------------------------------------------------------------
s=summary(mtcars)
class(s)
names(s)

## ------------------------------------------------------------------------
#this outputs element in 1st row and 2 column in tables s, 
# which will be min value of variable "cyl"
s[1,2]

## ------------------------------------------------------------------------
# summary function from another library have better accessible results
library(psych)
describe(mtcars)

## ------------------------------------------------------------------------
s=describe(mtcars)
class(s)
rownames(s)
colnames(s)
names(s)
## ------------------------------------------------------------------------
s["gear","max"]

## ------------------------------------------------------------------------
s[c("gear","mpg","wt"),c("max","sd","mad")]

## ------------------------------------------------------------------------
boxplot(mtcars$mpg)

# find out by looking at the documentation or elsewhere , how the whiskers are made in R
hist(mtcars$mpg)

hist(mtcars$mpg,breaks=10)

x=rnorm(10000)
hist(x)
hist(x,breaks=10)

boxplot(x)

# in the boxplot , find out how are the horizontal lines are drawn at the extremese
# are these always max and min values 
# or there is a forumla taken into account to come up with these limits
# what are those points depcting here outside of these lines

x=rnorm(1000)
boxplot(x)

## ------------------------------------------------------------------------
library(vcd)
data(Arthritis)
View(Arthritis)
## ------------------------------------------------------------------------
head(Arthritis)
tail(Arthritis)
mean(Arthritis$Age)

tab1=table(Arthritis$Improved)
tab3=table(Arthritis$Age)
tab3
tab1

## ------------------------------------------------------------------------
t=prop.table(tab1)

t
## ------------------------------------------------------------------------
round(t,2)
round(t,1)


prop.table(tab1)*100
head(Arthritis)
## ------------------------------------------------------------------------

unique(mtcars$cyl)

sum(mtcars$cyl == 4)
sum(mtcars$cyl == 6)
sum(mtcars$cyl == 8)

table(cyl = mtcars$cyl, vs =mtcars$vs)

sum(mtcars$cyl == 4 & mtcars$vs == 1)

table(Arthritis$Improved,
      Arthritis$Treatment)

xtabs(~Improved+Treatment,data=Arthritis)

## ------------------------------------------------------------------------
tab2=xtabs(~Improved+Treatment,data=Arthritis)
tab2

x = xtabs(~ cyl + gear , data= mtcars)
x

margin.table(x)
margin.table(x, 1)
margin.table(x, 2)
## ------------------------------------------------------------------------
prop.table(tab2)

#Percentages total to 1 across values of Treatment
prop.table(tab2,1)

#Percentages total to 1 across values of Improved
prop.table(tab2,2)

sum(tab2)
## ------------------------------------------------------------------------
margin.table(tab2)
margin.table(tab2,1)
margin.table(tab2,2)
## ------------------------------------------------------------------------
addmargins(tab2)
addmargins(tab2,2)
addmargins(tab2,1)

## ------------------------------------------------------------------------
tab3=xtabs(~Treatment+Sex+Improved,data=Arthritis)
ftable(tab3)

x = xtabs(~ cyl + gear + vs , data= mtcars)

x = xtabs( ~ gear + vs + cyl, data= mtcars)
x

ftable(x)

sum(mtcars$cyl ==  8 & mtcars$gear == 3 & mtcars$vs == 0)

## ------------------------------------------------------------------------
margin.table(tab3,1)
margin.table(tab3,c(1,3))
margin.table(tab3,c(2,3))
## ------------------------------------------------------------------------
ftable(prop.table(tab3))
  
ftable(prop.table(tab3,c(1,2)))

## ------------------------------------------------------------------------
ftable(addmargins(prop.table(tab3,c(1,2)),3))
#It is believed that mileage of a car is related to whether it has automatic 
# transmission or not, examine this using mtcars data.

## class case solution : by the students

mycars=mtcars[,c("mpg","am")]
mycars
mean(mycars$mpg)

mycars.am0=mycars[mycars$am==0,]
mycars.am1=mycars[mycars$am==1,]


mean(mycars.am0$mpg)
mean(mycars.am1$mpg)

#Using other information in the data analyse how , 
#engine type[vs]   affects car mileage 

myarth=Arthritis[Arthritis$Treatment=="Treated",]
myarth
mean(myarth[myarth$Improved=="None",]$Age)

mean(myarth[myarth$Improved=="Marked",]$Age)

mean(myarth[myarth$Improved=="Some",]$Age)



