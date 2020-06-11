

library(ggplot2)
## ----For Detailed comments on the codes , refer to reading material-----------------------------------------
## distribution of sample averages from a beta distribution
library(ggplot2)
set.seed(1)

d=data.frame(X=rbeta(200000,2,5))

p=ggplot(d,aes(x=X))

p+geom_histogram()

## population mean and variance 
mean(d$X) # 0.2860318
var(d$X) # 0.02556888

## claimed mean and variance of sample averages as per central limit theorem
# 0.2860318 = mean
# 0.0002556888 = variance

## ----
k=numeric(10000)

for (i in 1:10000){
  j=sample(1:200000,1000)
  
  k[i]=mean(d$X[j])
}

mean(k) #0.2857297
var(k) #0.0002536904

d1=data.frame(k)
p=ggplot(d1,aes(x=k))
p+geom_density()+xlab("Sample Averages")

## ----
## distribution of sample averages from a parabolic distribution
set.seed(1)
t=runif(20000)
set.seed(2000)
k=runif(20000)

X=ifelse(k>0.5,4+sqrt(1-1.332*t),4-sqrt(1-1.332*t))

d=data.frame(X=X)
p=ggplot(d,aes(x=X))
p+geom_histogram()

## ----
k=numeric(10000)
for (i in 1:10000){
  j=sample(1:20000,100)
  k[i]=mean(d$X[j],na.rm=T)
}
d1=data.frame(k)
p=ggplot(d1,aes(x=k))
p+geom_density()+xlab("Sample Averages")

## try this out with various other distiributions 
## check for mean and variances of sample averages
## try out different sample sizes


library(vcd)
library(ggplot2)
data("Arthritis")
p=ggplot(Arthritis,aes(x=Improved,y=Age)) 
p+geom_bar() 

## ------------------------------------------------------------------------
# setwd("/Users/lalitsachan/Dropbox//March onwards/CBAP with R/Data/")
wq=read.csv(".//Data//winequality-white.csv",sep=";")

## ------------------------------------------------------------------------

t.test(wq$fixed.acidity,mu = 6.10)
t.test(wq$fixed.acidity,mu = 6.840)

## ------------------------------------------------------------------------

t.test(wq$fixed.acidity,mu = 6.10,conf.level = 0.99)


## ------------------------------------------------------------------------
t.test(wq$fixed.acidity,mu = 6.10,alternative ="less" )

## ------------------------------------------------------------------------

t.test(wq$fixed.acidity,mu = 6.10,alternative ="greater" )


## ----
# please install package sas7bdat
library(sas7bdat)
d=read.sas7bdat(".//Data//hsb2.sas7bdat")

t.test(d$read,d$write,paired = TRUE)


## ------------------------------------------------------------------------
t.test(d$read,d$write,paired = TRUE,mu=0.50)

### unpaierd test
head(d)

read_f=d$read[d$female==1]
read_m=d$read[d$female==0]

var.test(read_f,read_m)

t.test(read_f,read_m,paired = FALSE,var.equal = TRUE)


## ----
wq$quality <- factor(wq$quality)
head(wq)


fit = aov(alcohol ~ quality ,data=wq)

summary(fit)


## ------------------------------------------------------------------------

pairwise.t.test(wq$alcohol, wq$quality, p.adj = "bonf")

## what is bonferroni adjustment

### catagorical variable tests
table(d$race)

prop.table(table(d$race))

## ------------------------------------------------------------------------
chisq.test(table(d$race),p=c(0.1,0.1,0.1,0.7))

head(d)

## ------------------------------------------------------------------------
chisq.test(table(d$ses,d$female))

## ------------------------------------------------------------------------
chisq.test(table(d$ses,d$race))

## ------------------------------------------------------------------------
table(d$race,d$ses)

## ------------------------------------------------------------------------
fisher.test(table(d$race,d$ses))

## ------------------------------------------------------------------------

## Dont ignore business context , and dont trust results of hypothesis tests blindly 

x=runif(400)
hist(x)
shapiro.test(x)

## --------------------------------------------------------------
y=rbeta(6000,2,8)
shapiro.test(y)
library(nortest)
ad.test(y)

## ------------------------------------------------------------------------

## ----
library(ggplot2)
df=data.frame(x,y)

ggplot(df,aes(x))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(df$x),sd=sd(df$x)),color="green")+
  ggtitle("Visual Normality Test for x ")

ggplot(df,aes(y))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(df$y),sd=sd(df$y)),color="green")+
  ggtitle("Visual Normality Test for y ")

## ------------------------------------------------------------------------
set.seed(1)
v1=rlnorm(20,0,0.4)
shapiro.test(v1)

## ------------------------------------------------------------------------
df=data.frame(v1)
ggplot(df,aes(x=v1))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(v1),sd=sd(v1)),color="green")+
  ggtitle("Visual Normality Test for v1 ")

## ------------------------------------------------------------------------
set.seed(1)
v2 = rt(60000,29)
ad.test(v2)

## ------------------------------------------------------------------------
df=data.frame(v2)
ggplot(df,aes(x=v2))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(v2),sd=sd(v2)),color="green")+
  ggtitle("Visual Normality Test for v2 ")

