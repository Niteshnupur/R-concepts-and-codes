
#this is how to write comments
# hash to comment out the lines, no multiple line commenting, however
#you can select multiple lines and use the shortcut cut to comment out
# shortcut for commenting : ctrl + shift + c
## ------------------------------------------------------------------------
## R is case sensitive

x
x=5
x <- 5
x
X="something"
X


## ------------------------------------------------------------------------
## naming rules
1.more=2.34
one.more=2.34
onemoreobject=2.34
one.more.object=2.34

# you can use dots(.) and underscore ( _ )
# you cannot have other special characters in the name
# you can use numbers also in object , BUT not at the begining

## ------------------------------------------------------------------------

ls()
#objects can be reassigned on the fly

x="Hadley Wickham"

# removing objects from environment

rm(one.more,one.more.object,onemoreobject)
ls()

## ------------------------------------------------------------------------
## logical values true and false, that has to be in caps
x=FALSE
x

x=TRUE
x

## ------------------------------------------------------------------------
x=F
x

# F = 20
FALSE = 20
x =F
x

x = TRUE
x = x+0

## shortcut for clearing console : ctrl+l
## ------------------------------------------------------------------------
# now we will be looking at class, type of the object
x="bunny"
y=F
z=4.5
class(x)
class(y)
class(z)
typeof(x)
typeof(y)
typeof(z)
## ------------------------------------------------------------------------
# anything within quotes is character for R

v1="23.45"

class(v1)
# you cannot do numeric operation on this


x
v1+2

## ------------------------------------------------------------------------
# change the type, given it is possible to convert the number

`+`(2, 5)
2 + 5

v2=as.numeric(v1)

#function will not change the input, only the output gets changed
class(v2)
class(v1)

v2+2
x1 <- 'one'
y <- as.numeric(x1)

typeof(y)
rm(y)
y <- NA
typeof(y)
## ------------------------------------------------------------------------

v1="King"
class(v1)

#hit tab for autocompletion

v2=as.numeric(v1)
v2
class(v2)

## ------------------------------------------------------------------------
# opening documentation for functions in R

?sum


## ------------------------------------------------------------------------
?smu

## ------------------------------------------------------------------------
??smu

## ------------------------------------------------------------------------
# Numeric Operations

x=2
y=8

x+y
x-y
x*y
x/y

## ------------------------------------------------------------------------
x^y
x**y

## ------------------------------------------------------------------------
z=(x+y-(x/y))**(x/10)

## ------------------------------------------------------------------------
#mathematical functions

tan(10)

log(2^14,10)
log(2^14)
log(2^14,2)


## ------------------------------------------------------------------------
## specific functions in R for string operations

x="Sachin"
y="Tendulkar"
z="Cricket"


paste(x,y,z,sep="+")

?paste
mean(c(1,2,3))
paste0('one', 'two', 'three')
paste('one', 'two', 'three')
?mean
## ------------------------------------------------------------------------
name=paste(x,y)

#sep controls how to concatenate
profile=paste(name,z,sep=":")
profile

## ------------------------------------------------------------------------
paste(x,y,z,sep="$")
paste(x,y,z,sep="%2")

#paste0 : by default separator is empty string
paste0(x,y,z)

## ------------------------------------------------------------------------

address="1612-Lone Tower-Mumbai"
newAd=sub("-","/",address)
newAd1 <- gsub('-', '/', address)

## ------------------------------------------------------------------------
newAd

## ------------------------------------------------------------------------
newAd=sub("-","/",newAd)
newAd

## ------------------------------------------------------------------------
newAd1=gsub("-","/",address)
newAd1
address
newAd1=gsub("e","skjdasnd",address)
newAd1

nchar('dafdafad1344')

## ------------------------------------------------------------------------
ip="192.168.23.134:219"            
abc=substr(ip,1,3)
abc


options(scipen = 999) ## Removes the scientific penalty
## ------------------------------------------------------------------------
x="Sa chin-$. ?/"
nchar(x)
x <- 1000000000
nchar(x)
x
## ------------------------------------------------------------------------
# Logical Operations, Writing Conditions

x=7
y=9

x>y
x<y
x==y
x!=y
x>=y
x<=y

## ------------------------------------------------------------------------
z=x>y
z

## ------------------------------------------------------------------------
x=20
x>=1 & x<=19

y="SAchin"
y=="Sachin" | y=="SACHIN"
y=="SAchin" | y=="SACHIN"

## ------------------------------------------------------------------------
# writing a vector, holding multiple values

x=c(2,4,89,-10,5,6) # vector
x
x=c(2,3,4,"a", FALSE)
x


# if one of the element is character, the entire vector is of character type
x=c(2,4,89,-10,67,73)
## ------------------------------------------------------------------------
class(x)
# vectors cannot contain mixed values, like both numeric and charater
## ------------------------------------------------------------------------

# Accessing vector elements
length(x)
x[3]
x[-3]
x[c(1,3)]

# access multiple values
#x[1,2,5] : this does not work

## ------------------------------------------------------------------------
p=c(1,2,5)


x[p]

x[c(1,2,5)]

x[-p]


## ------------------------------------------------------------------------
x[c(3,4,2,2,10,3)]

## ------------------------------------------------------------------------
x[c(2,3,8,10,5,2,2,4,9)]
#4 89 NA NA 67 4 4 -10 NA

# they need not be in order
# need not be unique
# need not be less than the length of vector

## ------------------------------------------------------------------------
x
x[-2]

## ------------------------------------------------------------------------
x[-c(2,3,6)]

x[c(2,3,-1)]


## ------------------------------------------------------------------------
# conditional access of vector

x
x > 4

L=x>4
L

# compare with each element of x, wherever it is true,it gives the value
## ------------------------------------------------------------------------
x[L]

## ------------------------------------------------------------------------
x[x>4]
# within square brackets you can pass condition also

## ------------------------------------------------------------------------
x
L
!L
x[!L]
x[!(x>4)]
# reverse the condition

y=c("a","a","b","c","d","b","a")

y == 'a'

x=c(34,56,12,-90,34,4,8)

# conditions can be based on another vector

x[y=='a']
x[y == 'a']


y[x%%3==0]

## ------------------------------------------------------------------------
#Creating vectors with handy functions and operators

# a:b creates a sequence from a to b intremental or decremental

x=2:10
x

4:-1
-5:5

2.3:7.9

## ------------------------------------------------------------------------

x=seq(1,5,by=0.3)
# 5 may or may not be included
## ------------------------------------------------------------------------
x

x=seq(5,1,by=-0.3)
x

x=seq(1,5,by=-0.3)
x

## ------------------------------------------------------------------------
x=seq(1,10,length=21)
x
#creates AP
# start and stop are included when length option is used
## ------------------------------------------------------------------------
x=1:5  ## 1,2,3,4,5
y=seq(2,3,length=5) # 2.00 2.25 2.50 2.75 3.00

z=c(x,y,2,3,6)
length(z)


## ------------------------------------------------------------------------
## what to repeat and how many times
rep(2,10)
rep("a",5)


## ------------------------------------------------------------------------
rep(c(1,5,6),4)


## ------------------------------------------------------------------------
rep(c("a","b","c","b"),4)


#vector repeated 4 times


rep(c("a","c","b"),4)

rep(c("a","c","b"),each=4)

# if each=4 it repeats individual elements 4 times


## ------------------------------------------------------------------------
# vector operations

x=1:5
y=seq(3,13,length=9)
x+y
x*y
x-y
x/y

x
# operation is done element by element(on each element) on
#vectors and the result is also vector
## ------------------------------------------------------------------------
log(x)

2**x

## ------------------------------------------------------------------------
paste0(1,"a")

x=1:10

y=rep("a",10)

z = paste0(x,y)


#concatenation will happen on individual elements
## ------------------------------------------------------------------------
# collapse option in paste function
z=paste(y,x,sep="")
m <- paste(z,collapse="+")
# collapses into a single string using "+" as separator
## Replacing a plus sign can be tricky as it is a special character
## Explained all the special  characetrs in class in detail
## *, +, \, ., (), --> explained already, we can discuss if there is any confusion
## ?, ! {} and [] are to be explained in the next class or so
gsub('\\+','-', m)

library(stringr)

str_extract('abc25', '\\d+') ## will only extract digits
str_extract('abc25', '[a-zA-Z]+') ## will only extract alphabets
str_extract('abc-25', '\\w+') ## will only extract abc as it encounters a dash hence regex will stop, you may choose to use str_extract_all to get all the matches(don't worry for now)
str_extract('abc-25', '.+') ## will extract the entire string
str_extract('abc25', '\\d*') 
## will extract '' only, digits will not get extracted, 
# because * means zero or more so when regex will start 
# it will first encounter nothingness which it will return, 
# if you want to return digits also use: str_extract_all (we will discuss this function once we know about lists)
# You should not worry about that for now.


str_view_all('abc25', '\\d*')

## Please don't worry about more complicated situations for now,
## Understanding the basics is first and foremost priority
## we shall do complicated situations more in coming classes while we 
## do data preparation

## Extracting the 'double' repeats in a words:
x <- c('coconut', 'banana', 'apple', 'grapes')

grepl('apple', x)
grepl( '(\\w)(\\w)\\1\\2', x, perl=T)
### line we shall start on 9th Nov


## ------------------------------------------------------------------------


x=1:10

y=rep("a",10)

z=paste(y,x,sep="")

set.seed(1)
f=round(runif(10),2)
f
#generate 10 numbers between 0 to 1 with two decimal places

paste(f,z,sep="*",collapse="+")
f
z
## ------------------------------------------------------------------------

# vector operations when lengths do not match
x=1:11
y=c(1,2,3)



## ------------------------------------------------------------------------
#you'll get a result but with a warning for length mismatch
# However you will not get this warning if smaller vector's length is
# a multiple of larger vector

x+y

## ------------------------------------------------------------------------
x=c("a","b")
y=1:10
z=c("c","d")
paste0(x,y,z)




## ------------------------------------------------------------------------
# Special utility functions for vectors

x=c("k","j","$","1","f")
y=letters
x
y
match(x,y)

?letters

#####
x=c("k","j","$","1","F")
a=letters
b=LETTERS
z=c(a,b)

match(x,z)
#it is case sensitive
#####

## ------------------------------------------------------------------------
x
y
x %in% y
x[x %in% y]

## ------------------------------------------------------------------------

100 %% 32
30 %% 12
30 %% 15


## ------------------------------------------------------------------------
x=56:78
x
which(x%%5==0)


# gives the indices where the condition is true
## ------------------------------------------------------------------------
##exercise

x=seq(40,1)#from 1 to 100, 40 numbers

x[which(x%%2!=0)]

## ------------------------------------------------------------------------
x=c(3,4,5,-10,1,2,0)
sort(x, decreasing = TRUE)
y=c("art","Ant","Bat")
sort(y)#sorts in dictionary order

## ------------------------------------------------------------------------
sort(x,decreasing = T)
sort(y,decreasing = T)


## ------------------------------------------------------------------------
rev(y)
rev(x)

## ------------------------------------------------------------------------
x=1:10
set.seed(1)
sample(x,3, replace = TRUE)
sample(x,3)



## ------------------------------------------------------------------------
# reproducible
set.seed(2)
sample(x,3)

## ------------------------------------------------------------------------
x=1:10

sample(x,7)

sample(x,7,replace=T)


## ------------------------------------------------------------------------
sample(c("a","b","c"),10,replace=T)

sample(c('a', 'b', 'c'), 30, replace=T)
# a => machine is alright
# b => machine has failed
# c => it needs repairs

## ------------------------------------------------------------------------
x=sample(c("a","b","c"),1000,replace=T,prob=c(0.6,.1,.3))
x
table(x)

sample(c(1,2,3,4),2,prob=c(0.5,.3,.2,.1))

## ------------------------------------------------------------------------
unique(x)
sum(1:10)
x <- 1:10
length(x[x > 2])/length(x)

mean(x > 2)

# finding missing values, operations with missing value

is.na(c(1,2,3,NA,NA,3,4,5))

c(1,2, NA_character_)

typeof(NA_character_)
typeof(NA_integer_)
typeof(NA_real_)

c(1,2,3,NA,NA,3,4,5)

x=c(1,2,3,NA,NA,3,4,5, NA)

mean(is.na(x))
sum(is.na(x))
sum(c(1,2,3,NA,NA,3,4,5))
sum(c(1,2,3,NA,NA,3,4,5),na.rm=T)

#a vector can not be of mixed type,it is unidimensional
## ------------------------------------------------------------------------
# list are versatile, it can store vector,dataframe etc

x=sample(1:100,10)
y=sample(c("a","b","c"),6,replace=T)
z=4.56

'list1'=list(xyz=x,xyz1=y,mno=z)
typeof(list1)

list1[[1]][1] <- 78
typeof(list1[[1]])

list1[[1]]
list1$xyz
list1$x
list1[['xyz']]
assign(paste0('a','1'), 20)
a1
## ------------------------------------------------------------------------
list1[[2]]

lyst1 <- list(c(1,2,3, 4, 6) , c(2,3,5) ,c(4,5,6))
names(lyst1) <- paste0('a',1:length(lyst1))
lyst1

names(lyst1)
## ------------------------------------------------------------------------
v=list1[[2]]
v[3]


list1[[2]][3]

# 
# list1[[2]]

list1[[2]][3]
list1[2][[1]]#single bracket will give the output as list

## ------------------------------------------------------------------------
list2=list(a=x,b=y,c=z)
list2

## ------------------------------------------------------------------------
list2$b
list2$b[3]

## ------------------------------------------------------------------------
set.seed(2)
x=round(runif(30),2)
y=sample(c("a","b","c"),30,replace = T)
d=data.frame(x,y, stringsAsFactors = F)

d$y
d$x


d$x[1]
d[1, 'x']
d[1:5,c('x', 'y')]


typeof(d)
class(d)
## ------------------------------------------------------------------------
View(d)
d$x
d$y
d[['x']]

## ------------------------------------------------------------------------
names(d)
colnames(d)
# names is a generic function 
## ------------------------------------------------------------------------
names(d)=c("num","char")
names(d)
d
d$x > 5
d <-
  data.frame(x = c(3, 8, 0, 1, 5, 2, 77, 10, 86, 10),
             y = letters[1:10],
             z = LETTERS[11:20])
d[d$x >= 10, c('x', 'y', 'z')]
## ------------------------------------------------------------------------
d=mtcars
d
d$carname=rownames(d)
rownames(d)=NULL

# d$vs <- NULL ## Assigning NULL will remove the column

View(d)
  ## ------------------------------------------------------------------------
dim(d)
nrow(d)
ncol(d)

## ------------------------------------------------------------------------
str(d)

## ------------------------------------------------------------------------
d$mpg <- as.character(d$mpg)
d$mpg <- as.numeric(d$mpg)
## ------------------------------------------------------------------------
data(mtcars)
d1=mtcars
d1

View(d1)
dim(d1)

## ------------------------------------------------------------------------
d1[3,6]

## ------------------------------------------------------------------------
#3rd row , all columns
d1[3,]
class(d1[3,])
# all rows, 6 th colunmn
d1[,6]
class(d1[,6])

# multiple rows, multiple column

d1[c(3,4,20),c(1,4,6)]


d1[-(3:17),-c(1,4,6,7)]

d[d$mpg>20,]


d[d$mpg>20 & d$am==1,]
nrow(d[d$mpg>20 & d$am==1,])

# Selecting column by their names

d[,c("wt","mpg")]

# Excluding columns by their names [ this is a little tricky because simple negative sign doesn't work]

d[,!(names(d1) %in% c("wt","mpg"))]



## ------------------------------------------------------------------------
d[order(d1$vs,d1$wt),c("vs","wt","mpg","gear")]

d[,c('vs', 'wt')]
d[order(d$wt),c('vs', 'wt')]

d[order(d$vs,d$wt),c('vs', 'wt')]

## ------------------------------------------------------------------------
d[order(d$vs,-d$wt),c("vs","wt","mpg","gear")]
##

d2=d1[,c("mpg","cyl","am","gear")]
d1

class(d$mpg)

colMeans(d[,'mpg', drop=FALSE])
mean(d[, 'mpg', drop=TRUE])

class(d[,'mpg', drop=FALSE])

class(d[, 'mpg', drop=TRUE])


#merging dataframes
#merging done by ID 

install.packages('dplyr')



## 
## ------------------------------------------------------------------------
##Explaination of joins: left ,right, inner, full
library(dplyr)


df1 = data.frame(CustomerId = c(1:6), 
                 Product = c(rep("Toaster", 3), rep("Radio", 3)), x = 1:6)

df1


df2 = data.frame(Customer_Id = c(3, 4, 7, 8), 
                 State = c(rep("Alabama", 2), rep("Ohio", 2)))

df2


merge(df1, df2, all.x =T, by.x = 'CustomerId', by.y = 'Customer_Id')

left=left_join(df1,df2,by=("CustomerId"="Customer_Id"))
left

merge(df1, df2, all.y =T, by.x = 'CustomerId', by.y = 'Customer_Id')

right=right_join(df1,df2,by = c('CustomerId' = 'Customer_Id'))
right

inner_join(df1, df2, by = c('CustomerId' = 'Customer_Id'))


merge(df1, df2, all.x = T, all.y = T, by.x = 'CustomerId', by.y = 'Customer_Id')

full=full_join(df1,df2,by= c('CustomerId' = 'Customer_Id'))
full

semi=semi_join(df1,df2,by=c('CustomerId' = 'Customer_Id'))
semi

anti=anti_join(df1,df2,by=c('CustomerId' = 'Customer_Id'))
anti


df1 = data.frame(CustomerId = c(1:6), 
                 Product = c(rep("Toaster", 3), rep("Radio", 3)), x = 1:6)

df2 = data.frame(Customer_Id = c(3, 4, 7, 8), 
                 Product = c(rep("Alabama", 2), rep("Ohio", 2)))



merge(df1, df2, all.x =T, by.x = 'CustomerId', by.y = 'Customer_Id', suffixes = c('_x', '_y'))

# now reverse the data frame and look at the result
## ------------------------------------------------------------------------
inner

## ------------------------------------------------------------------------
left

## ------------------------------------------------------------------------
right

## ------------------------------------------------------------------------
full

## ------------------------------------------------------------------------
semi

## ------------------------------------------------------------------------
anti

## ------------------------------------------------------------------------

x=c("delhi","pune","mumbai",
    "bangalore","hyderabad","lucknow","chennai")

print(nchar(x[1]))
print(nchar(x[2]))
print(nchar(x[3]))
print(nchar(x[4]))
print(nchar(x[5]))
print(nchar(x[6]))
print(nchar(x[7]))

for(i in c(1,2,3,4,5,6,7)){
  print(i*i)
}

mtcars$mpg

for(i in mtcars$mpg){
  print(i + 10)
}

## looping with indexes
x <- c(1,2,3,4,10, 20)
y <- c(2,3,4,5,6,7)
vect <- NULL
for(i in x){
  for(j in y){
    print(i)
    print(j)
    print('----')
    vect <- c(vect, (i + j))
    print(vect)
    print('----')
  }
}
vect

for( i in x){
  print(i)
}

cols <- c('mpg', 'cyl')

for( i in cols){
  print(i)
  print(mtcars[,i])
}



for(i in 1:length(x)){
  print(x[i])
}


for(i in mtcars$mpg){
  print(i)
}

# you can name your index whatever you want
# value vector can be any vector 
# its not necessary that we make use of index in the body of the for loop

for(city in x){
  print('hello')
}

head(mtcars)

for(items in names(mtcars)){
  print(median(mtcars[, items]))
}


# lyst = list()
lyst = vector("list", length(names(mtcars)))
names(lyst) <- paste0(names(mtcars),"_median")

for(items in names(mtcars)){
  lyst[[paste0(items,"_median")]] = median(mtcars[,items])
}
lyst


df <- data.frame(t(data.frame(lyst)))
names(df)
View(df)


df$colnames_median <- rownames(df)
rownames(df) <- NULL

names(df) <- c('median', 'colnames_median')

View(df)
df[, c('colnames_median', 'median')]


miss_replace <- function(x){
  x[is.na(x)] <- mean(x, na.rm=T)
  return(list(x , mean(x, na.rm=T)))
}

miss_replace(c(1,2,3,NA, 10, NA, 8,54,2))


df <- data.frame(x = c(1:5, NA, 7:8,NA, NA),
                 z = c(10:14, NA, NA, NA,2, 10),
                 y = c(rep('a',3),rep('b',5),rep('c',2)))



lapply(df, mean, na.rm=T)
lapply(df[,c('x', 'y')], sum, na.rm=T)

mean_sum <- function(dada) {
  print(environment())
  return(list('mean' = mean(dada, na.rm = T), 'sum' = sum(dada, na.rm = T)))
}

## Return is important if you miss it, then function will not return
## The below function will not work 
mean_sum_will_not_work <- function(dada) {
  l <- list('mean' = mean(dada, na.rm = T), 'sum' = sum(dada, na.rm = T))
}

mean_sum_will_not_work(df$x) ## will not work as we missed the return statement


lapply(df[,c('x','z')], mean_sum)

## body, formals and environment
body(mean_sum)
formals(mean_sum)
environment(mean_sum)
y <- 10
f <- function(x=10){
  y <- 100
  return(x + y)
}
vect <- c(0,1)

fibonnaci <- function(vect, n){
  # vect[3] = vect[1] + vect[2]
  # vect[4] = vect[3] + vect[2]
  # vect[5] = vect[4] + vect[3]
  for(i in 1:n){
    # print(vect[i+1])
    vect[i+2] = vect[i] + vect[i+1]
  }  
  return(vect)  
}
 
fibonnaci(vect, 30)

unlist(strsplit('12322', split=''))

palindrome <- function(x) {
  return(all(unlist(strsplit(x, split = '')) ==  rev(unlist(
    strsplit(x, split = '')
  ))))
}

all(unlist(strsplit('12321', split = '')) == 
      rev(unlist(strsplit('12321', split = ''))))

palindrome('12322')

x = c('a,b,c', 'x y z')
strsplit(x , split = ',|\\s+')

###---------------------Yet to cover the While loop ------------------##