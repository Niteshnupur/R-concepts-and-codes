## ----echo=FALSE,warning=FALSE,message=FALSE------------------------------
library(knitr)
library(dplyr)

## ----echo=FALSE----------------------------------------------------------
library(xtable)
Age=c(32,30,34,20,22,24)
Income=c(150,156,152,148,156,150)
g1=as.factor(c(2,3,2,1,3,1))
d=data.frame(Age,Income)
tbl_df(d)

## This is a table to understand what effects of scale happens when
## variables of different scales are visually seen

## ----echo=FALSE,warning=FALSE,message=F----------------------------------
library(ggplot2)
p=ggplot(d,aes(x=Age,y=Income,color=g1))
p+geom_point()+coord_fixed()

## showing the dots scattered here and there due to non standardisation of data

## ----echo=FALSE----------------------------------------------------------

d$Income=d$Income/10
tbl_df(d)

## One way of normalising the data, not very effective though
## dividing the values by a constant

## ----echo=FALSE,warning=FALSE,message=F----------------------------------
p=ggplot(d,aes(x=Age,y=Income,color=g1))
p+geom_point()+coord_fixed()

## ----echo=FALSE----------------------------------------------------------
d$Age=(d$Age-mean(d$Age))/sd(d$Age)
d$Income=(d$Income-mean(d$Income))/sd(d$Income)
tbl_df(d)

## standardising the data, this very important in clustering or any variance based algorith
## where distance is calculated, distance are impacted when scales are not right, hence
## we should always make them in same scale

## ----echo=FALSE,warning=FALSE,message=F----------------------------------
p=ggplot(d,aes(x=Age,y=Income,color=g1))
p+geom_point()+coord_fixed()

## Plotting the same graph again and compare with without scaled one graph

## ----echo=FALSE----------------------------------------------------------
library(ggplot2)
k=100-100*c(0.00,0.30,0.45,0.84,0.89,0.92,0.94,0.95,0.96,0.96)
d=data.frame(Rsq=c(0.00,0.30,0.45,0.84,0.89,0.92,0.94,0.95,0.96,0.96),SSW=k, K=as.factor(c(1:10)))
p=ggplot(d,aes(x=K,y=SSW,group=1))
p+geom_point(color="red",size=5)+geom_path(color="blue")

## This how the Elbow curve is seen while calculating 'k'
## This is just a representation

## ------------------------------------------------------------------------
data(mtcars)
cars=mtcars

## ------------------------------------------------------------------------
medians = sapply(cars, median)
mads = sapply(cars,mad)
cars.std = data.frame(scale(cars, center = medians, scale = mads))
## scaling the data to run the hierachial clustering

## ------------------------------------------------------------------------
cars.dist = dist(cars.std)


## step1 . Calculate the distance
## ------------------------------------------------------------------------
cars.hclust = hclust(cars.dist)
plot(cars.hclust,labels=rownames(cars),main='Default from hclust',col="black")

summary(cars.hclust)
## step2. Calculate the hclust aglomaration scheduling
## ------------------------------------------------------------------------
groups.3=cutree(cars.hclust,4)
## shtep3. Getting the clusters out from hclust command

cars$cluster = groups.3

## Understanding the behaviour of clusters basis one of the variable,
## you may check for other different variables

cars %>% 
  group_by(cluster) %>% 
  summarise(mean(mpg))




cars %>% 
  group_by(cluster) %>% 
  summarise(mean(drat))



groups.3
table(groups.3)
rownames(cars)[groups.3==1] ## getting the info for cluster no 1
rownames(cars)[groups.3==4] ## getting the infor for cluster no 4 etc
## ------------------------------------------------------------------------

####### k-means


n = 100
g = 6 
set.seed(g)
d <- data.frame(
  x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
  y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d,col="mediumturquoise",pch=16,
     xlab="Arrival Time Deviations", 
     ylab="Departure time Deviations",
     main="Scatter Plot: Delays from Schedule in Minutes ")


## Getting the data ready for k means
## ------------------------------------------------------------------------
mydata <- d

## Getting the wss for entire data:
wss <- (nrow(mydata)-1)*sum(sapply(mydata,var))

## Getting the wss iteratively for each iteration ,
## each iteration increments cluster by one and hence wss decreases, correspondingly
## bss increases every time
## we will plot wss and the number of cluster using plot funciton
## wherever, there is elbow is created in the elbow curve, we will fix that value of k(number of cluster)

## Alternatively you can use vegan package to do the same
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}

dat = data.frame(x = 1:15, y = wss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",col="mediumseagreen",pch=12)


ggplot(data= dat, aes(x = x, y = y)) + geom_point() + geom_line()


## ------------------------------------------------------------------------

## alternative way for kmeans
require(vegan)
fit <- cascadeKM(scale(d, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

## ------------------------------------------------------------------------
set.seed(2)

fit <- kmeans(mydata,5 ) 

## appending the cluster created by kmeans in original data
d$cluster=fit$cluster

head(d)

table(d$cluster)


library(ggplot2)
ggplot(d,aes(x,y,color=as.factor(cluster)))+geom_point()

## ------------------------------------------------------------------------
library(dplyr)

## reading the data
wine=read.csv("C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE\\Data\\winequality-red.csv",sep=";")
wine=wine %>%
  select(pH,sulphates,alcohol,total.sulfur.dioxide)


head(wine)
## ------------------------------------------------------------------------

## standardising function for the data
md=function(x){
  return((x-mean(x))/sd(x))
}

set.seed(1)

## using dplyr to standardise the data:
## you may use lapply also
wine_std=wine %>%
  mutate(pH=md(pH),
         sulphates=md(sulphates),
         alcohol=md(alcohol),
         total.sulfur.dioxide=md(total.sulfur.dioxide))


require(vegan)

## alernate way of doing kmeans
fit <- cascadeKM(wine_std, 1, 10, iter = 100)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


library(cluster)

## Silhoutte coefficient calculation
average_sil <- function(df,k,seed) {
  set.seed(seed)
  km_sel <- kmeans(df, centers = k, nstart = 10)
  ## do a kmeans fixing the nstart to 10(you can choose any number)
  sil <- silhouette(km_sel$cluster, dist(df))
  ## get the silhouette using silhouette from cluster package
  return(mean(sil[, 3])) ## get the mean of last column, This should not change irrespecting of different cluster assignment
  ## for example : one can have differnt numbers are assigned for frequencies but silhouette shouldn't change 
}

## Run the command to get the silhouette
seeds <- 1:10
names(seeds) <- make.names(1:10)
## You can check with changing seed even we have the same silhouette
sapply(seeds, function(x)average_sil(wine_std, 5, x), USE.NAMES = T)

## ------------------------------------------------------------------------
library(ggplot2)

#pair wise profiling plots
ggplot(wine_std,aes(pH,alcohol,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(pH,sulphates,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(pH,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(alcohol,sulphates,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(alcohol,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()
ggplot(wine_std,aes(sulphates,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()


head(wine_std)

