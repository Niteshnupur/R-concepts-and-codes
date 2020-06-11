#####Using CARET Ranger(RandomForest another way)###

library(caret) ; 

library(ranger) ## randomForest another library


## change you dependent variable to factor in case you are doing logistic

st_train1$store <- as.factor(st_train1$store)

st_train1$store <- as.factor(st_train1$store)


##  change the grid make it more exhaustive , the below is not exhaustive(its simple)


rf_grid <- expand.grid(mtry = c(6,7, 8), ## the grids are little different for ranger
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(3, 5 , 10))


fitControl <- trainControl(## 10-fold CV
    method = "cv",
    # repeats = 5,
    # sampling = 'up' , ## In case of doing an upsample
    number = 10,
    summaryFunction = twoClassSummary,
    classProbs = TRUE, verboseIter = TRUE)


## caret requires variables levels not to be numbers hence always do this

levels(st_train1$store ) <- make.names(levels(st_train1$store ))

levels(st_train2$store ) <- make.names(levels(st_train2$store ) )


## random forest
rf_fit <- train(form, 
                data = st_train1, 
                method = "ranger",
                num.trees = 700,
                trControl = fitControl,
                # provide a grid of parameters
                tuneGrid = rf_grid, metric = 'ROC')


## getting auc

caTools::colAUC(predict(rf_fit, newdata = st_train1, type = 'prob')[,'X1'], hr_train1$left)

caTools::colAUC(predict(rf_fit, newdata = st_train2, type = 'prob')[,'X1'], hr_train2$left)




### GBM


fitControl <- trainControl(## 10-fold CV
    method = "cv",
    # repeats = 5,
    number = 5,
    summaryFunction = twoClassSummary,
    classProbs = TRUE)

set.seed(1)

gbmGrid <- Âexpand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.01,
                        n.minobsinnode = 20)

gbm_samp_grid <- sample(1:nrow(gbmGrid),.50*nrow(gbmGrid))
grid <- gbmGrid[gbm_samp_grid,]


gbmFit1 <- train(left ~ ., data = hr_train1, 
                 method = "gbm", 
                 trControl = fitControl,
                 metric = 'ROC',
                 tuneGrid = grid,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)


caTools::colAUC(predict(gbmFit1, newdata = hr_train1, type = 'prob')[,'X1'], hr_train1$left)

caTools::colAUC(predict(gbmFit1, newdata = hr_train2, type = 'prob')[,'X1'], hr_train2$left)



