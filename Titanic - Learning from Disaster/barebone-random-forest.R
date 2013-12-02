require("caret")

set.seed(3456)
train.idx <- createDataPartition(titanic.train$Survived, p = .8, list = FALSE, times=1)

titanic.final.train <- titanic.train[train.idx,]
titanic.final.test <- titanic.train[-train.idx,]

forest.fitControl <- trainControl( method = "repeatedcv", number = 5, summaryFunction = twoClassSummary, classProbs=TRUE) 

registerDoParallel(makeCluster(6), cores=6)
forest.model1 <- train(Survived ~ Pclass + Age.factor + Sex + Title + SibSp +Parch +Embarked,
#forest.model1 <- train(Survived ~ .,
                       titanic.final.train,
                       trControl = forest.fitControl,
                       tuneLength=12,
                       metric = "ROC",
                       importance=TRUE)

# Testset
titanic.final.test.predict <- predict(forest.model1, titanic.final.test)
confusionMatrix(titanic.final.test.predict, titanic.final.test$Survived)

# ROC Curve
titanic.final.test.predict.prob <- predict(forest.model1, titanic.final.test, type="prob")
result.roc.model1 <-  roc(titanic.final.test$survived, titanic.final.test.predict.prob$yes)
plot(result.roc.model1, print.thres="best", print.thres.best.method="closest.topleft")

result.coords.model1 <- coords(  result.roc.model1, "best", best.method="closest.topleft",
                                 ret=c("threshold", "accuracy"))
result.coords.model1


titanic.submit <- NULL


titanic.submit$PassengerId <- titanic.predict$PassengerId
titanic.submit$Survived <- predict(forest.model1, titanic.predict)
titanic.submit <- as.data.frame(titanic.submit)
levels(titanic.submit$Survived) <- c(0,1)
titanic.submit$Survived <- as.integer(as.character(titanic.submit$Survived))

write.csv(x=titanic.submit, file="./output/titanic_mtry5_rforest_submission.csv", row.names=FALSE, quote=FALSE)

