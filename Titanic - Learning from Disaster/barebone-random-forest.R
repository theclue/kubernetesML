require("caret")

colnames(titanic.train)[2] <- ".outcome"

set.seed(3456)
train.idx <- createDataPartition(titanic.train$.outcome, p = .8, list = FALSE, times=1)

titanic.final.train <- titanic.train[train.idx,]
titanic.final.test <- titanic.train[-train.idx,]

titanic.final.train.subset.folds <- createFolds(titanic.final.train$.outcome, k=8, list=FALSE)

train.error.perc <- numeric()
train.error.size <- numeric()
test.error.perc <- numeric()


for (i in 8:8) {

titanic.current.train <- titanic.final.train[titanic.final.train.subset.folds <= i,]

#formula <- as.formula(".outcome ~ Pclass + Age.factor + Sex + Title + SibSp +Parch +Embarked")
formula <- as.formula(".outcome ~ Pclass + Age.factor + Sex + Title + SibSp + Parch +isAlone")

forest.fitControl <- trainControl( method = "repeatedcv", repeats = 5, summaryFunction = twoClassSummary, classProbs=TRUE, returnData=TRUE, seeds=NULL, savePredictions=TRUE, returnResamp="all")
#forest.fitControl <- trainControl(method = "repeatedcv", repeats = 5, classProbs=TRUE, returnResamp="all")
forest.grid <- createGrid("rf", len=13, data=titanic.current.train)


forest.model1 <- train(formula,
                       method="rf",
                       titanic.current.train,
                       trControl = forest.fitControl,
                       metric = "ROC",
                       #metric = "Accuracy",
                       tuneGrid = forest.grid,
                       fitBest = FALSE,
                       importance=TRUE)

confusion.train <- confusionMatrix(forest.model1)$table
train.error.perc <- c(train.error.perc, confusion.train[1,2] + confusion.train[2,1])
train.error.size <- c(train.error.size, nrow(titanic.current.train)) 

# Testset
titanic.final.test.predict <- predict(forest.model1, titanic.final.test)
confusion.test <- confusionMatrix(titanic.final.test.predict, titanic.final.test$.outcome)$table

test.error.perc <- c(test.error.perc, confusion.test[1,2] + confusion.test[2,1])

}

learning.curve <- as.data.frame(cbind(train.error.size, train.error.perc, test.error.perc))

extractPrediction(list(forest.model1))


# ROC Curve
titanic.final.test.predict.prob <- predict(forest.model1, titanic.final.test, type="prob")
result.roc.model1 <-  roc(titanic.final.test$.outcome, titanic.final.test.predict.prob$Yes)
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

write.csv(x=titanic.submit, file="./output/titanic_mtry2_rforest_submission.csv", row.names=FALSE, quote=FALSE)

