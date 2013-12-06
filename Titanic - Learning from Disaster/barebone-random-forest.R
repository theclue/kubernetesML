require("caret")

formula <- as.formula(".outcome ~ Pclass + Age.factor + Sex + Title + SibSp + Parch +Fare.log")

set.seed(3456)
train.idx <- createDataPartition(titanic.train$.outcome, p = .8, list = FALSE, times=1)

#titanic.forest.train <-titanic.train[,c(2,3,5,7,8,11,12:16)]

titanic.dummyvars <- dummyVars(formula, data=titanic.train)
titanic.forest.train <- as.data.frame(predict(titanic.dummyvars, newdata=titanic.train))
titanic.forest.train <- cbind(titanic.train$.outcome,titanic.forest.train)
colnames(titanic.forest.train)[1] <- ".outcome"

titanic.final.train <- titanic.forest.train[train.idx,]
titanic.final.test <- titanic.forest.train[-train.idx,]

titanic.final.train.subset.folds <- createFolds(titanic.final.train$.outcome, k=10, list=FALSE)

train.error.perc <- numeric()
train.error.size <- numeric()
test.error.perc <- numeric()


for (i in 1:10) {

titanic.current.train <- titanic.final.train[titanic.final.train.subset.folds <= i,]


forest.fitControl <- trainControl( method = "repeatedcv", repeats = 5, summaryFunction = twoClassSummary, classProbs=TRUE, returnData=TRUE, seeds=NULL, savePredictions=TRUE, returnResamp="all")
forest.grid <- createGrid("rf", data=titanic.current.train, len=30)


forest.model1 <- train(x=titanic.current.train[,-1],
                       y=titanic.current.train[,1],
                       method="rf",
                       trControl = forest.fitControl,
                       metric = "ROC",
                       #metric = "Accuracy",
                       tuneGrid = forest.grid,
                       #fitBest = FALSE,
                       importance=TRUE
                       )

confusion.train <- confusionMatrix(forest.model1)$table
train.error.perc <- c(train.error.perc, confusion.train[1,2] + confusion.train[2,1])
train.error.size <- c(train.error.size, nrow(titanic.current.train)) 

# Testset
titanic.final.test.predict <- predict(forest.model1, titanic.final.test)
confusion.test <- confusionMatrix(titanic.final.test.predict, titanic.final.test$.outcome)$table

test.error.perc <- c(test.error.perc, confusion.test[1,2] + confusion.test[2,1])

}

learning.curve <- as.data.frame(cbind(train.error.size, train.error.perc, test.error.perc))

#plot(extractPrediction(list(forest.model1), titanic.final.test))


# ROC Curve
titanic.final.test.predict.prob <- predict(forest.model1, titanic.final.test, type="prob")
result.roc.model1 <-  roc(titanic.final.test$.outcome, titanic.final.test.predict.prob$Yes)
plot(result.roc.model1, print.thres="best", print.thres.best.method="closest.topleft")

result.coords.model1 <- coords(  result.roc.model1, "best", best.method="closest.topleft",
                                 ret=c("threshold", "accuracy"))
result.coords.model1


titanic.submit <- NULL

# Submission

titanic.predict.dummyvars <- dummyVars(formula, data=titanic.predict)
titanic.forest.predict <- as.data.frame(predict(titanic.predict.dummyvars, newdata=titanic.predict))

titanic.submit$PassengerId <- titanic.predict$PassengerId
titanic.submit$Survived <- predict(forest.model1, titanic.forest.predict)
titanic.submit <- as.data.frame(titanic.submit)
levels(titanic.submit$Survived) <- c(0,1)
titanic.submit$Survived <- as.integer(as.character(titanic.submit$Survived))

file2save <- paste("./output/titanic_", forest.model1$method, "_", colnames(forest.model1$bestTune[1]), forest.model1$bestTune[1], "_submission.csv", sep="")

write.csv(x=titanic.submit, file=file2save, row.names=FALSE, quote=FALSE)

