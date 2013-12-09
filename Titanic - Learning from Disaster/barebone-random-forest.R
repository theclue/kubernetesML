require("caret")

# Parameters
formula <- as.formula(".outcome ~ Pclass + Age.factor + Sex + Title + SibSp + Parch +Fare.log")
folds <- 10

set.seed(3456)
train.idx <- createDataPartition(titanic.train$.outcome, p = .8, list = FALSE, times=1)

#titanic.forest.train <-titanic.train[,c(2,3,5,7,8,11,12:16)]

titanic.dummyvars <- dummyVars(formula, data=titanic.train)
titanic.forest.train <- as.data.frame(predict(titanic.dummyvars, newdata=titanic.train))
titanic.forest.train <- cbind(titanic.train$.outcome,titanic.forest.train)
colnames(titanic.forest.train)[1] <- ".outcome"

titanic.final.train <- titanic.forest.train[train.idx,]
titanic.final.test <- titanic.forest.train[-train.idx,]

titanic.final.train.subset.folds <- createFolds(titanic.final.train$.outcome, k=folds, list=FALSE)

train.error.perc <- numeric()
train.error.size <- numeric()
test.error.perc <- numeric()


for (i in 1:folds) {

writeLines(paste("Training with ", i, "/", folds, " of the training set...", sep=""))

titanic.current.train <- titanic.final.train[titanic.final.train.subset.folds <= i,]

#forest.fitControl <- trainControl( method = "repeatedcv", repeats = 5, summaryFunction = twoClassSummary, classProbs=TRUE, returnData=TRUE, seeds=NULL, savePredictions=TRUE, returnResamp="all")
forest.fitControl <- trainControl(method="oob", repeats=5, number=5, summaryFunction = twoClassSummary, classProbs=TRUE, returnData=TRUE, seeds=NULL, savePredictions=TRUE, returnResamp="all")

forest.grid <- createGrid("rf", data=titanic.current.train, len=14)


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

confusion.train <- confusionMatrix(predict(forest.model1, titanic.current.train), titanic.current.train$.outcome)$table
train.error.perc <- c(train.error.perc, (confusion.train[1,2] + confusion.train[2,1])/nrow(titanic.current.train))
train.error.size <- c(train.error.size, nrow(titanic.current.train)) 

# Testset
titanic.final.test.predict <- predict(forest.model1, titanic.final.test)
confusion.test <- confusionMatrix(titanic.final.test.predict, titanic.final.test$.outcome)$table

test.error.perc <- c(test.error.perc, (confusion.test[1,2] + confusion.test[2,1])/nrow(titanic.final.test))

}

learning.curve <- as.data.frame(cbind(train.error.size, train.error.perc, test.error.perc))

#plotObsVsPred(extractPrediction(list(forest.model1), testX=titanic.final.test[,-1], testY=titanic.final.test[,1]))
#plotClassProbs(extractProb(list(forest.model1), testX=titanic.final.test[,-1], testY=titanic.final.test[,1]))

# Learning curve on .mtry
titanic.test.learning <- numeric()
titanic.train.learning <- numeric()
for(hp in 1 : nrow(forest.grid)){
  
  forest.model.temp <- update(forest.model1, list(.mtry = forest.grid[hp,1]))
  titanic.final.test.predict.temp <- predict(forest.model.temp, titanic.final.test)
  titanic.final.train.predict.temp <- predict(forest.model.temp)
  
  confusion.train.temp <- confusionMatrix(titanic.final.train.predict.temp, titanic.final.train$.outcome)$table
  confusion.test.temp <- confusionMatrix(titanic.final.test.predict.temp, titanic.final.test$.outcome)$table
  
  titanic.test.learning <- c(titanic.test.learning, (confusion.test.temp[1,2] + confusion.test.temp[2,1])/nrow(titanic.final.test))
  titanic.train.learning <- c(titanic.train.learning, (confusion.train.temp[1,2] + confusion.train.temp[2,1])/nrow(titanic.final.train)) 
  
}

learning.curve.mtry <- as.data.frame(cbind(forest.grid[,1], titanic.train.learning, titanic.test.learning))


# ROC Curve
titanic.final.test.predict.prob <- predict(forest.model1, titanic.final.test, type="prob")
result.roc.model1 <-  roc(titanic.final.test$.outcome, titanic.final.test.predict.prob$Yes)
plot(result.roc.model1, print.thres="best", print.thres.best.method="closest.topleft")

result.coords.model1 <- coords(  result.roc.model1, "best", best.method="closest.topleft",
                                 ret=c("threshold", "accuracy"))
result.coords.model1

# Plot Learning Curves
require("ggplot2")
require("gridExtra")

l1 <- ggplot(learning.curve, aes(x=train.error.size, color=Set)) +                    
  geom_line(aes(y=train.error.perc, color="Training set")) +
  geom_line(aes(y=test.error.perc, color="Test set")) +
  xlab("Training set size") +
  ylab("Missclassification Error")

l2 <- ggplot(learning.curve.mtry, aes(x=V1, color=Set)) +                    
  geom_line(aes(y=titanic.train.learning, color="Training set")) +
  geom_line(aes(y=titanic.test.learning, color="Test set")) +
  xlab("mtry") +
  ylab("Missclassification Error")

grid.arrange(l1, l2, ncol=2)



# Submission

titanic.submit <- NULL

titanic.predict$.outcome <- NA
titanic.predict.dummyvars <- dummyVars(formula, data=titanic.predict)
titanic.forest.predict <- as.data.frame(predict(titanic.predict.dummyvars, newdata=titanic.predict))

titanic.submit$PassengerId <- titanic.predict$PassengerId
titanic.submit$Survived <- predict(forest.model1, titanic.forest.predict)
titanic.submit <- as.data.frame(titanic.submit)
levels(titanic.submit$Survived) <- c(0,1)
titanic.submit$Survived <- as.integer(as.character(titanic.submit$Survived))

file2save <- paste("./output/titanic_", forest.model1$method, "_", colnames(forest.model1$bestTune[1]), forest.model1$bestTune[1], "_submission.csv", sep="")

write.csv(x=titanic.submit, file=file2save, row.names=FALSE, quote=FALSE)

