require("caret")

#inTrain <- createFolds(titanic.train, k = 10)
#inTrain <- inTrain <- createDataPartition(titanic.train, p = 3/4, list = FALSE)

forest.model1 <- train(Survived ~ Pclass + Age.factor + Sex + Title + SibSp +Parch ,
                       titanic.train,
                       importance=TRUE)

titanic.submit <- NULL
titanic.submit$PassengerId <- titanic.test$PassengerId
titanic.submit$Survived <- predict(forest.model1, titanic.test)
titanic.submit <- as.data.frame(titanic.submit)
levels(titanic.submit$Survived) <- c(0,1)
titanic.submit$Survived <- as.integer(as.character(titanic.submit$Survived))

write.csv(x=titanic.submit, file="./output/titanic_raw_rforest_submission.csv", row.names=FALSE, quote=FALSE)

