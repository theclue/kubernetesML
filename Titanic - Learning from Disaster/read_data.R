titanic.train <- read.csv2(file="./data/train.csv", header=TRUE, sep=",")
titanic.test <- read.csv2(file="./data/test.csv", header=TRUE, sep=",")

titanic.train$Age <- as.integer(titanic.train$Age)
titanic.train$Pclass <- as.factor(titanic.train$Pclass)
titanic.train$Survived <- as.factor(titanic.train$Survived)
titanic.train$Fare <- as.integer(titanic.train$Fare)

titanic.test$Age <- as.integer(titanic.test$Age)
titanic.test$Pclass <- as.factor(titanic.test$Pclass)
titanic.test$Fare <- as.integer(titanic.test$Fare)
titanic.test$Survived <- as.factor(0)

titanic.submit <- read.csv2(file="./output/titanic_score_idents.csv", header=TRUE, sep=",")
titanic.submit <- titanic.submit[,1:2]
write.csv(x=titanic.submit, file="./output/titanic_raw_svm_submission.csv", col.names=TRUE, row.names=FALSE, sep=",", quote=FALSE)
