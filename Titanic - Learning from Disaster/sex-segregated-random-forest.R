require("caret")

formula <- as.formula(".outcome ~ Pclass + Age.factor + Title + SibSp + Parch +Fare.log +isAlone")

titanic.dummyvars <- dummyVars(formula, data=titanic.train)

titanic.train.male <- titanic.train[titanic.train$Sex=="M",]
titanic.train.female <- titanic.train[titanic.train$Sex=="F",]

titanic.forest.train.male <- as.data.frame(predict(titanic.dummyvars, newdata=titanic.train.male))
titanic.forest.train.male <- cbind(titanic.train.male$.outcome,titanic.forest.train.male)
colnames(titanic.forest.train.male)[1] <- ".outcome"
                         
titanic.forest.train.female <- as.data.frame(predict(titanic.dummyvars, newdata=titanic.train.female))
titanic.forest.train.female <- cbind(titanic.train.female$.outcome,titanic.forest.train.female)
colnames(titanic.forest.train.female)[1] <- ".outcome"

train.idx.male <- createDataPartition(titanic.forest.train.male$.outcome, p = .8, list = FALSE, times=1)
train.idx.female <- createDataPartition(titanic.forest.train.female$.outcome, p = .8, list = FALSE, times=1)

titanic.train.male <- titanic.forest.train.male[train.idx.male,]
titanic.train.female <- titanic.forest.train.female[train.idx.female,]

titanic.test.male <- titanic.forest.train.male[-train.idx.male,]
titanic.test.female <- titanic.forest.train.female[-train.idx.female,]

# Clean-up workspace
rm("train.idx.male", "train.idx.female", "titanic.forest.train.male", "titanic.forest.train.female")

####################
# TRAINING PHASE   #
####################
forest.fitControl <- trainControl(method="repeatedcv",
                                       repeats=5,
                                       number=10,
                                       summaryFunction = twoClassSummary,
                                       classProbs=TRUE,
                                       returnData=TRUE,
                                       savePredictions=TRUE,
                                       returnResamp="all")


forest.grid <- createGrid("rf", data=titanic.train.male, len=14)

model.rf.male <- train(x=titanic.train.male[,-1],
                       y=titanic.train.male[,1],
                       method="rf",
                       trControl = forest.fitControl,
                       metric = "ROC",
                       tuneGrid = forest.grid,
                       importance=TRUE)

model.rf.female <- train(x=titanic.train.female[,-1],
                       y=titanic.train.female[,1],
                       method="rf",
                       trControl = forest.fitControl,
                       metric = "ROC",
                       tuneGrid = forest.grid,
                       importance=TRUE)
                                           
##############
# PREDICTION #
##############

confusion.train.male <- confusionMatrix(predict(model.rf.male, titanic.train.male), titanic.train.male$.outcome)$table
train.error.male <- (confusion.train.male[1,2] + confusion.train.male[2,1])/nrow(titanic.train.male)

confusion.train.female <- confusionMatrix(predict(model.rf.female, titanic.train.female), titanic.train.female$.outcome)$table
train.error.female <- (confusion.train.female[1,2] + confusion.train.female[2,1])/nrow(titanic.train.female)

train.error.overall <- ((confusion.train.male[1,2] + confusion.train.male[2,1]) + (confusion.train.female[1,2] + confusion.train.female[2,1]))/(nrow(titanic.train.male) + nrow(titanic.train.female))

# Testset
titanic.test.male.predict <- predict(model.rf.male, titanic.test.male)
confusion.test.male <- confusionMatrix(titanic.test.male.predict, titanic.test.male$.outcome)$table
test.error.male <- (confusion.test.male[1,2] + confusion.test.male[2,1])/nrow(titanic.test.male)

titanic.test.female.predict <- predict(model.rf.female, titanic.test.female)
confusion.test.female <- confusionMatrix(titanic.test.female.predict, titanic.test.female$.outcome)$table
test.error.female <- (confusion.test.female[1,2] + confusion.test.female[2,1])/nrow(titanic.test.female)

test.error.overall <- ((confusion.test.male[1,2] + confusion.test.male[2,1]) + (confusion.test.female[1,2] + confusion.test.female[2,1]))/(nrow(titanic.test.male) + nrow(titanic.test.female))

titanic.error <- cbind(train.error.male, train.error.female, train.error.overall, test.error.male, test.error.female, test.error.overall)

rm("confusion.train.female", "confusion.train.male", "confusion.test.male", "confusion.test.female", "train.error.male", "train.error.female", "test.error.male", "test.error.female", "train.error.overall", "test.error.overall")
 
################
# EVALUATION   #
################

op <- par(mfrow = c(1, 2))

result.roc.male <-  roc(titanic.test.male$.outcome, predict(model.rf.male, titanic.test.male, type="prob")$Yes)
plot(result.roc.male, print.thres="best", print.thres.best.method="closest.topleft", main="ROC (Male)")
result.coords.male <- coords(result.roc.male, 
                               "best", 
                               best.method="closest.topleft",
                               ret=c("threshold", "accuracy"))

result.roc.female <-  roc(titanic.test.female$.outcome, predict(model.rf.female, titanic.test.female, type="prob")$Yes)
plot(result.roc.female, print.thres="best", print.thres.best.method="closest.topleft", main="ROC (Female)")
result.coords.female <- coords(result.roc.female, 
                             "best", 
                             best.method="closest.topleft",
                             ret=c("threshold", "accuracy"))

par(op)

rm("op")

# Learning curves on .mtry
titanic.test.learning.male <- numeric()
titanic.train.learning.male <- numeric()
titanic.test.learning.female <- numeric()
titanic.train.learning.female <- numeric()


for(hp in 1 : nrow(forest.grid)){
  
  forest.model.temp <- update(model.rf.male, list(.mtry = forest.grid[hp,1]))
  titanic.final.test.predict.temp <- predict(forest.model.temp, titanic.test.male)
  titanic.final.train.predict.temp <- predict(forest.model.temp)
  
  confusion.train.temp <- confusionMatrix(titanic.final.train.predict.temp, titanic.train.male$.outcome)$table
  confusion.test.temp <- confusionMatrix(titanic.final.test.predict.temp, titanic.test.male$.outcome)$table
  
  titanic.test.learning.male <- c(titanic.test.learning.male, (confusion.test.temp[1,2] + confusion.test.temp[2,1])/nrow(titanic.test.male))
  titanic.train.learning.male <- c(titanic.train.learning.male, (confusion.train.temp[1,2] + confusion.train.temp[2,1])/nrow(titanic.train.male)) 
  
}

for(hp in 1 : nrow(forest.grid)){
  
  forest.model.temp <- update(model.rf.female, list(.mtry = forest.grid[hp,1]))
  titanic.final.test.predict.temp <- predict(forest.model.temp, titanic.test.female)
  titanic.final.train.predict.temp <- predict(forest.model.temp)
  
  confusion.train.temp <- confusionMatrix(titanic.final.train.predict.temp, titanic.train.female$.outcome)$table
  confusion.test.temp <- confusionMatrix(titanic.final.test.predict.temp, titanic.test.female$.outcome)$table
  
  titanic.test.learning.female <- c(titanic.test.learning.female, (confusion.test.temp[1,2] + confusion.test.temp[2,1])/nrow(titanic.test.female))
  titanic.train.learning.female <- c(titanic.train.learning.female, (confusion.train.temp[1,2] + confusion.train.temp[2,1])/nrow(titanic.train.female)) 
  
}

learning.curve.mtry <- as.data.frame(cbind(forest.grid[,1], titanic.train.learning.male, titanic.train.learning.female, titanic.test.learning.male, titanic.test.learning.female))

rm("hp", "forest.model.temp", "titanic.final.test.predict.temp", "titanic.final.train.predict.temp", "confusion.train.temp", "confusion.test.temp", "titanic.train.learning.male", "titanic.train.learning.female", "titanic.test.learning.male", "titanic.test.learning.female")

# Plot Learning Curves
require("ggplot2")
require("gridExtra")

l1 <- ggplot(learning.curve.mtry, aes(x=V1, color=Male)) +                    
  geom_line(aes(y=titanic.train.learning.male, color="Training set")) +
  geom_line(aes(y=titanic.test.learning.male, color="Test set")) +
  xlab("mtry") +
  ylab("Missclassification Error")

l2 <- ggplot(learning.curve.mtry, aes(x=V1, color=Female)) +                    
  geom_line(aes(y=titanic.train.learning.female, color="Training set")) +
  geom_line(aes(y=titanic.test.learning.female, color="Test set")) +
  xlab("mtry") +
  ylab("Missclassification Error")

grid.arrange(l1, l2, ncol=2)

rm("l1", "l2")

##############
# SUBMISSION #
##############

titanic.submit <- NULL
titanic.submit.male <- NULL
titanic.submit.female <- NULL

titanic.predict$.outcome <- NA
titanic.predict.dummyvars <- dummyVars(formula, data=titanic.predict)
titanic.forest.predict <- as.data.frame(predict(titanic.predict.dummyvars, newdata=titanic.predict))

titanic.predict.male <- titanic.forest.predict[titanic.predict$Sex=="M",]
titanic.predict.female <- titanic.forest.predict[titanic.predict$Sex=="F",]

titanic.submit.male$PassengerId <- titanic.predict[titanic.predict$Sex=="M",]$PassengerId
titanic.submit.male$Survived <- predict(model.rf.male, titanic.predict.male)
titanic.submit.female$PassengerId <- titanic.predict[titanic.predict$Sex=="F",]$PassengerId
titanic.submit.female$Survived <- predict(model.rf.female, titanic.predict.female)

titanic.submit.male <- as.data.frame(titanic.submit.male)
titanic.submit.female <- as.data.frame(titanic.submit.female)


titanic.submit <- rbind(titanic.submit.male, titanic.submit.female)
levels(titanic.submit$Survived) <- c(0,1)

titanic.submit <- titanic.submit[order(titanic.submit[,1]), ]

titanic.submit$Survived <- as.integer(as.character(titanic.submit$Survived))

write.csv(x=titanic.submit, file="./output/titanic_segregated_submission.csv", row.names=FALSE, quote=FALSE)

