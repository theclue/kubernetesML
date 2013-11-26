titanic.train <- read.csv2(file="./data/train.csv", header=TRUE, sep=",")
titanic.test <- read.csv2(file="./data/test.csv", header=TRUE, sep=",")

titanic.colors<-list("gray90",c("#0000ff","#7700ee","#aa00cc","#dd00aa"), c("#ddcc00","#ee9900"),c("pink","lightblue"))

titanic.train$Age <- as.integer(titanic.train$Age)
titanic.train$Pclass <- as.factor(titanic.train$Pclass)
titanic.train$Survived <- as.factor(titanic.train$Survived)
titanic.train$Fare <- as.integer(titanic.train$Fare)
titanic.train$Deck <- factor(x=substr(titanic.train$Cabin, 0, 1))

titanic.train$Age.factor <- factor(x=as.character(ifelse(titanic.train$Age<12, "Bambino", "Adulto")))
levels(titanic.train$Survived)<- c('No', 'Si')
levels(titanic.train$Sex)<- c('F', 'M')

titanic.test$Age <- as.integer(titanic.test$Age)
titanic.test$Pclass <- as.factor(titanic.test$Pclass)
titanic.test$Fare <- as.integer(titanic.test$Fare)
titanic.test$Survived <- as.factor(0)
