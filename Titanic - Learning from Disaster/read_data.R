require("plotrix")

titanic.train <- read.csv2(file="./data/train.csv", header=TRUE, sep=",")
titanic.predict <- read.csv2(file="./data/test.csv", header=TRUE, sep=",")

titanic.colors<-list("gray90",c("#0000ff","#7700ee","#aa00cc","#dd00aa"), c("#ddcc00","#ee9900"),c("pink","lightblue"))

##### TRAINING SET

titanic.train$Age <- as.integer(as.character(titanic.train$Age))
titanic.train$Pclass <- as.factor(titanic.train$Pclass)
titanic.train$Survived <- as.factor(titanic.train$Survived)
titanic.train$Fare <- as.numeric(as.character(titanic.train$Fare))
titanic.train$Cabin[which(titanic.train$Cabin == "")] <- NA
titanic.train$Cabin <- as.character(titanic.train$Cabin)
titanic.train$Deck <- (x=substr(titanic.train$Cabin, 0, 1))
titanic.train$Deck[which(is.na(titanic.train$Deck))] <- "U"
titanic.train$Deck <- factor(titanic.train$Deck)

titanic.train$Title <- "Other"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Miss.", fixed=T))] <- "Miss"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Mr.", fixed=T))] <- "Mr"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Mrs.", fixed=T))] <- "Mrs"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Master.", fixed=T))] <- "Master"

titanic.train$Age[which(is.na(titanic.train$Age) & titanic.train$Title == "Mrs")] <- round(mean(titanic.train$Age[which(titanic.train$Title == "Mrs")], na.rm=TRUE))
titanic.train$Age[which(is.na(titanic.train$Age) & titanic.train$Title == "Mr")] <- round(mean(titanic.train$Age[which(titanic.train$Title == "Mr")], na.rm=TRUE))
titanic.train$Age[which(is.na(titanic.train$Age) & titanic.train$Title == "Miss")] <- round(mean(titanic.train$Age[which(titanic.train$Title == "Miss")], na.rm=TRUE))
titanic.train$Age[which(is.na(titanic.train$Age) & titanic.train$Title == "Master")] <- round(mean(titanic.train$Age[which(titanic.train$Title == "Master")], na.rm=TRUE))
titanic.train$Age[which(is.na(titanic.train$Age) & titanic.train$Title == "Other")] <- round(mean(titanic.train$Age[which(titanic.train$Title == "Other")], na.rm=TRUE))

titanic.train$Age.factor <- factor(x=as.character(ifelse(titanic.train$Age<=12, "Child", "Adult")))

titanic.train$isAlone <- factor(ifelse(titanic.train$Parch==0 & titanic.train$SibSp == 0, "Alone", "Not Alone"))

# Relabel some factors for exploratory analysis
levels(titanic.train$Survived)<- c('No', 'Yes')
levels(titanic.train$Sex)<- c('F', 'M')

### TEST SET

titanic.predict$Age <- as.integer(as.character(titanic.predict$Age))
titanic.predict$Pclass <- as.factor(titanic.predict$Pclass)
titanic.predict$Fare <- as.numeric(as.character(titanic.predict$Fare))
titanic.predict$Cabin[which(titanic.predict$Cabin == "")] <- NA
titanic.predict$Cabin <- as.character(titanic.predict$Cabin)
titanic.predict$Deck <- (x=substr(titanic.predict$Cabin, 0, 1))
titanic.predict$Deck[which(is.na(titanic.predict$Deck))] <- "U"
titanic.predict$Deck <- factor(titanic.predict$Deck)

titanic.predict$Title <- "Other"
titanic.predict$Title[which(grepl(x=titanic.predict$Name, pattern="Miss.", fixed=T))] <- "Miss"
titanic.predict$Title[which(grepl(x=titanic.predict$Name, pattern="Mr.", fixed=T))] <- "Mr"
titanic.predict$Title[which(grepl(x=titanic.predict$Name, pattern="Mrs.", fixed=T))] <- "Mrs"
titanic.predict$Title[which(grepl(x=titanic.predict$Name, pattern="Master.", fixed=T))] <- "Master"

titanic.predict$Age[which(is.na(titanic.predict$Age) & titanic.predict$Title == "Mrs")] <- round(mean(titanic.predict$Age[which(titanic.predict$Title == "Mrs")], na.rm=TRUE))
titanic.predict$Age[which(is.na(titanic.predict$Age) & titanic.predict$Title == "Mr")] <- round(mean(titanic.predict$Age[which(titanic.predict$Title == "Mr")], na.rm=TRUE))
titanic.predict$Age[which(is.na(titanic.predict$Age) & titanic.predict$Title == "Miss")] <- round(mean(titanic.predict$Age[which(titanic.predict$Title == "Miss")], na.rm=TRUE))
titanic.predict$Age[which(is.na(titanic.predict$Age) & titanic.predict$Title == "Master")] <- round(mean(titanic.predict$Age[which(titanic.predict$Title == "Master")], na.rm=TRUE))
titanic.predict$Age[which(is.na(titanic.predict$Age) & titanic.predict$Title == "Other")] <- round(mean(titanic.predict$Age[which(titanic.predict$Title == "Other")], na.rm=TRUE))

titanic.predict$isAlone <- factor(ifelse(titanic.predict$Parch==0 & titanic.predict$SibSp == 0, "Alone", "Not Alone"))

titanic.predict$Age.factor <- factor(x=as.character(ifelse(titanic.predict$Age<=12, "Child", "Adult")))

#levels(titanic.train$Survived)<- c('No', 'Yes')
levels(titanic.predict$Sex)<- c('F', 'M')
#titanic.predict$Survived <- as.factor(0)

# Removing last missing
titanic.train$Cabin <- NULL
titanic.predict$Cabin <- NULL
titanic.predict[is.na(titanic.predict$Fare),]$Fare <- mean(titanic.predict$Fare, na.rm=TRUE)