require("plotrix")

titanic.train <- read.csv2(file="./data/train.csv", header=TRUE, sep=",")
titanic.test <- read.csv2(file="./data/test.csv", header=TRUE, sep=",")

titanic.colors<-list("gray90",c("#0000ff","#7700ee","#aa00cc","#dd00aa"), c("#ddcc00","#ee9900"),c("pink","lightblue"))

##### TRAINING SET

titanic.train$Age <- as.integer(as.character(titanic.train$Age))
titanic.train$Pclass <- as.factor(titanic.train$Pclass)
titanic.train$Survived <- as.factor(titanic.train$Survived)
titanic.train$Fare <- as.integer(as.character(titanic.train$Fare))
titanic.train$Cabin[which(titanic.train$Cabin == "")] <- NA
titanic.train$Cabin <- as.character(titanic.train$Cabin)
titanic.train$Deck <- (x=substr(titanic.train$Cabin, 0, 1))
titanic.train$Deck[which(is.na(titanic.train$Deck))] <- "U"
titanic.train$Deck <- factor(titanic.train$Deck)

titanic.train$Age.factor <- factor(x=as.character(ifelse(titanic.train$Age<12, "Child", "Adult")))

titanic.train$Title <- "Other"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Miss.", fixed=T))] <- "Miss"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Mr.", fixed=T))] <- "Mr"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Mrs.", fixed=T))] <- "Mrs"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Master.", fixed=T))] <- "Master"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Rev.", fixed=T))] <- "Rev"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Dr.", fixed=T))] <- "Dr"
titanic.train$Title[which(grepl(x=titanic.train$Name, pattern="Sir.", fixed=T))] <- "Mr"

# Relabel some factors for exploratory analysis
levels(titanic.train$Survived)<- c('No', 'Yes')
levels(titanic.train$Sex)<- c('F', 'M')

### TEST SET

titanic.test$Age <- as.integer(as.character(titanic.test$Age))
titanic.test$Pclass <- as.factor(titanic.test$Pclass)
titanic.test$Fare <- as.integer(as.character(titanic.test$Fare))
titanic.test$Cabin[which(titanic.test$Cabin == "")] <- NA
titanic.test$Cabin <- as.character(titanic.test$Cabin)
titanic.test$Deck <- (x=substr(titanic.test$Cabin, 0, 1))
titanic.test$Deck[which(is.na(titanic.test$Deck))] <- "U"
titanic.test$Deck <- factor(titanic.test$Deck)

titanic.test$Age.factor <- factor(x=as.character(ifelse(titanic.test$Age<12, "Child", "Adult")))


#levels(titanic.train$Survived)<- c('No', 'Yes')
levels(titanic.test$Sex)<- c('F', 'M')
#titanic.test$Survived <- as.factor(0)
