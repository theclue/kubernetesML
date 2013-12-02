require("Amelia")
require("plotrix")

# Data breakdown

op <- par(mfrow = c(2, 1))

barNest(Survived~Age.factor+Sex+Pclass,titanic.train,col=titanic.colors,showall=TRUE,
        main="Sopravvissuti divisi per classe, età, sesso",ylab="Proporzione sopravvissuti",
        FUN="propbrk",shrink=0.15,trueval="Si")

barNest(Survived~Age.factor+Sex+Pclass,titanic.train,col=titanic.colors,showall=TRUE,
        main="Passeggeri divisi per classe, età, sesso",ylab="Numero di passeggeri",
        FUN="valid.n",shrink=0.15)

par(op)

# Missmaps
op <- par(mfrow = c(1, 2))

missmap(titanic.train, main = "Valori non disponibili (Training)")
missmap(titanic.test, main = "Valori non disponibili (Test)")

par(op)

op <- par(mfrow = c(1, 2))

boxplot(Age ~ Title, titanic.train, main="Title su Age (Training)")
boxplot(Age ~ Title, titanic.test, main="Title su Age (Test)")

par(op)

require("ggplot2")
ggplot(titanic.train,aes(x = log(Fare),fill = Survived)) + 
  geom_bar(position = "fill")


Fn <- ecdf(log(titanic.train$Fare+1)) 
plot(Fn, main="Fare Proportions", xlab="Fare (log)", ylab="Cumulative proportions", cex=0.5, col=as.integer(titanic.train$Survived))

op <- par(mfrow = c(1, 2))

titanic.train.fareprop.female <- titanic.train[titanic.train$Sex=="F", c(2,10)]
titanic.train.fareprop.female$Fare <- round(log10(titanic.train.fareprop.female$Fare + 1), digits=1)

tab.female <- prop.table(table(titanic.train.fareprop.female[, c(1,2)]), 2)*100
plot(as.numeric(dimnames(tab.female)$Fare), tab.female[2,], xlab = "Fare (Log)", ylab = "Female Survived [%]", cex=.6)

titanic.train.fareprop.male <- titanic.train[titanic.train$Sex=="M", c(2,10)]
titanic.train.fareprop.male$Fare <- round(log10(titanic.train.fareprop.male$Fare + 1), digits=1)

tab.male <- prop.table(table(titanic.train.fareprop.male[, c(1,2)]), 2)*100
plot(as.numeric(dimnames(tab.male)$Fare), tab.male[2,], xlab = "Fare (Log)", ylab = "Male Survived [%]", cex=.6)

par(op)

plot(as.numeric(dimnames(tab)$Fare), tab[2,], xlab = "Fare (Log)", ylab = "Survived [%]", cex=.6)

# Feature plot
featurePlot(x = titanic.train[,6:8], y = titanic.train$Survived, plot="ellipse", auto.key = list(columns = 3)) 