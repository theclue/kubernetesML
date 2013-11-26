# Data breakdown

op <- par(mfrow = c(2, 1))

barNest(Survived~Age.factor+Sex+Pclass,titanic.train,col=titanic.colors,showall=TRUE,
        main="Sopravvissuti divisi per classe, età, sesso",ylab="Proporzione sopravvissuti",
        FUN="propbrk",shrink=0.15,trueval="Si")

barNest(Survived~Age.factor+Sex+Pclass,titanic.train,col=titanic.colors,showall=TRUE,
        main="Passeggeri divisi per classe, età, sesso",ylab="Numero di passeggeri",
        FUN="valid.n",shrink=0.15)

par(op)