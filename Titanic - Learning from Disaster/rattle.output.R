titanic.submit <- read.csv2(file="./output/titanic_score_idents.csv", header=TRUE, sep=",")
titanic.submit <- titanic.submit[,1:2]
write.csv(x=titanic.submit, file="./output/titanic_raw_svm_submission.csv", col.names=TRUE, row.names=FALSE, sep=",", quote=FALSE)
write.table(x=titanic.test[,c(1:2,4:7,9,11,12:15)], file="./data/test.inputed.csv", col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE)
write.table(x=titanic.train[,c(1:3,5:8,12:16)], file="./data/train.inputed.csv", col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE)
