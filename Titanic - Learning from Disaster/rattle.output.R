titanic.submit <- read.csv2(file="./output/titanic_score_idents.csv", header=TRUE, sep=",")
titanic.submit <- titanic.submit[,1:2]
write.csv(x=titanic.submit, file="./output/titanic_raw_svm_submission.csv", col.names=TRUE, row.names=FALSE, sep=",", quote=FALSE)
