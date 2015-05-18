source("train.R")

#=======================================
testfile <- "./data/test.csv"
test <- read.csv(testfile)
# Contruct features
testf <- extractFeatures(test)
# break up data by working day and working hour
testdfs <- partitionByDayHourType(test, testf, hasCount=F)
preds <- mapply(predict, fits, testdfs )
testf$pCount <- 0
testf$pCount[testf$workingHour==0] <- preds[[1]]
testf$pCount[testf$workingHour==1] <- preds[[2]]
testf$pCount[testf$workingHour==2] <- preds[[3]]
submission <- data.frame(datetime = test$datetime, count = sapply(sapply(testf$pCount, max, 0), round) )
write.csv(submission, file="./data/submission.csv", row.names=F, quote=F)
