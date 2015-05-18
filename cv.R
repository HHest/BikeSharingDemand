source("train.R")

#=======================================
cvfile <- "./data/crossValidationSplit.csv"
cv <- read.csv(cvfile)
# Contruct features
cvf <- extractFeatures(cv)
# break up data by working day and working hour
cvdfs <- partitionByDayHourType(cv, cvf)
preds <- mapply(predict, fits, cvdfs )
cvf$pCount <- 0
cvf$pCount[cvf$workingHour==0] <- preds[[1]]
cvf$pCount[cvf$workingHour==1] <- preds[[2]]
cvf$pCount[cvf$workingHour==2] <- preds[[3]]
cv2 <- cv
cv2$pCount <- sapply(sapply(cvf$pCount, max, 0), round)

plot(cv2$count, cv2$pCount)
