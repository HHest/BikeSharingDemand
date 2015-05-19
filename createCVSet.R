library(lubridate)
library(dplyr)

trainfile <- "./data/train.csv"
dfFull <- read.csv(trainfile)
dfCV <- dfFull[day(dfFull$datetime)>15,]
write.csv(dfCV, "./data/crossValidationSplit.csv", row.names=F)
dfTrain <- dfFull[day(dfFull$datetime)<=15,]
write.csv(dfTrain, "./data/trainSplit.csv", row.names=F)

