library(lubridate)
library(dplyr)
library(caret)
library(doMC)
registerDoMC(cores=3)

set.seed(8252359)

extractFeatures <- function(df) {
	# grab hour of day & shift by -8 hours to 
	df <- mutate(df, 
				 hourAdj = hour(ymd_hms(as.character(df$datetime)) - dhours(8)),
				 # Break up data by workingday, and when workingday = 1 break
				 # by hourAdj = [0, 9] and [10, 23] => workinghours = 1
				 # i.e. break by hour = [8, 17] and [18, 7] => workinghours = 0
				 workingHour = ifelse( 
				 	workingday==0, 
				 	0, # not a working day at all
				 	ifelse( 
				 		(0 <= hourAdj) & (hourAdj <= 9), 
				 		2, # working hours of working day 
				 		1  # non-working hours of working day
				 	) 
				 )
	)
	features <- c("season",
				  #"holiday", # not enough holidays, which causes zero variance issues
				  # also have seen via plots that effect is small
				  "workingday",
				  "workingHour",
				  "weather",
				  "temp",
				  "atemp",
				  "humidity",
				  "windspeed",
				  "hourAdj"
	)
	df[, features]
}

partitionByDayHourType <- function(df, features, hasCount=TRUE) {
	if (hasCount) {
		df <- bind_cols(features, select(df, count))
	} else {
		df <- features
	}
	lapply(c(0,1,2), filterWorkingHour, df)
}

filterWorkingHour <- function(code, df) {
	# filter out the desired working hour, and throw away the redundant 
	# workingHour and workingday columns.
	df %>% filter(workingHour==code) %>% select(-workingHour, -workingday)
}

getScalings <- function(dfs) {
	nCol = length(dfs[[1]])
	reducedDfs <- lapply(dfs, select, temp:(nCol-1))
	lapply(reducedDfs, preProcess, method=c("center", "scale"))	
}

doTraining <- function(df, nPoly) {
	# RandomForest: set trainControl to take every 24 hours
	fitControl <- trainControl(method="timeslice", initialWindow=5*24, horizon=24, fixedWindow=T)
	fit <- train(count ~ ., data=df, method="rf", trControl=fitControl, preProcess=c("center", "scale") )
	
	# Linear model:
	#fit <- train(count ~ ., data=df, method="lm", preProcess=c("center", "scale") )
#	fit <- train(
#		count ~ I(1*(season==1)) + I(1*(season==2)) + I(1*(season==3)) + I(1*(season==4)) 
#			+ I(1*(weather==1))+ I(1*(weather==2))+ I(1*(weather==3))+ I(1*(weather==4))
#			+ temp + atemp + humidity + windspeed + poly(hourAdj, nPoly), 
#		data=df, method="lm", preProcess=c("center", "scale")
#	)	
}

#=======================================
trainfile <- "./data/trainSplit.csv"
train <- read.csv(trainfile)
# Contruct features
features <- extractFeatures(train)
# break up data by working day and working hour
dfs <- partitionByDayHourType(train, features)
fits <- lapply(dfs, doTraining, 6)
summary(fits[[1]])
summary(fits[[2]])
summary(fits[[3]])
#plot(fits[[1]]$finalModel)


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
