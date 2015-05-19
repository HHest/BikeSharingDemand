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

doTraining <- function(df, method, nPolyWind, nPolyHour) {
	if (method %in% c("rf", "rf_submit")) {
		# RandomForest: set trainControl to take every 24 hours
		fitControl <- trainControl(method="timeslice", initialWindow=5*24, horizon=24, fixedWindow=T)
		return( train(count ~ ., data=df, method="rf", trControl=fitControl, preProcess=c("center", "scale") ) )
	}
	if (method %in% c("rf_poly", "rf_poly_submit")) {
		# RandomForest: set trainControl to take every 24 hours
		fitControl <- trainControl(method="timeslice", initialWindow=5*24, horizon=24, fixedWindow=T)
		return( train(
			count ~ I(1*(season==1)) + I(1*(season==2)) + I(1*(season==3)) + I(1*(season==4)) 
			+ I(1*(weather==1)) + I(1*(weather==2)) + I(1*(weather==3)) + I(1*(weather==4))
			+ temp + atemp + humidity + poly(windspeed, nPolyWind) + poly(hourAdj, nPolyHour), 
			data=df, method="rf", trControl=fitControl, preProcess=c("center", "scale") ) )
	}
	if (method=="lm") {
		# Linear model:
		return( train(
			count ~ I(1*(season==1)) + I(1*(season==2)) + I(1*(season==3)) + I(1*(season==4)) 
			+ I(1*(weather==1)) + I(1*(weather==2)) + I(1*(weather==3)) + I(1*(weather==4))
			+ temp + atemp + humidity + poly(windspeed, nPolyWind) + poly(hourAdj, nPolyHour), 
			data=df, method="lm", preProcess=c("center", "scale")
		) )
	}
	if (method=="glm") {
		# general linear model using poisson for count data
		return ( train(
			count ~ I(1*(season==1)) + I(1*(season==2)) + I(1*(season==3)) + I(1*(season==4)) 
			+ I(1*(weather==1)) + I(1*(weather==2)) + I(1*(weather==3)) + I(1*(weather==4))
			+ temp + atemp + humidity + poly(windspeed, nPolyWind) + poly(hourAdj, nPolyHour), 
			data=df, method="glm", family="poisson", preProcess=c("center", "scale")
		) )
#		return( train(
#			count ~ season + weather + temp + atemp + humidity + poly(windspeed, nPolyWind) + poly(hourAdj, nPolyHour), 
#			data=df, method="glm", family="poisson", preProcess=c("center", "scale")
#		) )		
	}
}

#=======================================
method <- "rf_poly"
nPolyWind <- 2
nPolyHour <- 1

trainfile <- "./data/trainSplit.csv"
train <- read.csv(trainfile)
print(train[train$weather==4,])
train <- train[-4497,]
if (method %in% c("rf_submit", "rf_poly_submit")) {
	trainfile <- "./data/train.csv"
 	train <- read.csv(trainfile)	
}

# Contruct features
features <- extractFeatures(train)
# break up data by working day and working hour
dfs <- partitionByDayHourType(train, features)

fits <- lapply(dfs, doTraining, method, nPolyWind, nPolyHour)

print(fits[[1]]$result)
print(fits[[2]]$result)
print(fits[[3]]$result)

#plot(fits[[1]]$finalModel)

# collect error statistics
preds <- mapply(predict, fits, dfs )
features$pCount <- 0
features$pCount[features$workingHour==0] <- preds[[1]]
features$pCount[features$workingHour==1] <- preds[[2]]
features$pCount[features$workingHour==2] <- preds[[3]]
train$pCount <- sapply(sapply(features$pCount, max, 0), round)
trainError <- sum( (train$pCount - train$count)^2 )/length(train$pCount)/2	
trainRMSLE <- sqrt( sum( (log(train$pCount+1) - log(train$count+1) )^2 )/length(train$pCount) )

plot(train$count, train$pCount)


