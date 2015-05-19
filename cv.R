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
cv$rCount <- sapply(cvf$pCount, round)
cv$pCount <- sapply(sapply(cvf$pCount, max, 0), round)
cvError <- sum( (cv$pCount - cv$count)^2 )/length(cv$pCount)/2
cvRMSLE <- sqrt( sum( (log(cv$pCount+1) - log(cv$count+1) )^2 )/length(cv$pCount) )

if (exists("errors")) {
	errors <- unique( bind_rows(errors, 
								data.frame(method=method, nPolyWind=nPolyWind, nPolyHour=nPolyHour, Err=trainError, RMSLE=trainRMSLE, type="train"),	
								data.frame(method=method, nPolyWind=nPolyWind, nPolyHour=nPolyHour, Err=cvError, RMSLE=cvRMSLE, type="cv")	
	) )
} else {
	errors <- bind_rows(
		data.frame(method=method, nPolyWind=nPolyWind, nPolyHour=nPolyHour, Err=trainError, RMSLE=trainRMSLE, type="train"),	
		data.frame(method=method, nPolyWind=nPolyWind, nPolyHour=nPolyHour, Err=cvError, RMSLE=cvRMSLE, type="cv")	
	)
}

g <- ggplot(errors, aes(nPolyHour, Err, colour=type)) + geom_line() + geom_point() + facet_grid(method ~ nPolyWind, drop=F); g

