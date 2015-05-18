library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

trainfile <- "./data/trainSplit.csv"

dfT <- read.csv(trainfile)

dfT <- ( dfT
		 %>% mutate(one=1, 
		 		   dt = ymd_hms(as.character(datetime)), 
		 		   hour = hour(dt), # hour of day must be important?
				   hour_minus4=hour(dt - dhours(4)), 
				   hour_minus8=hour(dt - dhours(8)), 
				   wday=wday(datetime), # week day also important? 
				   workinghour = ifelse( workingday==0, 0, ifelse( (0 <= hour_minus8) & (hour_minus8 <= 9), 2, 1) )
		 )
)
#dfT[dfT$weather==4,] # one rainy hour
#dfT <- dfT[-4497,]
#dfT[dfT$weather==4,] # one rainy hour

group_hour <- group_by(dfT, hour, workingday)
sum_hour <- summarize(group_hour, avgRentals = mean(count), std = sd(count))
g <- ggplot(sum_hour, aes(hour, avgRentals, std)) + geom_point() + geom_line() + facet_grid(. ~ workingday) + geom_smooth(method = "lm", formula="y~poly(x,6)"); g

# found peaks at 8:00, 12:00, and 17:00
group_hour <- group_by(dfT, hour_minus8, workingday)
sum_hour <- summarize(group_hour, avgRentals = mean(count), std = sd(count))
g <- ggplot(sum_hour, aes(hour_minus8, avgRentals, std)) + geom_point() + geom_line() + facet_grid(. ~ workingday) + geom_smooth(method = "lm", formula="y~poly(x,6)"); g

#group by workinghour
group_hour <- group_by(dfT, hour_minus8, workinghour)
sum_hour <- summarize(group_hour, avgRentals = mean(count), std = sd(count))
g <- ggplot(sum_hour, aes(hour_minus8, avgRentals, std)) + geom_point() + geom_line() + facet_grid(. ~ workinghour) + geom_smooth(method = "lm", formula="y~poly(x,6)"); g



g <- ggplot(dfT, aes(hour, count)) + geom_point(alpha=.1); g
g <- ggplot(sum_hour, aes(hour, avgRentals)) + geom_point(alpha=.1) + facet_grid(. ~ season); g
g <- ggplot(sum_hour, aes(hour, avgRentals)) + geom_point(alpha=.1) + facet_grid(. ~ weather); g
g <- ggplot(sum_hour, aes(hour, avgRentals)) + geom_point(alpha=.1) + facet_grid(. ~ wday) + geom_smooth(method = "lm", formula="y~poly(x,6)"); g 
g <- ggplot(sum_hour, aes(hour, avgRentals)) + geom_point(alpha=.1) + facet_grid(. ~ wday) + geom_smooth(method = "loess"); g 
# weekdays have peaks 8 & 17; weekends have peak ~12

group_hour_minus4 <- group_by(dfT, hour_minus4, season, weather, workingday, wday)
sum_hour_minus4 <- summarize(group_hour_minus4, avgRentals = mean(count))
g <- ggplot(sum_hour_minus4, aes(hour_minus4, avgRentals)) + geom_point(alpha=.1) + geom_smooth(method = "lm", formula="y~poly(x,6)"); g
g <- ggplot(sum_hour_minus4, aes(hour_minus4, avgRentals)) + geom_point(alpha=.1) + facet_grid(. ~ workingday) + geom_smooth(method = "lm", formula="y~poly(x,6)"); g 
g <- ggplot(sum_hour_minus4, aes(hour_minus4, avgRentals)) + geom_point(alpha=.1) + facet_grid(. ~ workingday) + geom_smooth(method = "loess"); g 
g <- ggplot(sum_hour_minus4, aes(hour_minus4, avgRentals)) + geom_point(alpha=.1) + facet_grid(. ~ wday) + geom_smooth(method = "lm", formula="y~poly(x,6)"); g 
# TODO: capture the 8 & 17 peaks. Avoid having negative rental!

group_hour_minus3 <- group_by(dfT, hour_minus3)
sum_hour_minus3 <- summarize(group_hour_minus3, avgRentals = mean(count))
g <- ggplot(sum_hour_minus3, aes(hour_minus3, avgRentals)) + geom_point() + geom_line() + geom_smooth(method = "lm", formula="y~poly(x,6)"); g

group_weather <- group_by(dfT, weather)
sum_weather <- summarize(group_weather, avgRentals = mean(count), weatherDays = sum(one))
g <- ggplot(sum_weather, aes(weather, avgRentals)) + geom_bar(stat="identity"); g
g2 <- ggplot(sum_weather, aes(weather, weatherDays)) + geom_line(); g2
g<- ggplot(dfT, aes(weather, count)) + geom_point(alpha=.1); g

group_season_weather <- group_by(dfT, season, weather)
sum_season_weather <- summarize(group_season_weather, avgRentals = mean(count), weatherDays = sum(one))
g <- ggplot(sum_season_weather, aes(weather, avgRentals)) + geom_line() + facet_grid(. ~ season); g
g2 <- ggplot(sum_season_weather, aes(weather, weatherDays)) + geom_line() + facet_grid(. ~ season); g2
g<- ggplot(dfT, aes(weather, count)) + geom_point(alpha=.1) + facet_grid(. ~ season); g
# one rainy day outlier

group_temp <- group_by(dfT, temp)
sum_temp <- summarize(group_temp, avgRentals = mean(count), tempDays = sum(one))
g1 <- ggplot(sum_temp, aes(temp, avgRentals)) + geom_line() + geom_smooth(method = "lm");
g2 <- ggplot(sum_temp, aes(temp, tempDays)) + geom_line();
grid.arrange(g1, g2, ncol=2)
g<- ggplot(dfT, aes(temp, count)) + geom_point(alpha=.1); g

group_atemp <- group_by(dfT, atemp)
sum_atemp <- summarize(group_atemp, avgRentals = mean(count), atempDays = sum(one))
g1 <- ggplot(sum_atemp, aes(atemp, avgRentals)) + geom_line() + geom_smooth(method = "lm");
g2 <- ggplot(sum_atemp, aes(atemp, atempDays)) + geom_line();
grid.arrange(g1, g2, ncol=2)
g<- ggplot(dfT, aes(atemp, count)) + geom_point(alpha=.1); g

group_humidity <- group_by(dfT, humidity)
sum_humidity <- summarize(group_humidity, avgRentals = mean(count), humidityDays = sum(one))
g1 <- ggplot(sum_humidity, aes(humidity, avgRentals)) + geom_line() + geom_smooth(method = "lm");
g2 <- ggplot(sum_humidity, aes(humidity, humidityDays)) + geom_line();
grid.arrange(g1, g2, ncol=2)
g<- ggplot(dfT, aes(humidity, count)) + geom_point(alpha=.1); g

group_windspeed <- group_by(dfT, windspeed)
sum_windspeed <- summarize(group_windspeed, avgRentals = mean(count), windspeedDays = sum(one))
g1 <- ggplot(sum_windspeed, aes(windspeed, avgRentals)) + geom_line() + geom_smooth(method = "lm");
g2 <- ggplot(sum_windspeed, aes(windspeed, windspeedDays)) + geom_line();
grid.arrange(g1, g2, ncol=2)
g<- ggplot(dfT, aes(windspeed, count)) + geom_point(alpha=.1); g

group_holiday <- group_by(dfT, holiday)
sum_holiday <- summarize(group_holiday, avgRentals = mean(count))
g <- ggplot(sum_holiday, aes(holiday, avgRentals)) + geom_bar(stat="identity"); g
# can drop holidays? especially because there are so few of them (3%)
# need to figure out how to perform sub-samples that preserves a whole day

group_workingday <- group_by(dfT, workingday)
sum_workingday <- summarize(group_workingday, avgRentals = mean(count))
g <- ggplot(sum_workingday, aes(workingday, avgRentals)) + geom_line() + geom_smooth(method = "lm"); g

group_wday <- group_by(dfT, wday)
sum_wday <- summarize(group_wday, avgRentals = mean(count), typeDays = sum(one))
g <- ggplot(sum_wday, aes(wday, avgRentals)) + geom_point() + geom_smooth(method = "lm"); g
g <- ggplot(dfT, aes(wday, count)) + geom_point(alpha=.1) + geom_smooth(method = "lm"); g

# TODO: how to constrain prediction to be positive.
featurePlot(x=dfT[, c("hour", "workingday", "temp")], y=dfT$count, plot="pairs")
