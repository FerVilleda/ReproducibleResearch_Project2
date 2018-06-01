## 1. Leer los datos
library(stringi)
library (lattice)
library (survival)
library (Formula) 
library (ggplot2)
library(Hmisc)


stepsData <- read.csv("activity.csv", header = TRUE)
stepsData$date <- as.Date(stepsData$date)

## 2. Histogram of the total number of steps taken each day
TotalSteps <- tapply(stepsData$steps, stepsData$date, FUN = sum, na.rm = TRUE)
qplot(TotalSteps, binwidth = 1000, xlab = "total number of steps taken each day")
file.create("plot1.png")
ggsave("plot1.png")

## 3. Mean and median number of steps taken each day
mean(TotalSteps, na.rm = TRUE)
median(TotalSteps, na.rm = TRUE)

##4.Time series plot of the average number of steps taken
meansteps <- tapply(stepsData$steps, stepsData$interval, mean, na.rm = TRUE)
png("plot2.png", width=480, height=480)
plot(meansteps, type="l", col="hotpink", xlab="5-minute intervals", ylab="Average Steps", main = "Average daily activity pattern")
dev.off()

## 5. The 5-minute interval that, on average, contains the maximum number of steps
meansteps <- as.data.frame.table(meansteps)
names(meansteps) <- c("interval","avsteps")
meansteps[which.max(meansteps$avsteps),]

##6. Code to describe and show a strategy for imputing missing data
missingSteps <- is.na(stepsData$steps)
table(missingSteps)
imputeStepsData <- stepsData
imputeStepsData$steps <- impute(imputeStepsData$steps, fun=mean)
imputedTotalSteps <- tapply(imputeStepsData$steps, imputeStepsData$date, sum)

##7. Histogram of the total number of steps taken each day after missing values are imputed
qplot(imputedTotalSteps,binwidth=500, xlab = "Total steps per day", ylab = "Frequency")
file.create("plot3.png")
ggsave("plot3.png")

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
WeekDayOrEnd <- function(date){
  daytype <- weekdays(date)
  if (daytype %in% c("lunes", "martes", "miércoles", "jueves", "viernes")) {
    return("Weekday")
  }else if (daytype %in% c("sábado", "domingo")) {
    return("Weekend")
  }else{return("Invalid date")}
}

imputeStepsData$dayType <- sapply(imputeStepsData$date, WeekDayOrEnd)
averages <- aggregate(steps ~ interval + dayType, data = imputeStepsData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(dayType ~ .) + xlab("5-minute interval") + ylab("Total steps")
file.create("plot4.png")
ggsave("plot4.png")
