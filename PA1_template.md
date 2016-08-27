#Project Assignment 1: Reproducible Research
##Logan Cones
Load related packages and read the data

```r
library(knitr)
opts_chunk$set(echo = TRUE)
library(dplyr)
library(lattice)
library(lubridate)
```
##Load & Preprocess data and remove rows with NA values

```r
setwd("/Users/logancones/Desktop/Data_Science")
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
data <- read.csv('activity.csv')
complete <- complete.cases(data)
dataComplete <- data[complete,]
```
##Group and summarize data for the histogram

```r
grouped <- group_by(dataComplete, date)
summarized <- summarise(grouped, steps = sum(steps))
```
##Plot Histogram

```r
hist(summarized$steps, xlab = "Steps", main = "Histogram of Daily Steps Taken")
```

![plot of chunk unnamed-chunk-4](/unnamed-chunk-4-1.png)

##What is mean & median total steps per day?


```r
meanSteps <- mean(summarized$steps)
medianSteps <- median(summarized$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps
```

```
## [1] 10765
```

meanSteps equals mean steps taken per day medianSteps equals median steps per day

##What is average daily activity Pattern
1. Make time series plot of the 5-minute interval and average number of steps taken, averaged across all days
2. Determine which interval on average contains the maximum number of steps

```r
intervalGroup <- group_by(dataComplete, interval)
intervalSummarized <- summarize(intervalGroup, Mean_Steps = mean(steps))
plot(intervalSummarized, type = "l", main = "Mean Steps by Interval")
```

![plot of chunk unnamed-chunk-6](/unnamed-chunk-6-1.png)

```r
Interval <- max(intervalSummarized$Mean_Steps)
filteredData <- filter(intervalSummarized, Mean_Steps == Interval)
maxInterval <- filteredData$interval
maxInterval
```

```
## [1] 835
```
Interval maxInterval contains max number of steps on average across all days 
##Calculate and report number of NA in dataset

```r
missingValues <- sum(is.na(data$steps))
missingValues
```

```
## [1] 2304
```
##Fill in NA with mean number of steps taken for each interval

```r
for(i in 1:nrow(data)){
  if(is.na(data$steps[i])){
    for(j in 1:nrow(intervalSummarized)){
      if(data$interval[i] == intervalSummarized$interval[j]){
        data$steps[i] <- as.integer(intervalSummarized$Mean_Steps[j])
      }
    }
  }
}
```
##Reaggreeate data for new histogram

```r
groupedNew <- group_by(data, date)
summarizedNew <- summarise(groupedNew, steps = sum(steps))
hist(summarizedNew$steps, xlab = "Steps", main = "Histogram of Daily Steps Taken")
```

![plot of chunk unnamed-chunk-9](/unnamed-chunk-9-1.png)
##Calculate new mean and median and difference between new meand and medians compared to original's

```r
newMeanSteps <- mean(summarizedNew$steps)
newMedianSteps <- median(summarizedNew$steps)
meanDifference <- abs(newMeanSteps - meanSteps)
medianDifference <- abs(newMedianSteps - medianSteps)
newMeanSteps
```

```
## [1] 10749.77
```

```r
newMedianSteps
```

```
## [1] 10641
```

```r
meanDifference
```

```
## [1] 16.41819
```

```r
medianDifference
```

```
## [1] 124
```
##Create new factor to determine differences between weekdays and weekends

```r
data$date <- weekdays(as.Date(data$date))
weekday <- c("Monday","Tuesday", "Wednesday", "Thursday", "Friday")
for(i in 1:nrow(data)){
  if(data$date[i] %in% weekday){
    data$date[i] <- "Weekday"
  } else {
    data$date[i] <- "Weekend"
  }
}
```
Finally we're going to make a plot containing a time series plot of the 5-minute interval and the average # of steps taken averages for both weekdays or weekend days on the yaxis

```r
dateintervalGroup <- group_by(data, interval, date)
dateintervalSummarized <- summarize(dateintervalGroup, Mean_Steps = mean(steps))
weekdayData <- filter(dateintervalSummarized, date == "Weekday")
weekdayData <- select(weekdayData, -date)
weekendData <- filter(dateintervalSummarized, date == "Weekend")
weekendData <- select(weekendData, -date)
par(mfrow=c(2,1))
plot(weekendData, type = "l", main = "Mean Steps Taken on Weekends")
plot(weekdayData, type = "l", main = "Mean Steps Taken on Weekdays")
```

![plot of chunk unnamed-chunk-12](/unnamed-chunk-12-1.png)

