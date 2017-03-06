---
title: "Assignment week 2"
output: html_document
---



## Loading and preprocessing the data

Loading and preprocessing the data R code
###1. Load the data


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "dataset.zip", mode = "wb")
unzip("dataset.zip")
dataset <- read.csv("activity.csv");
```

###2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
filteredDataset <- dataset[complete.cases(dataset),]
```

## What is mean total number of steps taken per day?

###1. Calculate the total number of steps taken per day


```r
stepsPerDay <- tapply(filteredDataset$steps, filteredDataset$date, sum)
stepsPerDay <- stepsPerDay[complete.cases(stepsPerDay)]
```

###2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(stepsPerDay)
```

![plot of chunk histStepsPerDay](figure/histStepsPerDay-1.png)

###3. Calculate and report the mean and median of the total number of steps taken per day

```r
oldMean <- mean(stepsPerDay, na.rm = TRUE)
print("Mean")
```

```
## [1] "Mean"
```

```r
oldMean
```

```
## [1] 10766.19
```

```r
oldMedian <-median(stepsPerDay)
print("Median")
```

```
## [1] "Median"
```

```r
oldMedian
```

```
## [1] 10765
```


## What is the average daily activity pattern?

###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(ggplot2)
stepsByInterval <- tapply(filteredDataset$steps, filteredDataset$interval, mean)
stepsByInterval <- as.data.frame.table(stepsByInterval)
colnames(stepsByInterval)[1] <- "interval"
colnames(stepsByInterval)[2] <- "steps_mean"
ggplot(stepsByInterval, aes(interval, steps_mean)) + geom_point(na.rm=TRUE)
```

![plot of chunk stepPerInterval](figure/stepPerInterval-1.png)

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsByInterval[stepsByInterval[["steps_mean"]] == max(stepsByInterval[["steps_mean"]]), ]
```

```
##     interval steps_mean
## 104      835   206.1698
```

## Imputing missing values

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(dataset[!complete.cases(dataset), ])
```

```
## [1] 2304
```

###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
modifiedSteps <- ifelse(is.na(dataset$steps),stepsPerDay[dataset$date], dataset$steps)
```

Replaced NA by mean of daily steps. 

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
modifiedDataset <- dataset
modifiedDataset$steps <- modifiedSteps
```

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newStepsPerDay <- tapply(filteredDataset$steps, filteredDataset$date, sum)
newStepsPerDay <- newStepsPerDay[complete.cases(newStepsPerDay)]
hist(newStepsPerDay)
```

![plot of chunk strategyAppliedHist](figure/strategyAppliedHist-1.png)


```r
newMean <- mean(newStepsPerDay, na.rm = TRUE)
print("New Mean")
```

```
## [1] "New Mean"
```

```r
newMean
```

```
## [1] 10766.19
```

```r
newMedian <- median(newStepsPerDay)
print("New Median")
```

```
## [1] "New Median"
```

```r
newMedian
```

```
## [1] 10765
```

```r
print("Mean is different?")
```

```
## [1] "Mean is different?"
```

```r
print(newMean != oldMean)
```

```
## [1] FALSE
```

```r
print("Median is different?")
```

```
## [1] "Median is different?"
```

```r
print(newMedian != oldMedian)
```

```
## [1] FALSE
```


##Are there differences in activity patterns between weekdays and weekends?

###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekendDataset <- filteredDataset
isWeekend <- ifelse(weekdays(as.Date(weekendDataset$date)) %in% c("sábado", "domingo"), "weekend", "weekday")
weekendDataset$isWeekend <- isWeekend
```


###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Result for weekend means:

```r
stepsByIntervalAndWeekendFactor <- aggregate(steps ~ interval+isWeekend, data = weekendDataset, mean)
stepsByIntervalWeekend <- stepsByIntervalAndWeekendFactor[stepsByIntervalAndWeekendFactor$isWeekend == "weekend",]
ggplot(stepsByIntervalWeekend, aes(interval, steps)) + geom_point(na.rm=TRUE)
```

![plot of chunk activityPatternsWeekend](figure/activityPatternsWeekend-1.png)

Result for weekday means:

```r
stepsByIntervalAndWeekendFactor <- aggregate(steps ~ interval+isWeekend, data = weekendDataset, mean)
stepsByIntervalWeekday <- stepsByIntervalAndWeekendFactor[stepsByIntervalAndWeekendFactor$isWeekend == "weekday",]
ggplot(stepsByIntervalWeekday, aes(interval, steps)) + geom_point(na.rm=TRUE)
```

![plot of chunk activityPatternsWeekday](figure/activityPatternsWeekday-1.png)
