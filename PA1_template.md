# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First, unzip and read the CSV into a raw dataframe:


```r
zipfile <- "activity.zip"
if(!file.exists(zipfile)) {
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, destfile = zipfile, method = "curl", quiet = TRUE)
}
unzip(zipfile)

csvfile <- "activity.csv"
if(!file.exists(csvfile)) { stop("Expected activity.csv") }

activityData <- read.csv(csvfile)
```


## What is mean total number of steps taken per day?

First we need to get transform the data to aggregate steps by date:


```r
sumByDate <- function(data, column) {
  dates <- split(data, data$date)
  sapply(dates, function(byDate) {
    sum(byDate[[column]])
  })
}
summedByDate <- sumByDate(activityData, "steps")
```

We can begin to understand the shape of the data with a simple histogram:


```r
hist(summedByDate)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The mean number of steps taken per day:


```r
mean(summedByDate, na.rm = TRUE)
```

```
## [1] 10766
```

And the median:


```r
median(summedByDate, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

First we need to aggregate the data by mean for each interval across all dates:

First we need to get transform the data to aggregate steps per day:


```r
intervals <- split(activityData, activityData$interval)
meanByInterval <- sapply(intervals, function(byInterval) {
  mean(byInterval$steps, na.rm = TRUE)
})
```

Now we can plot the mean across days for each interval:


```r
plot(names(meanByInterval), meanByInterval, type='l', xlab = "Interval", ylab = "Mean")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

The interval with the most steps on average:


```r
as.numeric(names(which.max(meanByInterval)))
```

```
## [1] 835
```

## Imputing missing values

Total number of missing steps observations across all intervals and days:


```r
sum(is.na(activityData))
```

```
## [1] 2304
```

Add a column where NA steps observations are imputed using the mean for the interval of the missing value across all days:


```r
activityData$imputedSteps <-
  with(activityData,
       ifelse(is.na(steps), meanByInterval[as.character(interval)], steps))
```

Get the total steps by date for the imputed dataset, show a histogram and get the mean and median:


```r
imputedSummedByDate <- sumByDate(activityData, "imputedSteps")
hist(imputedSummedByDate)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
mean(imputedSummedByDate)
```

```
## [1] 10766
```

```r
median(imputedSummedByDate)
```

```
## [1] 10766
```

Note that the mean is identical to the estimates from earlier. This is because we are using the mean across days for each interval, and the data set either has days with no NA intervals or days with only NA intervals, which results in the total steps for missing days being exactly equal to the sum of the average steps of each interval, which is equal to the mean of all the day sums. Basically, we've added a bunch of datapoints that are right at the mean, and changed no others, so the mean hasn't moved.

The median has moved and is now equal to the mean, because we've clustered a few more points near the mean, which has evened out the slight skew.

## Are there differences in activity patterns between weekdays and weekends?

Let's add a new variable that differentiates between weekdays and weekends:


```r
dayNames <- weekdays(as.POSIXct(activityData$date))
activityData$dayType <-
  ifelse(dayNames %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')
```


```r
library(plyr)
library(lattice)
meanByIntervalAndDayType <- ddply(activityData, .(interval, dayType), function(group) {
  data.frame(mean = mean(group$steps, na.rm = TRUE))
})
with(meanByIntervalAndDayType,
     xyplot(mean ~ interval | dayType,
            type = 'l',
            xlab = 'Interval',
            ylab = 'Number of steps',
            layout = c(1,2)))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

These plots show that weekdays are more active in the morning, but not by much, and activity remains consistently high throughout the day on weekends. Weekday activity has a small peak later in the afternoon, which is possibly correlated with returning home from work.
