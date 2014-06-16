# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
setwd("~/Vijay/Coursera/RepData/")
mydata <- read.csv(unz("repdata-data-activity.zip", "activity.csv"), header = TRUE, 
    sep = ",")
```


## What is mean total number of steps taken per day?

```r
hist(tapply(mydata$steps, mydata$date, sum), xlab = "Total daily steps", breaks = 20, 
    main = "Total of steps taken per day")
```

![plot of chunk histogram](figure/histogram.png) 



```r
# Calculate mean and median total number of steps taken per day
total.steps <- as.numeric(tapply(mydata$steps, mydata$date, sum))
# mean number of steps taken per day
mean(total.steps, na.rm = TRUE)
```

```
## [1] 10766
```

```r
# median number of steps taken per day
median(total.steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
step.interval <- aggregate(steps ~ interval, data = mydata, mean, na.rm = TRUE)
plot(steps ~ interval, data = step.interval, type = "l", main = "Average daily steps 5-minute interval", 
    ylab = "Average steps", xlab = "Time of day")
```

![plot of chunk dailypattern](figure/dailypattern.png) 

```r

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
step.interval[which.max(step.interval$steps), ]$interval
```

```
## [1] 835
```


## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(mydata$steps))
```

```
## [1] 2304
```


- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
fn <- function(interval) {
    step.interval[step.interval$interval == interval, ]$steps
}
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newdata <- mydata  # Make a new dataset with the original data
count <- 0  # Count the number of data filled in
for (i in 1:nrow(newdata)) {
    if (is.na(newdata[i, ]$steps)) {
        newdata[i, ]$steps <- fn(newdata[i, ]$interval)
        count = count + 1
    }
}
print(paste("Total", count, "NA values were filled"))
```

```
## [1] "Total 2304 NA values were filled"
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps.new <- aggregate(steps ~ date, data = newdata, sum)
hist(steps.new$steps, xlab = "Total daily steps", main = "Total of steps taken per day")
```

![plot of chunk histo](figure/histo.png) 

```r
mean(steps.new$steps)  # mean
```

```
## [1] 10766
```

```r
median(steps.new$steps)  # median
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?
- For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
newdata$day <- ifelse(as.POSIXlt(as.Date(newdata$date))$wday%%6 == 0, "weekend", 
    "weekday")
newdata$day <- factor(newdata$day, levels = c("weekend", "weekday"))
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
stepInterval.new <- aggregate(steps ~ interval + day, newdata, mean)
require(lattice)
```

```
## Loading required package: lattice
```

```r
xyplot(steps ~ interval | factor(day), data = stepInterval.new, aspect = 1/2, 
    type = "l")
```

![plot of chunk pplot](figure/pplot.png) 

