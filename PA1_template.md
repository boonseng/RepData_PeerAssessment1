#Reproducible Research Peer Assessment 1  
  
##Loading and preprocessing the data  
  
####Show any code that is needed to:

1. Load the data (i.e. read.csv())

```r
acts = read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
acts [,2] = as.Date(acts[,2], "%Y-%m-%d")
```
  
##What is mean total number of steps taken per day?  
  
####Make a histogram of the total number of steps taken each day  

```r
require(plyr)
options(scipen=999)
days = ddply(acts,.(date),summarise,steps = sum(steps))
plot (days, type="h")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
  
####Calculate and report the mean and median total number of steps taken per day  

The mean total number of steps taken per day is 10766.19.

```r
round(mean(days[,2], na.rm=TRUE), 2)
```

```
## [1] 10766.19
```

The median total number of steps taken per day is 10765.

```r
median(days[,2], na.rm=TRUE)
```

```
## [1] 10765
```
  
##What is the average daily activity pattern?  

####Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
times = ddply(acts,.(interval),summarize,steps=mean(steps, na.rm=TRUE))
plot (times, type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
  
####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  
The 835th 5-minute interval contains the maximum number of steps on average across all the days in the dataset.

```r
times[which.max(times[,2]),]$interval
```

```
## [1] 835
```
  
##Imputing missing values  

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
  
####Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(acts$steps))
```

```
## [1] 2304
```

####Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
I will be using the mean for that 5-minute interval to fill in the missing values in the dataset.  

####Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
acts2 <- ddply(acts, .(interval), function(acts) {acts$steps[is.na(acts$steps)]=times[which(times$interval == acts$interval),]$steps; return(acts)})
```
  
####Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
days2 = ddply(acts2,.(date),summarise,steps = sum(steps))
plot (days2, type="h")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
  
Aftering imputing missing data, the mean total number of steps taken per day is 10766.19.

```r
round(mean(days2[,2], na.rm=TRUE), 2)
```

```
## [1] 10766.19
```
  
Aftering imputing missing data, the median total number of steps taken per day is 10766.19.

```r
round(median(days2[,2], na.rm=TRUE), 2)
```

```
## [1] 10766.19
```
####Do these values differ from the estimates from the first part of the assignment? 
Yes.  
####What is the impact of imputing missing data on the estimates of the total daily number of steps?
While the average or mean remains largely the same, the total number of steps would have increased.


##Are there differences in activity patterns between weekdays and weekends?
  
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
acts3 = cbind (acts2, weekdays(acts2[,2]) %in% c('Sunday','Saturday'))
colnames(acts3) = c("steps", "date", "interval", "weekend")
times2 = ddply(acts3,.(weekend, interval),summarize,steps=mean(steps, na.rm=TRUE))
weekends = times2 [which(times2$weekend),]
weekdays = times2 [which(!times2$weekend),]
par(mfrow=c(2,1))
plot (weekdays$interval, weekdays$steps, type="l")
plot (weekends$interval, weekends$steps, type="l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
