---
title: "Reproducible Research: Peer Assessment 1"
Author: Yogesh Funde
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
if(!file.exists("data")) {
  dir.create("data")
}
temp <- tempfile()   # creating a temporary files
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)

unzip(temp, exdir="./data")
unlink(temp) #deleting a temporary file
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
df <- read.csv("./data/activity.csv")
dfActivity <- na.omit(df)
dfsteps <- aggregate(dfActivity$steps, by=list(dfActivity$date), sum)
names(dfsteps) <- c("Date", "Total_Steps")
hist(dfsteps$Total_Steps, main = "Total Steps per Day", col = "salmon")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2. Calculate and report the mean and median total number of steps taken
per day


```r
paste0("Average number of Steps are: " ,mean(dfsteps$Total_Steps))
```

```
## [1] "Average number of Steps are: 10766.1886792453"
```

```r
paste0("The median steps are : ", median(dfsteps$Total_Steps))
```

```
## [1] "The median steps are : 10765"
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)


```r
dfstepsitervals <- aggregate(dfActivity$steps, by=list(dfActivity$interval), mean)
names(dfstepsitervals) <- c("Interval", "Average_steps")
plot(dfstepsitervals$Average_steps, type="l", xlab="Intervals", ylab="Mean Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

```r
paste("The 5 minute interval with maximum average number of steps across days is", dfstepsitervals$Interval[which.max(dfstepsitervals$Average_steps)])
```

```
## [1] "The 5 minute interval with maximum average number of steps across days is 835"
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

```r
paste("The number of missing cases are:",sum(is.na(df)))
```

```
## [1] "The number of missing cases are: 2304"
```

2. Devise a strategy for filling in all of the missing values in the dataset. 
I have used the mean of steps for a 5-minute interval to replace a missing value.
See below the first 10 rows of original data and imputed data.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
dfcomplete <- df %>% 
              group_by(interval) %>% 
              mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
head(df,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
head(dfcomplete, 10)
```

```
## # A tibble: 10 x 3
## # Groups:   interval [10]
##     steps date       interval
##     <dbl> <chr>         <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
```

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.

A new dataset dfcomplete is created in the above step. We will check dimensions of both the datasets and first few rows to verify.


```r
dim(df)
```

```
## [1] 17568     3
```

```r
head(df, 5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

```r
dim(dfcomplete)
```

```
## [1] 17568     3
```

```r
head(dfcomplete, 5)
```

```
## # A tibble: 5 x 3
## # Groups:   interval [5]
##    steps date       interval
##    <dbl> <chr>         <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
```

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day.
Do these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total
daily number of steps?

```r
dfsteps <- aggregate(dfcomplete$steps, by=list(dfcomplete$date), sum)
names(dfsteps) <- c("Date", "Total_Steps")
hist(dfsteps$Total_Steps, main = "Total Steps per Day", col = "salmon")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
paste0("Average number of Steps are: " ,mean(dfsteps$Total_Steps))
```

```
## [1] "Average number of Steps are: 10766.1886792453"
```

```r
paste0("The median steps are : ", median(dfsteps$Total_Steps))
```

```
## [1] "The median steps are : 10766.1886792453"
```

As can be seen above, estimates of mean and median have remain unchanged post imputation.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.

```r
Wkday <- ifelse(weekdays(as.Date(dfcomplete$date)) %in% c("Saturday", "Sunday"), "WE", "WD")
wday <- factor(Wkday, levels = c("WD", "WE"))
ungroup(dfcomplete)
```

```
## # A tibble: 17,568 x 3
##     steps date       interval
##     <dbl> <chr>         <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
## # ... with 17,558 more rows
```

```r
dfcomplete$wday <- wday
head(dfcomplete, 10)
```

```
## # A tibble: 10 x 4
## # Groups:   interval [10]
##     steps date       interval wday 
##     <dbl> <chr>         <int> <fct>
##  1 1.72   2012-10-01        0 WD   
##  2 0.340  2012-10-01        5 WD   
##  3 0.132  2012-10-01       10 WD   
##  4 0.151  2012-10-01       15 WD   
##  5 0.0755 2012-10-01       20 WD   
##  6 2.09   2012-10-01       25 WD   
##  7 0.528  2012-10-01       30 WD   
##  8 0.868  2012-10-01       35 WD   
##  9 0      2012-10-01       40 WD   
## 10 1.47   2012-10-01       45 WD
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis). 


```r
library(lattice)
dftimeseries <- dfcomplete %>% select(interval, wday, steps) %>% group_by(interval, wday) %>% summarise_all(mean)
xyplot(dftimeseries$steps~dftimeseries$interval | dftimeseries$wday, type="l", xlab="Interval", ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->





