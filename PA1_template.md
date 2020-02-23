---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

### Firstly, download the zip file from web link:


```r
# download the dataset from web link
download_data <- function(){      
      file_name <<- "repdata-data-activity.zip"
      if (!file.exists(file_name)) {
            file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
            download.file(file_url, file_name, method = "curl")
      }
}

download_data()
```

### Unzip the zip file:


```r
# unzip the dataset
unzip_data <- function(){
      if (!file.exists("activity.csv")) {
            unzip(file_name)
      }
}

unzip_data()
```

### Load the dataset:


```r
# load the dataset
act_df <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day


```r
steps_per_day <- tapply(act_df$steps, act_df$date, sum, na.rm = TRUE)
steps_per_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```


### If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

**We can use a histogram to plot the number of steps recorded each day:**


```r
qplot(steps_per_day, xlab = "Total Steps Recorded Per Day", ylab = "Frequency", bins = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Calculate and report the mean and median total number of steps taken per day


```r
steps_per_day_mean <- mean(steps_per_day)
steps_per_day_median <- median(steps_per_day)
```
**Mean of steps taken per day is 9354.2295082**

**Median of steps taken per day is 10395**

## What is the average daily activity pattern?


```r
avg_steps_pattern <-  aggregate(x = list(mean_steps = act_df$steps),
                                by = list(interval = act_df$interval),
                                mean, na.rm = TRUE)
```

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ggplot(avg_steps_pattern, aes(interval, mean_steps)) +
      geom_line(size = 1) +
      labs(x = "Counts of 5-Mins Interval", y = "Average Number of Steps") +
      theme_classic()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps <- which.max(avg_steps_pattern$mean_steps)
max_time <- gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avg_steps_pattern[max_steps, 'interval'])
```

**People tend to have most steps at 8:35**

## Imputing missing values

### Calculate and report the total number of missing values in the dataset


```r
num_null <- sum(is.na(act_df$steps))
```

**Number of missing values is 2304**

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**The strategy used for filling in all of missing values in the dataset is to replace the missing values with mean.**

### Create a new dataset that is equal to the original dataset but with the missing data filled in:


```r
imputed_act_df <- act_df
imputed_act_df$steps <- impute(act_df$steps, mean)
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputed_steps_per_day <- tapply(imputed_act_df$steps, imputed_act_df$date, sum)
qplot(imputed_steps_per_day, xlab = "Total Steps Per Day After Imputation", 
      ylab = "Frequency", bins = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
imputed_steps_per_day_mean <- mean(imputed_steps_per_day)
imputed_steps_per_day_median <- median(imputed_steps_per_day)
```

**Mean of steps taken per day after imputation is 1.0766189\times 10^{4}**

**Median of steps taken per day after imputation is 1.0766189\times 10^{4}**

**The histogram is close to normal distribution after imputation, and the mean of steps taken per day is increased after imputation.**

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
imputed_act_df$weekday <- ifelse(as.POSIXlt(imputed_act_df$date)$wday %in% c(0, 6), 
                                 "weekend", "weekday")
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
agg_imputed_act_df <- aggregate(steps ~ interval + weekday, data = imputed_act_df, mean)
ggplot(agg_imputed_act_df, aes(interval, steps)) +
      geom_line(size = 1) +
      facet_grid(weekday ~ .) +
      labs(x = "Counts of 5-Mins Interval", y = "Average Number of Steps") +
      theme_classic()
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

