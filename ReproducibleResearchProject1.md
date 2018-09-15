---
Author: Ann Bowers
Date: September 14, 2018
Title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---



#Reproducible Research Project 1

##Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: Activity monitoring data

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Loading and preprocessing the data

Unzip the data file and save it into the 'ActivityData' variable and allow the date to read in as a string


```r
library(dplyr)
library(ggplot2)
unzip("activity.zip")
ActivityData<-read.csv("activity.csv", header=T, sep=",", stringsAsFactors=F)
```

Convert the date to the correct format and look at the structure of the data table to make sure it matches the information given in the directions


```r
ActivityData$date<-as.Date(ActivityData$date, "%Y-%m-%d")
str(ActivityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##Question 1: What is the mean total number of steps taken per day?
Ignore the missing values in the dataset
  
####1. Calculate the total number of steps taken per day


```r
DailySteps<-aggregate(steps~date, ActivityData, sum)
head(DailySteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

####2. Make a histogram of the total number of steps taken each day


```r
ggplot(DailySteps, aes(x=steps)) +
    geom_histogram(col="blue",fill="green") +
    labs(x="Steps Logged",y="Count of Days",title="Histogram of Total Steps Taken Each Day")
```

![](ReproducibleResearchProject1_files/figure-html/hist_steps-1.png)<!-- -->

####3. Calculate and report the mean and median of the total number of steps taken per day


```r
MeanSteps<-mean(DailySteps$steps)
MedianSteps<-median(DailySteps$steps)
print(paste("The mean number of steps is ",round(MeanSteps,2), " and the median number of steps is ",MedianSteps,".",sep=""), quote=F)
```

```
## [1] The mean number of steps is 10766.19 and the median number of steps is 10765.
```

##Question 2: What is the average daily activity pattern?
What is the average daily activity pattern?

####1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
IntervalSteps<-aggregate(steps~interval, ActivityData, mean)
ggplot(IntervalSteps, aes(x=interval, y=steps)) +
  geom_line(col="blue") +
  labs(x="5-minute Interval", y="Mean number of steps", title="Time Series of Average Steps Taken")
```

![](ReproducibleResearchProject1_files/figure-html/time_series-1.png)<!-- -->

####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
head(IntervalSteps)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
MaxInterval<-IntervalSteps[which.max(IntervalSteps$steps),]
print(paste("The most steps taken on average in a 5-minute interval is ", round(MaxInterval$steps,2), " and this occurs in interval number ", MaxInterval$interval,".", sep=""), quote=F, width=25)
```

```
## [1] The most steps taken on average in a 5-minute interval is 206.17 and this occurs in interval number 835.
```

##Question 3: Imputing missing values
Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

####1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
print(paste("The number of rows missing data is ",sum(!complete.cases(ActivityData)),".",sep=""),quote=F)
```

```
## [1] The number of rows missing data is 2304.
```

####2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

  Since data varies so significantly over 5-minute intervals, I will be using the mean of the interval to fill in any   missing data
  
####3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
CleanActData<-ActivityData
for(i in 1:nrow(CleanActData)){
    if(is.na(CleanActData$steps[i])) {
      IntervalNum<-CleanActData$interval[i]
      IntervalInfo<-subset(IntervalSteps, interval==IntervalNum)
      CleanActData$steps[i]<-IntervalInfo$steps
    }
}
```
The summary information of our original data looked like this


```r
summary(ActivityData)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

The clean data set looks like this


```r
summary(CleanActData)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

You can see that all the NAs have been removed.  

####4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
CleanDailySteps<-aggregate(steps~date, CleanActData, sum)
ggplot(CleanDailySteps, aes(x=steps)) +
    geom_histogram(col="red",fill="blue") +
    labs(x="Steps Logged",y="Count of Days",title="Histogram of Total Steps Taken Each Day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](ReproducibleResearchProject1_files/figure-html/plot_new-1.png)<!-- -->

```r
CleanMeanSteps<-mean(CleanDailySteps$steps)
CleanMedianSteps<-median(CleanDailySteps$steps)
print(c(paste("The mean number of steps is ",round(CleanMeanSteps,2), " and the median number of steps is ",round(CleanMedianSteps,2),".",sep=""), paste("This mean is ", round(CleanMeanSteps - MeanSteps,2)," higher than the original and the median is ",round(CleanMedianSteps - MedianSteps,2), " higher than the original.",sep=""),paste("The total impact of imputing missing data was an additional ",round(sum(CleanActData$steps) - sum(ActivityData$steps,na.rm=T),2)," steps.",sep="")), quote=F) 
```

```
## [1] The mean number of steps is 10766.19 and the median number of steps is 10766.19.        
## [2] This mean is 0 higher than the original and the median is 1.19 higher than the original.
## [3] The total impact of imputing missing data was an additional 86129.51 steps.
```

##Question 4: Are there differences in activity patterns between weekdays and weekends?
Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

####1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
StringPlace<-data.frame("",stringsAsFactors=F)
WeekActData<-data.frame(cbind(CleanActData,weekdays(CleanActData$date),StringPlace),stringsAsFactors = F)
colnames(WeekActData)<-c("steps","date","interval","weekday","day")
head(WeekActData)
```

```
##       steps       date interval weekday day
## 1 1.7169811 2012-10-01        0  Monday    
## 2 0.3396226 2012-10-01        5  Monday    
## 3 0.1320755 2012-10-01       10  Monday    
## 4 0.1509434 2012-10-01       15  Monday    
## 5 0.0754717 2012-10-01       20  Monday    
## 6 2.0943396 2012-10-01       25  Monday
```

```r
for (i in 1:nrow(WeekActData)){
    if(WeekActData$weekday[i]=="Saturday" || WeekActData$weekday[i]=="Sunday") {
      WeekActData$day[i]<-"weekend"
    }
    else {
      WeekActData$day[i]<-"weekday"
    }
}
```

####2. Make a panel plot containning a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
WeekdayPlace<-data.frame("weekday",stringsAsFactors=F)
WeekendPlace<-data.frame("weekend",stringsAsFactors=F)
WeekendData<-subset(WeekActData,day=="weekend")
WeekdayData<-subset(WeekActData,day=="weekday")
IntWeekendSteps<-cbind(aggregate(steps~interval, WeekendData, mean),WeekendPlace)
IntWeekdaySteps<-cbind(aggregate(steps~interval, WeekdayData, mean),WeekdayPlace)
colnames(IntWeekendSteps)<-c("interval","steps","day")
colnames(IntWeekdaySteps)<-c("interval","steps","day")
IntWeekSteps<-rbind(IntWeekendSteps,IntWeekdaySteps)
ggplot(IntWeekSteps, aes(x=interval, y=steps)) +
  geom_line(color = "blue") +
  facet_wrap(~day,nrow=2,ncol=1) +
  labs(x="Interval", y="Number of Steps")
```

![](ReproducibleResearchProject1_files/figure-html/panel_plot-1.png)<!-- -->
