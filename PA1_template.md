---
title: "Reproducible Research Week 2 Project 1 Report"
author: "Anonymous"
date: "2024-05-15"
output: html_document
---



### Assignment

From Professor Peng's original README:&nbsp;&nbsp;*"This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file."*  

*"Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis."*  

*"For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)."*  

*** 


#### Loading and preprocessing the data

The download and unpack (unzip) of the source data is seen as a one-time process, and is not
necessary to be repeated through reruns of this markdown script. 
So this author chooses to simply block quote the download and unzip sequence. 
It is taken that any reproduction of this paper will begin after the unzip of the data.  


> source <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"  
> dest   <- "TheData.zip"  
> download.file(source,dest)  
> unzip(dest)  

The libraries this analysis will use:  


```r
library(dplyr)
library(lubridate)
library(lattice)
```

*** 

Let's preprocess the original data to see what we're dealing with.  



```r
# let's bring in the raw data...
foo <- read.csv("activity.csv")
# ...and what does the data look like?
str(foo)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Let us account for any missing values.

```r
# with only 3 columns, who has the missing data?
anyNA(foo$steps)
```

```
## [1] TRUE
```

```r
anyNA(foo$date)
```

```
## [1] FALSE
```

```r
anyNA(foo$interval)
```

```
## [1] FALSE
```

*** 

#### What is mean total number of steps taken per day?
*For this part of the assignment, you can ignore the missing values in the dataset.*  

```r
foo <- foo[complete.cases(foo),]
```

1. *Make a histogram of the total number of steps taken each day.*  

```r
SPD <- foo %>% group_by(date) %>% summarise(StepsPerDay = sum(steps))
hist(SPD$StepsPerDay, 
     col = "salmon", 
     xlab = "Steps", 
     main = "Steps per Day"
)
```

<div class="figure" style="text-align: center">
<img src="figure/unnamed-chunk-5-1.png" alt="plot of chunk unnamed-chunk-5"  />
<p class="caption">plot of chunk unnamed-chunk-5</p>
</div>

2. *Calculate and report the mean and median total number of steps taken per day.*  

The mean calculated:  


```r
mean(SPD$StepsPerDay)
```

```
## [1] 10766.19
```

And the median calculated:  


```r
median(SPD$StepsPerDay)
```

```
## [1] 10765
```

*** 


#### What is the average daily activity pattern?
*Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*  

```r
avgSPD <- foo %>% group_by(interval) %>% summarise(AvgPerDay = mean(steps))
colnames(avgSPD) <- c("Time","AvgSteps")
plot(avgSPD$Time, avgSPD$AvgSteps, 
     type = "l", 
     xlab = "Interval",
     ylab = "Average Steps",
     )
```

<div class="figure" style="text-align: center">
<img src="figure/unnamed-chunk-8-1.png" alt="plot of chunk unnamed-chunk-8"  />
<p class="caption">plot of chunk unnamed-chunk-8</p>
</div>


*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*  

```r
avgSPD[which.max(avgSPD$AvgSteps),1]
```

```
## # A tibble: 1 × 1
##    Time
##   <int>
## 1   835
```
It appears that 8:35a.m., on average, is the busiest part of the day!  


*** 


#### Imputing missing values

*Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*  

```r
# we reset the original data by simply reloading the original raw dataset
foo <- read.csv("activity.csv")
```

*Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*  

```r
sum(is.na(foo))
```

```
## [1] 2304
```
There appears to be 2304 missing values for 'steps', marking a 
13.1147541% loss of the total step measurements.  

*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*  

```r
X <- foo %>% group_by(date) %>% summarise(steps = mean(steps))
M <- mean(X$steps,na.rm = TRUE)
```

*Create a new dataset that is equal to the original dataset but with the missing data filled in.*  

```r
foo$steps[is.na(foo$steps)] <- M
```

*** 


*Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*  

```r
SPD <- foo %>% group_by(date) %>% summarise(StepsPerDay = sum(steps))
hist(SPD$StepsPerDay, 
     col = "salmon", 
     xlab = "Steps", 
     main = "Steps per Day"
)
```

<div class="figure" style="text-align: center">
<img src="figure/unnamed-chunk-14-1.png" alt="plot of chunk unnamed-chunk-14"  />
<p class="caption">plot of chunk unnamed-chunk-14</p>
</div>

With the mean being:  


```r
mean(SPD$StepsPerDay)
```

```
## [1] 10766.19
```
And the median being:  


```r
median(SPD$StepsPerDay)
```

```
## [1] 10766.19
```

*Do these values differ from the estimates from the first part of the assignment?  What is the impact of imputing missing data on the estimates of the total daily number of steps?*  

Yes.  The frequency counts hve increased and the mean and median are now the same.  

*** 


#### Are there differences in activity patterns between weekdays and weekends?
*For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*  

*Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*  


```r
F <- c("weekday","weekend") 
F <- factor(F)
```

*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*  


```r
foo$date <- as.Date(foo$date)
foo$DayType <- F[(weekdays(foo$date) %in% c("Saturday", "Sunday")) + 1]
avgSPD <- foo %>% group_by(DayType,interval) %>% summarise(AvgPerDay = mean(steps))
colnames(avgSPD) <- c("DayType","Interval","AvgSteps")
panels <- list(strip.background = list(col = "wheat"))
xyplot(AvgSteps ~ Interval | as.factor(DayType), data = avgSPD, 
       type = "l", 
       col = "blue", 
       par.settings = panels, 
       layout = c(1,2)
)
```

<div class="figure" style="text-align: center">
<img src="figure/unnamed-chunk-18-1.png" alt="plot of chunk unnamed-chunk-18"  />
<p class="caption">plot of chunk unnamed-chunk-18</p>
</div>
  
Yes, visually, one can see that during the week there is a burst of activity during the morning 
and a slight settling over the afternoon, whereas, during a weekend, there is a more pronounced spread of busy-feet during the entire day.  

Looks like people are more out-and-about over the weekend!
