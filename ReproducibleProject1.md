---
title: "Reproducible Reserach --Course Project 1"
author: "Rajesh"
date: "26/06/2020"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

**1.Code for reading in the dataset and/or processing the data**


```r
DF <- read.csv('activity.csv')
str(DF)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

**2.Histogram of the total number of steps taken each day**


```r
DFsteps <- tapply(DF$steps, DF$date, FUN=sum, na.rm=TRUE)
library(ggplot2)
qplot(DFsteps, binwidth=1000, xlab="total number of steps taken each day")
```

![](ReproducibleProject1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

**3.Mean and median number of steps taken each day**


```r
stepsMean <- mean(DFsteps, na.rm=TRUE)
stepsMedian <- median(DFsteps, na.rm=TRUE)
stepsMean
```

```
## [1] 9354.23
```

```r
stepsMedian
```

```
## [1] 10395
```

**4.Time series plot of the average number of steps taken**


```r
averages <- aggregate(x=list(steps=DF$steps), by=list(interval=DF$interval),FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) + geom_line() + ggtitle("Time Series: average number of steps") + xlab("5-minute interval") +   ylab("average number of steps taken")
```

![](ReproducibleProject1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

**5.The 5-minute interval that, on average, contains the maximum number of steps**

```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


**6.Code to describe and show a strategy for imputing missing data**


```r
library(ggplot2)
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 4.0.2
```

```r
require(gridExtra)
DF2 <- DF
# add column for calculation of steps
DF2$CI <- "original"

# Store number of rows in data
l <- nrow(DF2)

#Calculating numbers of NAs
length(which(is.na(DF2$steps)))
```

```
## [1] 2304
```

```r
#Calculating NA by mean values
for (i in 1:l) {
  if (is.na(DF2[i,1])) {
    DF2[i,1] <- averages[averages$interval == DF2[i,3],2]
    DF2[i,4] <- "completed"
  }
}

DFsteps2 <- tapply(DF2$steps, DF2$date, FUN=sum, na.rm=TRUE )

# Recalculate the mean and median for new modified data 
stepsMean2 <- mean(DFsteps2)
stepsMedian2 <- median(DFsteps2)

c(stepsMean2, stepsMean)
```

```
## [1] 10766.19  9354.23
```

```r
c(stepsMedian2, stepsMedian)
```

```
## [1] 10766.19 10395.00
```

**7.Histogram of the total number of steps taken each day after missing values are imputed**

```r
#plotting histogram with new calculated mean values

DFsteps2 <- tapply(DF2$steps, DF2$ date, FUN=sum, na.rm=TRUE)
library(ggplot2)
plot2<-qplot(DFsteps2, binwidth=1000, xlab="total number of steps taken each day")

#Comparing Histogram with missing values and Without missing values

DFsteps <- tapply(DF$steps, DF$date, FUN=sum, na.rm=TRUE)
library(ggplot2)
plot1<-qplot(DFsteps, binwidth=1000, xlab="total number of steps taken each day")

DFsteps2 <- tapply(DF2$steps, DF2$ date, FUN=sum, na.rm=TRUE)
library(ggplot2)
plot2<-qplot(DFsteps2, binwidth=1000, xlab="total number of steps taken each day")


#Plotting both original and modified 
grid.arrange(plot1, plot2, ncol=2)
```

![](ReproducibleProject1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

**8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**

```r
# Creating function to find a day of week for dataset
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
#Data with filled missing values
DF2$date <- as.Date(DF2$date)
DF2$day <- sapply(DF2$date, FUN = weekday.or.weekend)

averages <- aggregate(steps ~ interval + day, data = DF2, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")
```

![](ReproducibleProject1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
**9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report**
