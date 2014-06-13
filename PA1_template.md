# Reproducible Research: Peer Assessment 1
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.




## Loading and preprocessing the data
1. Check if zip file exists
2. If not, download the file,
3. Unzip and read the file activity.csv


```r
filename <- "repdata_activity.zip"
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!file.exists(filename)) {
    download.file(fileurl, destfile = filename, method = "curl")
}
unzip(filename)
datafile <- read.csv("activity.csv", colClasses = c("numeric", "character", 
    "numeric"))
# convert to date format
datafile$date <- as.Date(datafile$date, format = "%Y-%m-%d")
```

Explore data


```r
summary(datafile)
```

```
##      steps           date               interval   
##  Min.   :  0    Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0    1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0    Median :2012-10-31   Median :1178  
##  Mean   : 37    Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12    3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806    Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

```r
str(datafile)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```



## What is mean total number of steps taken per day?


```r
# aggregate by date
aggdata <- aggregate(steps ~ date, datafile, sum)
mean_val <- mean(aggdata$steps)
median_val <- median(aggdata$steps)
hist(aggdata$steps, main = "Histogram of Steps per Day", xlab = "# of steps")
```

![plot of chunk calc_mean](figure/calc_mean.png) 


Mean of dataset **10766.19**   
Median is **10765**


## What is the average daily activity pattern?


```r
aggdata <- aggregate(steps ~ interval, data = datafile, mean, na.rm = T)
maxinterval <- aggdata[which.max(aggdata$steps), ]

plot(aggdata$interval, aggdata$steps, type = "l", xlab = "Interval", ylab = "Avg Steps", 
    main = "Average Daily Pattern")
points(maxinterval$interval, 0, col = "red", pch = 15)  # highlight max interval
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
aggdata[which.max(aggdata$steps), ]
```

```
##     interval steps
## 104      835   206
```



## Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). 

```r
nadata <- is.na(datafile)
table(nadata)
```

```
## nadata
## FALSE  TRUE 
## 50400  2304
```


Replacing missing value with mean value of that interval

```r
mdata <- merge(datafile, aggdata, by = "interval", all.x = TRUE)
mdata$steps.x <- ifelse(is.na(mdata$steps.x), mdata$steps.y, mdata$steps.x)
mdata <- mdata[, c(1:3)]
colnames(mdata) <- c("interval", "steps", "date")
aggdata <- aggregate(steps ~ date, mdata, sum)
mean_val <- mean(aggdata$steps)
median_val <- median(aggdata$steps)
hist(aggdata$steps, main = "Histogram of Steps per Day", xlab = "# of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

Updated Mean values  
Mean of dataset **10766.19**   
Median is **10766.19**

## Are there differences in activity patterns between weekdays and weekends?

Create a new column in the dataset to indicate week day type.



```r
mdata$daytype <- ifelse(weekdays(as.Date(mdata$date)) %in% c("Saturday", "Sunday"), 
    "weekend", "weekday")
head(mdata)
```

```
##   interval steps       date daytype
## 1        0   1.7 2012-10-01 weekday
## 2        0   0.0 2012-11-23 weekday
## 3        0   0.0 2012-10-28 weekend
## 4        0   0.0 2012-11-06 weekday
## 5        0   0.0 2012-11-24 weekend
## 6        0   0.0 2012-11-15 weekday
```

```r
par(mfrow = c(2, 1))
wkday <- aggregate(steps ~ interval, data = mdata, subset = mdata$daytype == 
    "weekday", FUN = mean)
plot(wkday, type = "l", main = "Weekday")
wkend <- aggregate(steps ~ interval, data = mdata, subset = mdata$daytype == 
    "weekend", FUN = mean)
plot(wkend, type = "l", main = "Weekend")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 



