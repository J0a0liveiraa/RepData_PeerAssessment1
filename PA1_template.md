# Reproducible Research: Peer Assessment 1
This assignment is part of the Coursera course Reproducible Research.
Activity monitoring data will be used here as an accessory to demonstrate our
capacity to make a literate statistical program.
You can find more information about it in the `README.md` document.

## Loading data and packages
Some functions from specific packages will be used during this assigment.
Therefore it is important to load them 


```r
library (dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library (ggplot2)
```

Checks if the data are already in the working directory, if not the file is downloaded
(unzip included) from [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)


```r
if(!file.exists("activity.csv"))
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile="repdata-data-activity.zip")
    unzip("repdata-data-activity.zip")
```

The variables are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`).
    
* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format.
    
* **interval**: Identifier for the 5-minute interval in which
    measurement was taken.

The `read.csv()` function is used with the default values

* Headers=TRUE.

* NAs encoded as `NA`.


```r
RAW <- read.csv ("activity.csv")
```

## What is mean total number of steps taken per day?
The data (RAW) is grouped day (date) and then all steps taken each day are summed.
After processing the data set, a histogram is presented. 
Finally the mean and median are calculated.


```r
ACT<- group_by (RAW, date)
ACT<-summarise(ACT, sum(steps))
names (ACT) <- c("Date","Step")
hist (ACT$Step,col="red", breaks=15, main ="Histogram of Activity", xlab="Nº of Steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
MEAN_ORI <- mean (ACT$Step, na.rm=TRUE)
MEDIAN_ORI <- median (ACT$Step, na.rm=TRUE)
```

Each day the mean and median are 10766.1886792453 and 10765,
respectively.

## What is theaverage daily activity pattern?
Here the data (RAW) is grouped by intervals,  then the mean for each 
interval is calculated (without NAs).


```r
ACT<- group_by (RAW, interval)
ACT<-summarise(ACT, mean(steps,na.rm=TRUE))
names (ACT) <- c("Interval","Steps")
plot(ACT,type="l", main="Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
MAX <- ACT$Interval[which.max(ACT$Steps)]
```
 The 5-minutes interval with more steps starts at
 *08:35* 
 and ends 5 minutes later.

## Imputing missing values
The number of NAs is showed and to complete the dataset, the averaged
teps per interval is included.
This way we will recreate average days that will have features really close to
the original dataset.


```r
summary (RAW)[7,1]
```

```
## [1] "NA's   :2304  "
```

```r
ACT1 <- RAW
ACT2<- group_by (RAW, interval)
ACT2<-summarise(ACT2, mean(steps,na.rm=TRUE))
names (ACT2) <- c("Interval","Steps")
for (i in 1:length (ACT1$steps)) {
    if (is.na(ACT1$steps[i])==TRUE) {
        for (j in 1:length (ACT2$Interval)) {
            if (ACT1$interval[i]==ACT2$Interval[j]) {
                ACT1$steps[i]<-ACT2$Steps[j]
            }
        }
    }
}
```

### Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Now the problem is equal to question 1. The data is group by day (date),  
and the steps are summed.


```r
ACT1<- group_by (ACT1, date)
ACT1<-summarise(ACT1, sum(steps))
names (ACT1) <- c("Date","Steps")
hist (ACT1$Steps,col="red", breaks=15, main ="Histogram of Activity", xlab="Nº of Steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
MEAN <- mean (ACT1$Steps) 
MEDIAN <- median (ACT1$Steps)
```
Also the mean ande median of each dataset are:

* **Means** original: 10766.1886792453, corrected: 10766.1886792453.
* **Medians** original: 10765, corrected: 10766.1886792453.

Of course the mean value is the same, because the number of steps introduced 
was the mean for that specific 5-minutes interval.
The median changes because the dataset is bigger.

### Are there differences in activity patterns between weekdays and weekends?
The NAs in the original dataset were substituted by the averaged 
number of steps per interval.
This way we will recreate average days that will have features really close to
the original dataset.


```r
ACT1<- RAW
ACT2<- group_by (RAW, interval)
ACT2<-summarise(ACT2, mean(steps,na.rm=TRUE))
names (ACT2) <- c("Interval","Steps")
for (i in 1:length (ACT1$steps)) {
    if (is.na(ACT1$steps[i])==TRUE) {
        for (j in 1:length (ACT2$Interval)) {
            if (ACT1$interval[i]==ACT2$Interval[j]) {
                ACT1$steps[i]<-ACT2$Steps[j]
            }
        }
    }
}
```

### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
We are going to add the distinction between weekday and weekend in the dataset.


```r
ACT1$date<-as.POSIXct(ACT1$date)
ACT1<-mutate (ACT1, Week=weekdays(ACT1$date))
for (i in 1:length (ACT1$steps)) {
    if (ACT1$Week[i]=="Saturday"|ACT1$Week[i]=="Sunday") {
        ACT1$Week[i]<-"Weekend"
    }else {
        ACT1$Week[i]<-"Weekday"
    }
}
```

### 2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using **simulated data**:
We have to average the daily data to obtain the pattern across all days in the week and weekend.


```r
ACT1<- group_by (ACT1, interval, Week)
ACT1<-summarise(ACT1, mean(steps))
names (ACT1) <- c("Interval","Week","Steps")
ggplot(ACT1, aes(x=Interval, y=Steps, colour=Week)) +
    geom_line() +
    ylab ("Mean Steps") +
    xlab ("Interval") +
    ggtitle("Activity Pattern")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

The dataset has 4 distinctive high activity periods at 8am, 12pm, 4pm, 7pm. 
This is explained by the meals/snacks times. Then where the patterns differ 
between the two periods are between 5am and 8am (more activity during the weekdays), 
and also globally during the rest of the day there are more activity during the weekend. 
This is explained easily by work hours where people tend to be sitting at their 
desk when they are potentialy moving during the weekend.
