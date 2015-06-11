#Some Important tasks!
##Check the existence of the dataset
if(!file.exists("activity.csv")){
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile="repdata-data-activity.zip")
    unzip("repdata-data-activity.zip")
}
##Install important packages
install.packages ("dplyr")
library (dplyr)
install.packages ("ggplot2")
library (ggplot2)

#Project Assignment
##1 - Loading and preprocessing the data
RAW <- read.csv ("activity.csv")

##2 - What is mean total number of steps taken per day?
ACT<- group_by (RAW, date)
ACT<-summarise(ACT, sum(steps))
names (ACT) <- c("Date","Step")
hist (ACT$Step,col="red", breaks=15, main ="Histogram of Activity", xlab="Nº of Steps per day")
dev.print(png, file = "Hist.png", width = 480, height = 480)
dev.off()
mean (ACT$Step, na.rm=TRUE)
median (ACT$Step, na.rm=TRUE)

##3 - What is the average daily activity pattern?
ACT<- group_by (RAW, interval)
ACT<-summarise(ACT, mean(steps,na.rm=TRUE))
names (ACT) <- c("Interval","Steps")
plot(ACT,type="l", main="Daily Activity Pattern")
dev.print(png, file = "Plot.png", width = 480, height = 480)
dev.off()
ACT$Interval[which.max(ACT$Steps)]

##4 - Imputing missing values
ACT1<- RAW
A<-summary (ACT1)
A[7,1]
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
ACT1<- group_by (ACT1, date)
ACT1<-summarise(ACT1, sum(steps))
names (ACT1) <- c("Date","Steps")
hist (ACT1$Steps,col="red", breaks=15, main ="Histogram of Activity", xlab="Nº of Steps per day")
dev.print(png, file = "Hist2.png", width = 480, height = 480)
dev.off()
mean (ACT1$Steps) #SAME as the dataset without NA. Expected, since the strategy used was the substitution of NA by the mean os steps in the interval!
median (ACT1$Steps) #Different as the dataset without NA

##5 - Are there differences in activity patterns between weekdays and weekends?
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
ACT1$date<-as.POSIXct(ACT1$date)
ACT1<-mutate (ACT1, Week=weekdays(ACT1$date))
for (i in 1:length (ACT1$steps)) {
    if (ACT1$Week[i]=="Saturday"|ACT1$Week[i]=="Sunday") {
        ACT1$Week[i]<-"Weekend"
    }else {
        ACT1$Week[i]<-"Weekday"
    }
}
ACT1<- group_by (ACT1, interval, Week)
ACT1<-summarise(ACT1, mean(steps))
names (ACT1) <- c("Interval","Week","Steps")
png(file = "Plot2.png", width = 480, height = 480)
ggplot(ACT1, aes(x=Interval, y=Steps, colour=Week)) +
    geom_line() +
    ylab ("Mean Steps") +
    xlab ("Interval") +
    ggtitle("Activity Pattern")
dev.off()

