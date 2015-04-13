# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

I loaded the data and I processed the data into a format suitable for analysis


```r
act <- read.csv("activity.csv",header = T)
act$dt <- strptime(act$date, "%Y-%m-%d")
act$interval <- as.factor(act$interval)
act$dtf <- as.factor(act$date)
```


## What is mean total number of steps taken per day?


```r
t <- tapply(act$steps,act$dtf,sum,na.rm=T)
hist(t,nclass=10,xlab="Total number of steps taken per day",main="Total number of steps taken per day",col="red")
```

![](PA1_template_files/figure-html/totalstep-1.png) 

```r
mt <- mean(t)
mdt <-median(t)
```




## What is the average daily activity pattern?


```r
t2 <- tapply(act$steps,act$interval,mean,na.rm=T)
t2 <- data.frame(t2)
t2$interval <- row.names(t2)
names(t2) <- c("steps","interval")
with(t2,plot(interval,steps, col="blue",type="l"))
```

![](PA1_template_files/figure-html/aveact-1.png) 

```r
mx <- which.max(t2$steps)
t2[mx,2]
```

```
## [1] "835"
```

## Imputing missing values

I replaced the missing values with the mean value of the steps

```r
sum(is.na(act$steps))  
```

```
## [1] 2304
```

```r
act2<-act
act2$steps <- replace(act2$steps,is.na(act2$steps),round(mean(act2$steps,na.rm=T),0))
t3 <- tapply(act2$steps,act2$dtf,sum,na.rm=T)
hist(t3,nclass=10,xlab="Total number of steps taken per day",main="Total number of steps taken per day",col="red")
```

![](PA1_template_files/figure-html/missingv-1.png) 

```r
mean(t3)
```

```
## [1] 10751.74
```

```r
median(t3)
```

```
## [1] 10656
```
These mean and median values differ from the estimates from the first part of the assignment. Imputing missing data has increasd the estimates of the total daily number of steps.



## Are there differences in activity patterns between weekdays and weekends?


```r
act4 <- act
act4$wd <- weekdays(act4$dt)
act4$weflag <- act4$wd %in% c("szombat","vasárnap")
t4 <- tapply(act4$steps,act4[,c(3,7)],mean,na.rm=T)
t4 <- data.frame(t4)
t4$interval <- row.names(t4)
names(t4) <- c("weekday","weekend","interval")
par(mfrow=c(2,1),mar=c(5,5,1,1))
with(t4, plot(interval, weekend, col="blue", type="l",ylab="Number of steps", main="weekend"))
with(t4, plot(interval, weekday, col="blue", type="l",ylab="Number of steps", main="weekday"))
```

![](PA1_template_files/figure-html/weekends-1.png) 