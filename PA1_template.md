
Reproducible Research assignment 1
===========================================================

From the assignment statement: "This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day."

This assignment involves downloading and anaylizing the data in various ways.

## Load and preprocess the data


```r
# 1. Load the data:
rawdata <- read.csv('activity.csv')

# 2. Process/transform the data (if necessary) into a format suitable for your analysis:
# Keep only those observations with no NAs:
data <- rawdata[complete.cases(rawdata),]
```

## Mean total number of steps taken per day


```r
# Calculate total number of steps taken per day:
days <- unique(data[,2])
daysteps <- 0
i <- 0
for(day in days)
  { 
	i <- i + 1
	daydata <- data[data[,2]==days[i],]
	if(length(daydata[,1])==288){daysteps[i] <- sum(daydata[,1])}else{daysteps[i]<--99}
  }
daysteps[-daysteps==-99] <- NULL
hist(daysteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meansteps <- mean(daysteps)
mednsteps <- median(daysteps)
meansteps
```

```
## [1] 10766.19
```

```r
mednsteps
```

```
## [1] 10765
```

In this data set, the average number of steps per day is 1.0766189\times 10^{4}, and the median number of steps per day is 1.0765\times 10^{4}.

## Average daily activity pattern


```r
intervals <- unique(data[,3])
avg_steps <- 0
i <- 0
for(interval in intervals)
	{
	i <- i + 1
	intervaldata <- data[data[,3]==intervals[i],]
	avg_steps[i] <- mean(intervaldata[,1])	
	}
plot(intervals,avg_steps,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxsteps <- max(avg_steps)
maxinterval <- intervals[avg_steps==maxsteps]
maxhr <- floor(maxinterval/60)
maxmin <- (maxinterval/60-maxhr)*60
```
On average, the individual highest activity level (41.2339623 steps/min) occured during 5-minute interval 835.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numNAs = length(rawdata[,1])-length(data[,1])
```
There are 2304 rows with NAs in the raw data.

2. Devise a strategy for filling in all of the missing values in the dataset.

I will replace each NA with the average number of steps across all days in the dataset for that interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newdata <- rawdata
for (i in 1:length(newdata[,1]))
	{
	if(is.na(newdata[i,1])){newdata[i,1]=avg_steps[match(newdata[i,3],intervals)]}
	}
```

4.  Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.


```r
# Calculate total number of steps taken per day:
days <- unique(newdata[,2])
daysteps <- 0
i <- 0
for(day in days)
  { 
	i <- i + 1
	daydata <- newdata[newdata[,2]==days[i],]
	if(length(daydata[,1])==288){daysteps[i] <- sum(daydata[,1])}else{daysteps[i]<--99}
  }
daysteps[daysteps==-99] <- NULL
hist(daysteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
meansteps <- mean(daysteps)
mednsteps <- median(daysteps)
meansteps
```

```
## [1] 10766.19
```

```r
mednsteps
```

```
## [1] 10766.19
```
In this data set, *now with missing values imputed by substituting the average number of steps taken during the associted 5-minute interval across all days containing data*, the average number of steps per day is 1.0766189\times 10^{4}, and the median number of steps per day is 1.0766189\times 10^{4}. Notice that now the mean and median are equal, whereas they were not exactly equal before we imputed the missing data.

## Are there differences in activity patterns between weekdays and weekends?


```r
for(i in 1:length(newdata[,1]))
	{
	newdata[i,4] <- weekdays(as.Date(newdata[i,2]))
	if(newdata[i,4]=="Saturday"|newdata[i,4]=="Sunday"){newdata[i,5]<-"weekend"}else{newdata[i,5]<-"weekday"}
	}
newvar <- factor(newdata[,5])
newdata[,6] <- newvar
newdata <- newdata[-c(4:5)]
```

2. Make a panel plot containing a time series plot of the 50minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.3
```

```r
intervals <- unique(newdata[,3])
weekday_data <- newdata[which(newdata[,4]=="weekday"),]
weekend_data <- newdata[which(newdata[,4]=="weekend"),]
weekday_avg_steps <- 0
weekend_avg_steps <- 0
i <- 0
for(interval in intervals)
	{
	i <- i + 1
	weekday_intervaldata <- weekday_data[weekday_data[,3]==intervals[i],]
	weekday_avg_steps[i] <- mean(weekday_intervaldata[,1])
	weekend_intervaldata <- weekend_data[weekend_data[,3]==intervals[i],]
	weekend_avg_steps[i] <- mean(weekend_intervaldata[,1])
	}

par(mfrow = c(2,1))
plot(intervals,weekday_avg_steps,type="l",ylim=c(0,250))
title(main = "Weekdays",font=2)
plot(intervals,weekend_avg_steps,type="l",ylim=c(0,250))
title(main = "Weekends",font=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

The above plots are not pretty (still an R newbie), but they show time series lines plots of the average steps taken per 5-minute interval for all weekdays and weekends, respectively, in the dataset with missing values imputed.

