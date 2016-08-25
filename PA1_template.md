# Reproducible Research: Peer Assessment 1
Jussi Kahtava  
22 August 2016  
Peer Assignment 1 for the Reproducable Research module in Data Science Specialisation
=====================================================================================

This assignment makes use of data from a personal activity monitoring device which collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

We start by loading the libraries needed in running the relevant R packages.


```r
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
```

## Loading and preprocessing the data
The monitoring data is first uploaded to R for preprocessing and is stored in a data frame `activity`. The date column is formatted into POSIXct format. The interval column is converted into factors. We also note that the file has a number of NAs which we'll address later on during processing.

```r
activity <- read.csv("./activity.csv", header = TRUE, stringsAsFactors = FALSE )
activity$date <- as.POSIXct(activity$date)
activity$interval <- as.factor(activity$interval)
```

Examining the interval values in `activity$interval`, it is clear that the data points represent five-minute time intervals in hours and minutes. In order to make them print out properly in plots later on the invervals are converted into strings more clearly showing the time instant in question.

```r
intervalLevels <- levels(activity$interval)
intervalLevels <-  gsub(" ", "0", sprintf("%4s", intervalLevels))
tmp <- strsplit(intervalLevels, "")
tmp2 <- sapply(tmp, function(x) {paste0(x[1:2], collapse = "")})
tmp3 <- sapply(tmp, function(x) {paste0(x[3:4], collapse = "")})
intervalLevels <- paste(tmp2, tmp3, sep=":", collapse = NULL)
levels(activity$interval) <- intervalLevels
```

At this stage we have converted the `interval` column of the original uploaded data in `activity` into a different representation but the information remains the same.

## What is mean total number of steps taken per day?

The subject's movement was recorded over 61 days in five minute intervals.  The next chunk of code calculates 

- the total number of steps taken each day 
- the mean of the total number of steps across all the days
- the median of the total number of steps across all the days

and plots the result as a histogram. Missing values were converted to zeros, so dates with no values for steps at all fall into the left-most bin of the histogram.

```r
dailyActivity <- activity%>%group_by(date)%>%summarise(sum(steps, na.rm = TRUE))
names(dailyActivity) <- c("date","dailysteps")
meanSteps <- round(mean(dailyActivity$dailysteps, na.rm = TRUE), digits = 0)
medianSteps <- median(dailyActivity$dailysteps, na.rm = TRUE)
bins = range(dailyActivity$dailysteps)[2]
```
It turns out that the *median* number of daily steps is 10395 and the *mean* number of daily steps is 9354. The binwidth used in the histogram is 1060 which appears a reasonable size for the distribution in question. Using smaller bidwidths tends to brek the histogram into separate bars with white space between them. The fact that the distribution's mean is smaller than its median shows that the distribution is skewed left. This is due to the days with missing data being converted to zero steps.


```r
ggplot(dailyActivity, aes(x=dailysteps)) + geom_histogram(binwidth = bins/20, fill = "orangered") + geom_vline(size=1, aes(xintercept = medianSteps)) + geom_vline(size=1, color = "darkgreen", aes( xintercept = meanSteps)) + labs(x="number of steps taken per day")
```

![Histogram with missing data](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

The median of daily steps is shown as a black vertical line on the histogram whereas the mean is a green vertical line to the left of the median.

## What is the average daily activity pattern?

In this section we evaluate the daily activity pattern for the subject. Each five minute interval (288 of them across one day) is averaged over all the days in the measurement period. Note that missing values are not included in the computation. These averages are plotted and shown over a 24-h interval.


```r
fiveminActivity <- activity%>%group_by(interval)%>%summarise(mean(steps, na.rm = TRUE))
names(fiveminActivity) <- c("interval","meansteps")
# generate tick spacing and marks for the x axis of the plot
hops <- seq(from = 1, to = length(intervalLevels), by = 12)
```

```r
p <- ggplot(fiveminActivity, aes(x=as.numeric(interval), y = meansteps)) + geom_line() + labs(x="interval", y = "number of steps")
p + scale_x_continuous(breaks = seq(1,288, 12), labels = intervalLevels[hops]) + theme(axis.text.x=element_text(angle=90,hjust=1))
```

![Daily averages with missing data](PA1_template_files/figure-html/unnamed-chunk-7-1.png)

Below we calculate at which time interval the maximum number of steps was taken.

```r
maxSteps <- fiveminActivity$meansteps[which.max(fiveminActivity$meansteps)]
maxStepsTime <- fiveminActivity$interval[which.max(fiveminActivity$meansteps)]
nextInterval <- fiveminActivity$interval[which.max(fiveminActivity$meansteps)+1]
```
The maximum number of steps taken on an average day took place during the interval 08:35 - 08:40.

## Imputing missing values

At this step we analyse the data more closely and identify how many missing values it has in total. 

```r
missingIntervals <- is.na(activity$steps)
counter <- table(missingIntervals)
```
The number of missing values from the `activity$steps` column is 2304, which is a total of 13.11% of all the observations. This is a fairly high figure, so simply removing those rows from the dataset without careful analysis is not an option. 

The missing values appear to be concentrated on specific calendar days, i.e. a day either has no missing `steps` values or all its `steps` values are missing. In total, fully eight dates are missing completely of `steps` data. These dates are 

```
## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
## [6] "2012-11-10" "2012-11-14" "2012-11-30"
```
And they represent the following weekdays:

```
## [1] "Monday"    "Monday"    "Thursday"  "Sunday"    "Friday"    "Saturday" 
## [7] "Wednesday" "Friday"
```

Clearly, the distribution is fairly even across weekdays and we lose no key information regarding the balance of weekdays and weekends.

Should the missing values be randomnly interspersed between existing values in a given calendar day, it would be better to impute the missing figures by replacing a missing five-minute interval value with the overall mean of the *same* five-minute interval value calculated earlier. However, since the missing values consist of complete calendar days without data, a simpler option may indeed be just to remove those days completely from the data frame. 

For the sake of completeness we consider both approaches below, i.e. 

- removing the rows with missing values
- replacing an interval with missing value with the respective five-minute-interval mean

The first approach is very quick with the following command:

```r
activityImputed <- activity[!missingIntervals, ]
```

For the second approach, the code chunk below replaces all the rows in `activity` having NAs in `activity$steps` with the respective five-minute-interval mean over all the observed days. The other rows continue to have the original data points from the uploaded file.


```r
missingData <- activity[missingIntervals, ]
mergedMissing <- merge(missingData, fiveminActivity, by = "interval")
tmp <- mergedMissing %>% mutate(steps = meansteps)%>%select(interval, steps, date)
missingData2 <- unique(merge(missingData, tmp, by = "interval")%>%select(steps.y, date.x,interval))
names(missingData2) <- c("steps", "date","interval")
missingData <- arrange(missingData2, date, interval)
activityImputed2 <- activity
activityImputed2[missingIntervals, ] <- missingData[, ]
```

### Recalculation with imputed data

Now we have a new dataset where the missing values are imputed with the average value for each interval across the number of days. Let's recalculate the mean total number of steps taken each day. 

### Removing dates with missing values from the original data

```r
dailyActivity <- activityImputed%>%group_by(date)%>%summarise(sum(steps, na.rm = TRUE))
names(dailyActivity) <- c("date","dailysteps")
meanStepsImputed <- round(mean(dailyActivity$dailysteps, na.rm = TRUE), digits = 0)
medianStepsImputed <- median(dailyActivity$dailysteps, na.rm = TRUE)
bins = range(dailyActivity$dailysteps)[2]
```

### Replacing missing values with five-minute interval means

```r
dailyActivity2 <- activityImputed2%>%group_by(date)%>%summarise(sum(steps, na.rm = TRUE))
names(dailyActivity2) <- c("date","dailysteps")
meanStepsImputed2 <- round(mean(dailyActivity2$dailysteps, na.rm = TRUE), digits = 0)
medianStepsImputed2 <- round(median(dailyActivity2$dailysteps, na.rm = TRUE), digits = 0)
bins = range(dailyActivity2$dailysteps)[2]
statistics <- data.frame(c("mean", "median"), c(as.integer(meanSteps), as.integer(medianSteps)), c(as.integer(meanStepsImputed), as.integer(medianStepsImputed)),c(as.integer(meanStepsImputed2), as.integer(medianStepsImputed2)))
names(statistics) <- c("centraltendency","withNAs","imputedNAsRemoved","imputedNAsAveraged")
```

### Comparison of central tendencies

It turns out that the mean number of daily steps is the same for both approaches at 10766. On the other hand, there is a very small difference in the median number of daily steps is at 10765 and 10766. This is understandable because in the second approach we hav added new values to the dataset even though they concentrate around the mean. When the median and mean values for the original dataset with missing values are compared to these, we can see that the left-skewed characteristics of the distribution disappears (due to imputing NAs with average values).

```
##   centraltendency withNAs imputedNAsRemoved imputedNAsAveraged
## 1            mean    9354             10766              10766
## 2          median   10395             10765              10766
```
The histogram shows a marked reduction in the left-most bin centered around zero steps, since the missing values were imputed. This time also the vertical lines showing mean and median overlap, which shows that the distribution is no longer skewed. 

### Histogram removing rows with missing values


```r
ggplot(dailyActivity, aes(x=dailysteps)) + geom_histogram(binwidth = bins/20, fill = "orangered") + geom_vline(size=1, aes(xintercept = medianStepsImputed)) + geom_vline(size=1, color = "darkgreen", aes( xintercept = meanStepsImputed)) + labs(x="number of steps taken per day")
```

![Histogram with missing data imputed through removing rows](PA1_template_files/figure-html/unnamed-chunk-17-1.png)

### Histogram for imputing missing values with averages

```r
ggplot(dailyActivity2, aes(x=dailysteps)) + geom_histogram(binwidth = bins/20, fill = "orangered") + geom_vline(size=1, aes(xintercept = medianStepsImputed)) + geom_vline(size=1, color = "darkgreen", aes( xintercept = meanStepsImputed)) + labs(x="number of steps taken per day")
```

![Histogram with missing data imputed through averaging](PA1_template_files/figure-html/unnamed-chunk-18-1.png)

## Are there differences in activity patterns between weekdays and weekends?

In this section we look into whether there are any differences in the activity patterns for the subject between weekdays and weekends.First we add a new column to `activity` named `dayofweek`. This new column will hold the factor `weekday` for days of Monday, Tuesday, Wednesday, Thursday and Friday; and `weekend` for Saturday and Sunday. 


```r
activity <- mutate(activityImputed, dayofweek = weekdays(date))
weekendDays <- grepl("^S", weekdays(activity$date))
activity$dayofweek[weekendDays] <- "weekend"
activity$dayofweek[!weekendDays] <- "weekday"
activity$dayofweek <- as.factor(activity$dayofweek)
```


```r
fiveminActivity2 <- activity%>%group_by(interval, dayofweek)%>%summarise(mean(steps, na.rm = TRUE))
names(fiveminActivity2) <- c("interval","dayofweek","meansteps")
hops <- seq(from = 1, to = length(intervalLevels), by = 12)
```

```r
p <- ggplot(fiveminActivity2, aes(x=as.numeric(interval), y = meansteps)) + geom_line() + labs(x="interval", y = "number of steps") + facet_grid(dayofweek~.)
p + scale_x_continuous(breaks = seq(1,288, 12), labels = intervalLevels[hops]) + theme(axis.text.x=element_text(angle=90,hjust=1))
```

![Daily averages with missing data imputed](PA1_template_files/figure-html/unnamed-chunk-21-1.png)

We can see a pattern which is likely expected, i.e. the subject gets up and starts moving quite a bit earlier on weekdays than during weekend. The subject also settles down and moves less earlier during weekday evenings. 

