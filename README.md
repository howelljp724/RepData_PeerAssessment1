# RepData_PeerAssessment1
Reproducible Research Peer Assessment 1
Assignment 1, Reproducible Research
This assignment is designed to give students experience in MarkDown language.  It contains 5 steps, each of which has the student
solve a problem using R and then input this code into a MarkDown document.

Background: 
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, 
Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. 
But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and
software for processing and interpreting the data.
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through
out the day. The data consists of two months of data from an anonymous individual collected during the months of October and 
November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data

The data for this assignment can be downloaded from the course web site:
Dataset: Activity monitoring data [52K]
The variables included in this dataset are:
"steps": Number of steps taking in a 5-minute interval (missing values are coded as NA)

"date": The date on which the measurement was taken in YYYY-MM-DD format

"interval": Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. 
Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks
in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated
via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for
your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.



I. Loading and Processing the data



# II. What is mean total number of steps taken per day?

## 1. Calculate the total number of steps taken per day:
```{r}
library(base)
library(dplyr)
library(datasets)
library(lattice)
library(stats)
days<-group_by(activity, date)
sumdays<-summarize(days, steps=sum(steps, na.rm=TRUE))
```
### The total number of steps taken per day is as follows: 
```{r}
print(as.data.frame(sumdays), row.names=FALSE)
```

## 2. Make a Histogram of the total number of steps taken per day:
```{r}
hist(sumdays$steps, xlab="total steps taken (Interval=5K)", main = "Histogram of Total Steps Taken Each Day")
```

## 3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
totalstepsday<-sumdays %>% summarize(meansteps=mean(steps, na.rm=TRUE), mediansteps=median(steps, na.rm=TRUE))
mnstps<-totalstepsday$meansteps
mdnstps<-totalstepsday$mediansteps
#this is the mean total number of steps taken per day (values range from 0 to 15K)
```
### The mean of the total number of steps taken per day is `r mnstps`.
### The median of the total number of steps taken per day is `r mdnstps`.

# III. What is the average daily activity pattern?
## 1. Make a time-series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
meaninterval<-days %>% group_by(interval) %>% summarize(steps=mean(steps, na.rm=TRUE))
with(meaninterval, plot(interval, steps, type = "l", ylab="average steps taken across all 61 days"))
```

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
maxsteps<-meaninterval[meaninterval$steps==max(meaninterval$steps), ]$interval
```
### The interval that contains the maximum number of steps is `r maxsteps`

# IV. Inputting missing values

## 1. Calculate and report the total number of missing values in the dataset 
```{r}
nasteps = sum(is.na(activity$steps)) #total number of rows with NA's
nainterval = sum(is.na(activity$interval)) #confirms no NA values for intervals.
nadate = sum(is.na(activity$date)) #confirms no NA values for dates.
```
### Total number of missing values in the dataset is `r nasteps`

## 2. Devise a strategy for filling in all of the missing values in the dataset.
### My strategy is coded below.  
```{r}
steps2<-ifelse(is.na(activity$steps), meaninterval$steps, activity$steps)
#determine if the value of 'steps' is NA. If so, determine what interval it corresponds to and assign it the value of the total mean steps of that interval (meaninterval$steps). If the value is not NA assign its original value (activity$steps) back to it.
```
## 3. Create a new dataset that is equal to the original dataset but with missing values filled in.
### The code for this step is as follows:
```{r}
activity2<- activity %>% mutate(steps = steps2)#Create new dataset equivalent to first but with NAs filled in.
```

## 4a. Make a histogram of the total number of steps taken each day:
```{r}
days2<-group_by(activity2, date)
sumdays2<-summarize(days2, steps=sum(steps, na.rm=TRUE))
hist(sumdays2$steps, xlab="total steps taken (Interval=5K)", main = "Histogram of Total Steps Taken Each Day")
```
## 4b. Calculate and report the mean and median total number of steps taken per day.
```{r}
totalstepsday2<-sumdays2 %>% summarize(meansteps=mean(steps, na.rm=TRUE), mediansteps=median(steps, na.rm=TRUE))
```
### The mean total number of steps taken per day is `r totalstepsday2$meansteps`
### The median total number of steps taken per day is `r totalstepsday2$mediansteps`
### These values do differ from the estimates from part I.3.  They are higher.
### The impact of imputing missing values is that it makes the distribution of the data set more "normalized".  This can be seen in both the comparison of the histograms and the comparison of the means and medians.  The histogram of imputed data is more normal (bell-shaped) and the mean and median are very close to being equivalent.

# V. Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with 2 levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
day_week<-weekdays(as.POSIXct(days2$date), abbreviate=TRUE) #day_week variable uses "weekdays()" function to determine the actual day (i.e. "Mon", "Tue", "Wed", etc) of days2$date

#daydet assigns the value of either 'weekday' or 'weekend' to a value
daydet<-function(x){
        if(x %in% c("Mon", "Tue", "Wed", "Thu", "Fri")){
                x<-'weekday'
        }
        else{x<-'weekend'}
}
#finaldaydet is a list of character values ('weekday', 'weekend') that is the result of inputting the variable 'day_week' to the function 'daydet' (via sapply)
finaldaydet<-as.character(sapply(day_week, daydet))

days2 <-cbind(days2, finaldaydet) #days2 is a dataframe now containing where each value of each variable falls, whether on a weekday or weekend.
days3<-group_by(days2, finaldaydet)#days3 now groups the dataframe by value of 'finaldaydet'
```

## 2. Make a panel plot containing a time series plot (i.e. type = 'l') of the 5-min interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
c<-xyplot(steps~interval|finaldaydet, data = days3, ylab="Number of steps", type="l", layout=c(1, 2))
print(c)
```

