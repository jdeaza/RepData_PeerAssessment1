Peer Assessment 1
=================

This is the Peer Assesment 1 of the Reproducible Research course

Loading and preprocessing the data


```r
data_a1 <- read.csv("./passessment1/activity-3.csv")
library(ggplot2)
library(plyr)
library(Hmisc)
library(knitr)
library(markdown)
data_sum <- tapply(data_a1$steps, data_a1$date, FUN = sum, na.rm = TRUE)
```
What is mean total number of steps taken per day?
-------------------------------------------------

* First, I will make a histogram
* Second, I will estimate mean and median total number of steps without missing values

```r
hist(data_sum, col = "red", main = "Histogram steps without missing values", 
    xlab = "Number of steps", breaks = 20)
```

<img src="figure/unnamed-chunk-2.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

```r
mean(data_sum, na.rm = TRUE)
```

```
[1] 9354
```

```r
median(data_sum, na.rm = TRUE)
```

```
[1] 10395
```
What is the average daily activity pattern?
-------------------------------------------

* Subsetting data to get average daily activity pattern
* Making plot average daily pattern
* To get the 5-minute interval with maximum number of steps 

```r
data_mean <- as.data.frame(tapply(data_a1$steps, data_a1$interval, mean, na.rm = TRUE))
colnames(data_mean) <- "steps"
plot(data_mean$steps, type = "l", main = "Average activity pattern", xlab = "5 minute Interval", 
    ylab = "Averaged across all day")
```

<img src="figure/unnamed-chunk-3.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

```r
data_mean[which.max(data_mean$steps), ]
```

```
  835 
206.2 
```
Imputing missing values
-----------------------

* Load required packages
* Create new data frame with NA imputed
* Reorder data (plyr package orders by interval in this case)
* Make histogram steps taken each day
* Estimate mean and median total number of steps with missing values imputed
* Differences between mean and median with missing values and without missing values
* Creating new data frame to compare outcomes statistics with missing values and data imputed
* Comparative Plot

```r
data_new <- ddply(data_a1, "interval", mutate, imputed.value = impute(steps, 
    mean))
data_new <- data_new[order(data_new$date), ]
dsum_new <- tapply(data_new$imputed.value, data_new$date, sum)
hist(dsum_new, col = "blue", main = "With missing values imputed", xlab = "Number of steps", 
    cex.main = 1, breaks = 20)
```

<img src="figure/unnamed-chunk-4.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />
Estimating mean and median with imputed values

```r
mean(dsum_new)
```

```
[1] 10766
```

```r
median(dsum_new)
```

```
[1] 10766
```
Histogram with imputed values

```r
par(mfrow = c(2, 1))
hist(data_sum, col = "red", main = "Without missing values", xlab = "Number of steps", 
    cex.main = 1, cex.lab = 1, breaks = 20)
hist(dsum_new, col = "blue", main = "With missing values imputed", xlab = "Number of steps", 
    cex.main = 1, breaks = 20)
mtext("Histogram steps", side = 3, line = -1, outer = TRUE, cex.main = 2.5, 
    font = 2)
```

<img src="figure/unnamed-chunk-6.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />
Comparative plot

```r
ndata <- data.frame(Measure = factor(c("Mean missing", "Mean imputed", "Median missing", 
    "Median imputed"), levels = c("Mean missing", "Mean imputed", "Median missing", 
    "Median imputed")), Value = c(9354.23, 10766.19, 10395, 10766.19))
ggplot(data = ndata, aes(x = Measure, y = Value)) + geom_bar(aes(fill = Measure), 
    stat = "identity") + ggtitle("Steps statistics: Data with NA vs data imputed")
```

<img src="figure/unnamed-chunk-7.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />
Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

* Creating data with weekdays function and factor variable "Weekday" and "Weekend"
* Changing levels names in factor variable
* Plot outcomes weekday and weekend

```r
data_new$date_n <- as.Date(data_new$date, format = "%Y-%m-%d")
data_new$weekday <- factor(weekdays(data_new$date_n))
levels(data_new$weekday) <- list(Weekday = "Monday", Weekday = "Tuesday", Weekday = "Wednesday", 
    Weekday = "Thursday", Weekday = "Friday", Weekend = "Sunday", Weekend = "Saturday")
data_meanw <- ddply(data_new, .(interval, weekday), summarise, imputed.value = mean(imputed.value))
```
Plot with differences between weekday and weekend

```r
xyplot(imputed.value ~ interval | weekday, data = data_meanw, type = "l", layout = c(1, 
    2), ylab = "Number of steps", col = "blue")
```

<img src="figure/unnamed-chunk-9.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />
