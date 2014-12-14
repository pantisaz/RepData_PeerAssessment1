
# First Peer Assesement - Reproducible Data


## Loading the processed data

```r
data<- read.csv("activity.csv")
```

## What is the mean total number of steps taken 

```r
hist<-aggregate(data$steps, by=list(date=data$date), sum, na.rm = TRUE)

hist(hist[,2], main = "Histogram", xlab = "Total number of steps", breaks = 25)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(hist[,2])
```

```
## [1] 9354.23
```

```r
median(hist[,2])
```

```
## [1] 10395
```

## What is the average daily activity pattern


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```r
means<-aggregate(data$steps, by=list(interval=data$interval), mean, na.rm = TRUE)

colnames(means)<-c("interval", "Average.steps")

qplot(interval, Average.steps, data= means, xlab = "Interval", ylab = "Average number of steps", geom = "line")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
means[which.max(means[,2]),]
```

```
##     interval Average.steps
## 104      835      206.1698
```

## Imputing missing values


```r
length(which(is.na(data$steps)))
```

```
## [1] 2304
```

```r
df<- merge(data, means, by= "interval")

for (i in 1:nrow(df)){
  
  if (is.na(df[i,2]) == TRUE){
    
    df[i,2] <- df[i,4]
    
  }
  
}

histo<-aggregate(df$steps, by=list(date=df$date), sum, na.rm = TRUE)

hist(histo[,2], main = "Histogram", xlab = "Total number of steps", breaks = 25)
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(histo[,2])
```

```
## [1] 10766.19
```

```r
median(histo[,2])
```

```
## [1] 10766.19
```

## Are there any differences in activity patterns between weekdays and weekends?


```r
df$day<- weekdays(as.Date(df$date))

library(car)
df$day<-recode(df$day,"c('Monday','Tuesday','Wednesday','Thursday','Friday')='Weekday'") 
  
df$day<-recode(df$day,"c('Sunday','Saturday')='Weekend'")  

df1<-aggregate(df$steps, by=list(interval=df$interval, day=df$day), mean, na.rm = TRUE)

ggplot(df1, aes(interval, x)) + ylab("Average number of steps") + geom_line() + facet_grid(day ~ .)
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

