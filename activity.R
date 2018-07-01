library(dplyr)
library(ggplot2)

## 1    Loading the raw data

        #1.a) Read activity data. The assumption is that the file is available in the working directory
        activity <- read.csv(file=".\\activity.csv", header=TRUE, sep=",")

        #1.b) copy the data to other variables for reference.
        activity_original <- activity
        activity_na_filled <- activity
        
        #1.c) Remove NA values from data. activiy data set contains NA, remove all NA for further analysis
        activity<-activity[which(is.na(activity$steps)==FALSE),]

## 2    What is mean total number of steps taken per day?
        
        #2.a) Calculate the total number of steps taken per day
        steps_per_day <-aggregate(x = activity$steps, by = list(Day=activity$date), FUN = sum)

        #2.b) Label the result columns
        names(steps_per_day)<- c("day", "total_steps")

        #2.c) Make a histogram of the total number of steps taken each day
        hist(
                x=steps_per_day$total_steps,
                col="blue",
                xlab="Total number of steps taken each day",
                ylab="count",
                main="Hisotgram of steps taken each day"
        )

        #2.d) Calculate and report the mean and median of the total number of steps taken per day
        mean_steps_per_day <- mean(steps_per_day$total_steps)
        median_steps_per_day <- median(steps_per_day$total_steps)

##3     What is the average daily activity pattern?
        
        #3.a) Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
        interval_average_step<-aggregate(x = activity$steps, by = list(interval=activity$interval), FUN = mean)

        #3.b) Name the columns
        names(interval_average_step) <- c("interval","average_steps")

        #3.c) Make an average activity plot
        plot(interval_average_step$interval, interval_average_step$average_steps, 
             type="l",
             xlab="Interval",
             ylab="Average steps taken",
             main="Average steps taken during 5 minute interval")

        #3.d) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        interval_with_max_steps <- interval_average_step$interval[which.max(interval_average_step$average_steps)]        
 
##4     Inputing missing values      
        
        #4.a) Calculate and report the total number of missing values in the dataset 
        missing_entries <-length(which(is.na(activity_original$steps)==TRUE))
        
        #4.b) The strategy for missing values is to put the interval's average rounded to integer
        for (i in 1:nrow(activity_na_filled)) {
                if (is.na(activity_na_filled$steps[i])) {
                        # Find the index value for when the interval matches the average
                        index <- which(activity_na_filled$interval[i] == interval_average_step$interval)
                        # Assign the value to replace the NA
                        activity_na_filled$steps[i] <- round(interval_average_step[index,]$average_steps)
                }
        }
        
        #4.c) Create a new dataset that is equal to the original dataset but with the missing data filled in.
        #activity_na_filled is the new data set
        
        #4.d) Make a histogram of the total number of steps taken each day and Calculate and report the 
        #mean and median total number of steps taken per day. 
        #Do these values differ from the estimates from the first part of the assignment? 
        #What is the impact of imputing missing data on the estimates of the total daily number of steps?
      
                #4d.i) Calculate the total number of steps taken per day after replacing NAs
                na_filled_steps_per_day <-aggregate(x = activity_na_filled$steps, by = list(Day=activity_na_filled$date), FUN = sum)
        
                #4d.ii) Label the result columns
                names(na_filled_steps_per_day)<- c("day", "total_steps")
        
                #4d.iii) hist with NA filled
                hist(
                        x=na_filled_steps_per_day$total_steps,
                        col="blue",
                        xlab="Total number of steps taken each day",
                        ylab="count",
                        main="Hisotgram of steps taken each day"
                )
        
                #4.e) Calculate and report the mean and median of the total number of steps taken per day after replacing NAs
                na_filled_mean_steps_per_day <- mean(na_filled_steps_per_day$total_steps)
                na_filled_median_steps_per_day <- median(na_filled_steps_per_day$total_steps)
        
##5     Are there differences in activity patterns between weekdays and weekends?
        
        #5.a) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
                activity_na_filled$date <- as.Date(activity_na_filled$date)
                
                activity_na_filled$dayofweek <- 
                                                sapply(weekdays(activity_na_filled$date), switch, 
                                  Monday = "Weekday", 
                                  Tuesday = "Weekday", 
                                  Wednesday = "Weekday", 
                                  Thursday = "Weekday",
                                  Friday = "Weekday",
                                  Saturday = "Weekend",
                                  Sunday = "Weekend"
                                  )
                
                dayaverage <- activity_na_filled %>%
                        group_by(dayofweek, interval) %>%
                        summarize(AverageSteps=mean(steps))
        #5.b) Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
        #     & average number of steps taken, averaged across all weekdays or weekends(y-axis).
               
                
                # Use qplot because facets easily divides the graph into two better than
                # the base graphics system.
                qplot(interval, AverageSteps, data=dayaverage,
                      type="l",
                      geom="line",
                      xlab="Interval",
                      ylab="Number of Steps (Average)",
                      main="Average steps taken Weekends vs. Weekdays",
                      facets =dayofweek ~ .)                  
                
                
        