## Introduction

This report analyzes data from a personal activity monitoring device.
The dataset contains the number of steps taken in 5-minute intervals
each day.

**The goals of this analysis are to:**

-   Load and preprocess the data

-   Investigate the total number of steps taken per day

-   Explore the average daily activity pattern

-   Impute missing values and assess their impact

-   Compare activity patterns between weekdays and weekends

-   All code used to generate the results is shown in the report.

### 1. Loading and preprocessing the data Writing

    activity<- read.csv("C:/Users/nsookraj/Documents/R_projects/Reproducible Project 1/activity.csv")

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    summary(activity)

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

    activity <- activity %>%
    mutate(
    date = as.Date(date),
    interval = as.integer(interval)
    )

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### 3. What is the mean total number of steps taken per day?

For this part of the assignment, we ignore missing values in the
dataset.

#### 3.1 Total number of steps taken per day Writing

    total_steps_per_day <- activity %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = TRUE))

    head(total_steps_per_day)

    ## # A tibble: 6 × 2
    ##   date       steps
    ##   <date>     <int>
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

#### 3.2 Histogram of total steps per day (ggplot) Writing

    ggplot(total_steps_per_day, aes(x = steps)) +
    geom_histogram(binwidth = 1000, boundary = 0, color = "black", fill = "white") +
    labs(
    title = "Histogram of Total Steps per Day",
    x = "Total steps per day",
    y = "Number of days"
    )

![](report_files/figure-markdown_strict/unnamed-chunk-3-1.png)

#### 3.3 Mean and median total number of steps per day

    mean_total <- mean(total_steps_per_day$steps)
    median_total <- median(total_steps_per_day$steps)

    mean_total

    ## [1] 9354.23

    median_total

    ## [1] 10395

In this dataset, the mean total number of steps taken per day is r
round(mean\_total, 2) steps and the median is r round(median\_total, 2)
steps.

### 4. What is the average daily activity pattern?

Here we examine how activity varies across the 5-minute intervals of the
day.

#### 4.1 Average number of steps per interval (across all days) Writing

    avg_steps_interval <- activity %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = TRUE))

    head(avg_steps_interval)

    ## # A tibble: 6 × 2
    ##   interval  steps
    ##      <int>  <dbl>
    ## 1        0 1.72  
    ## 2        5 0.340 
    ## 3       10 0.132 
    ## 4       15 0.151 
    ## 5       20 0.0755
    ## 6       25 2.09

#### 4.2 Time series plot of 5-minute interval and average steps (ggplot) Writing

    ggplot(avg_steps_interval, aes(x = interval, y = steps)) +
    geom_line() +
    labs(
    title = "Average Daily Activity Pattern",
    x = "5-minute interval",
    y = "Average number of steps"
    )

![](report_files/figure-markdown_strict/unnamed-chunk-6-1.png)

#### 4.3 Interval with the maximum average number of steps Writing

    max_idx <- which.max(avg_steps_interval$steps)
    max_interval <- avg_steps_interval$interval[max_idx]
    max_interval

    ## [1] 835

On average across all days, the 5-minute interval with the maximum
number of steps is interval r max\_interval.

### 5. Imputing missing values

The dataset contains missing values (NA) in the steps variable. These
missing values may bias some calculations, so we will impute them.

#### 5.1 Total number of missing values in the dataset Writing

    total_na <- sum(is.na(activity$steps))
    total_na

    ## [1] 2304

There are r total\_na missing step values in the dataset.

#### 5.2 Strategy for imputing missing values

We use the following strategy:

Replace each missing steps value with the mean number of steps for that
5-minute interval, calculated across all days.

This preserves the overall daily activity pattern shape.

#### 5.3 Create a new dataset with imputed values

First compute the mean steps per interval:

    interval_means <- activity %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps, na.rm = TRUE))

Now merge these means into the original dataset and replace missing
values:

    activity_imputed <- activity %>%
    left_join(interval_means, by = "interval") %>%
    mutate(
    steps = ifelse(is.na(steps), mean_steps, steps),
    date = as.Date(date) # ensure date is Date class
    ) %>%
    select(steps, date, interval)

    sum(is.na(activity_imputed$steps)) # should be 0

    ## [1] 0

#### 5.4 Histogram of total steps per day (after imputation) Writing

    total_steps_imputed <- activity_imputed %>%
    group_by(date) %>%
    summarise(steps = sum(steps))

    head(total_steps_imputed)

    ## # A tibble: 6 × 2
    ##   date        steps
    ##   <date>      <dbl>
    ## 1 2012-10-01 10766.
    ## 2 2012-10-02   126 
    ## 3 2012-10-03 11352 
    ## 4 2012-10-04 12116 
    ## 5 2012-10-05 13294 
    ## 6 2012-10-06 15420

    ggplot(total_steps_imputed, aes(x = steps)) +
    geom_histogram(binwidth = 1000, boundary = 0, color = "black", fill = "white") +
    labs(
    title = "Histogram of Total Steps per Day (After Imputation)",
    x = "Total steps per day",
    y = "Number of days"
    )

![](report_files/figure-markdown_strict/unnamed-chunk-12-1.png)

#### 5.5 Mean and median after imputing missing values Writing

    mean_imputed <- mean(total_steps_imputed$steps)
    median_imputed <- median(total_steps_imputed$steps)

    mean_imputed

    ## [1] 10766.19

    median_imputed

    ## [1] 10766.19

Mean (original, with NAs ignored): r round(mean\_total, 2)

Median (original, with NAs ignored): r round(median\_total, 2)

Mean (after imputation): r round(mean\_imputed, 2)

Median (after imputation): r round(median\_imputed, 2)

#### 5.6 Impact of imputing missing data

Imputing missing values using the interval mean:

Leaves the mean essentially unchanged

Usually moves the median closer to the mean

Reduces the number of days with very low or zero total steps (since
missing days are no longer treated as having 0 steps)

Overall, imputation makes the distribution of total steps per day
smoother and less variable, but preserves the general activity level.

### 6. Are there differences in activity patterns between weekdays and weekends?

For this part, we use the imputed dataset.

#### 6.1 Create a factor variable for weekday vs weekend Writing

    activity_imputed <- activity_imputed %>%
    mutate(
    day_type = ifelse(
    weekdays(date) %in% c("Saturday", "Sunday"),
    "weekend",
    "weekday"
    ),
    day_type = factor(day_type)
    )

    table(activity_imputed$day_type)

    ## 
    ## weekday weekend 
    ##   12960    4608

#### 6.2 Average steps per interval by weekday/weekend Writing

    interval_daytype <- activity_imputed %>%
    group_by(interval, day_type) %>%
    summarise(steps = mean(steps), .groups = "drop")

head(interval\_daytype)

#### 6.3 Panel plot: weekday vs weekend activity patterns (ggplot) Writing

    ggplot(interval_daytype, aes(x = interval, y = steps)) +
    geom_line() +
    facet_wrap(~ day_type, ncol = 1) +
    labs(
    title = "Average Daily Activity Patterns: Weekday vs Weekend",
    x = "5-minute interval",
    y = "Average number of steps"
    )

![](report_files/figure-markdown_strict/unnamed-chunk-16-1.png)

#### 6.4 Interpretation

From the panel plot, we can observe that:

On weekdays, the activity pattern tends to have a more pronounced peak
during certain hours (e.g., typical commuting or working hours).

On weekends, the pattern may appear more spread out or shifted,
suggesting different daily routines.

Thus, there are noticeable differences in activity patterns between
weekdays and weekends.
