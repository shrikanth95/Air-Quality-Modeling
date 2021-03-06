AQM Data Processing Functions
================
Authors
October 9, 2018

Notes:

-   Two data sets
    -   CO data from August used only to work with weekly seasonality.
    -   CO, Wind Speed and Wind Direction data from early September
-   Assumption: The programs are written for a minimum seasonality of 24 hours.

Varaibles required for running this tutorial
--------------------------------------------

``` r
C_data <- read.csv(file = 'Raw Sensor Data/Climo_co_corr_IISc_1_10.csv', header = TRUE, sep = ";")
cTime <-  as.POSIXct(C_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
ws_c <- as.numeric(mean(diff(cTime)))*60 # sampling frequecy (seconds)

xts.c <- xts(x = C_data$Value, order.by = cTime)
fault_window = "hours" # to identify faults in the data
std_tolerance = 0.01 # standard deviation for threshold for identifying constant values.
daily_threshold <- floor(24*3600/ws_c) # Expected number of samples per day
frequency_tolerance.c = 10
frequency_tolerance.w = 10
```

### Compute mode of an array

``` r
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
```

### Downsample an xts object based on averaging time

``` r
downSample <- function(data, avg_time, type = "non-periodic"){
  
  # data = xts.c
  # avg_time = 0.25
  if ((end(data)-start(data)) < (as.POSIXct(Sys.timeDate()) - as.POSIXct(Sys.timeDate() - 3600))) return()
  flag = 0
  num_days =  length(split(data, f  = "days"))
  start_time <- as.POSIXct(date(index(data[1])))-5.5*3600
  
  time_ref <- start_time + seq(0, num_days*24*3600, 3600*avg_time)
  time <- start_time + (3600*avg_time) + seq(0, (num_days* 24*3600 - 3600*avg_time), (3600*avg_time)) 
  down <- xts(array(-1, num_days*24/avg_time), order.by = time)
  
  if(type == "non-periodic"){
    for(i in 1:(num_days*24/avg_time)){
    temp <- window(data, start = time_ref[i], end = time_ref[i+1])
    coredata(down[i]) <- mean(coredata(temp))
    }
  }
  if(type == "periodic"){
    for(i in 1:(num_days*24/avg_time)){
      temp <- window(data, start = time_ref[i], end = time_ref[i+1])
      coredata(down[i]) <- Mode(coredata(temp))
    }
  }
  
  return(down)
}
```

-   Inputs:
    -   data (a uni variate xts object)
    -   Averaging time (in hours)
    -   Type of variable
        -   Periodic: Like wind directions, the down sampling is done by computing the mode.
        -   Non-periodic: Like wind speed and concentration, down sampling is done by computing the mean.
-   Output:
    -   An xts object that is down sampled

### Usage - Example with 15 min averaging time

``` r
avg_time  = 0.25
xts.ds.c <- downSample(xts.c, avg_time)
plot(xts.ds.c)
```

![](AQM_Functions_Summary_files/figure-markdown_github/unnamed-chunk-4-1.png)

### Usage - Example with 2 day averaging time

``` r
avg_time  = 2*24
xts.ds.c <- downSample(xts.c, avg_time)
plot(xts.ds.c)
```

![](AQM_Functions_Summary_files/figure-markdown_github/unnamed-chunk-5-1.png)

Construct master dataframe
--------------------------

``` r
get_ts_master_dataFrame <- function(fn.ts.wdir, fn.ts.conc, fn.ts.ws, fn.ts.tmp, avg_time){
  
  C_data <- read.csv(file = fn.ts.conc, header = TRUE, sep = ";")
  cTime <-  as.POSIXct(C_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  
  W_data.d <- read.csv(file = fn.ts.wdir, header = TRUE, sep = ";")
  wTime.d <-  as.POSIXct(W_data.d$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  
  W_data.s <- read.csv(file = fn.ts.ws, header = TRUE, sep = ";")
  wTime.s <-  as.POSIXct(W_data.s$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  
  T_data <- read.csv(file = fn.ts.tmp, header = TRUE, sep = ";")
  tTime <-  as.POSIXct(T_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")

  # C_data <- read.csv(file = 'Raw Sensor Data/Climo_co_corr_IISc_1_10.csv', header = TRUE, sep = ";")
  #   cTime <-  as.POSIXct(C_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  #   
  #   W_data.d <- read.csv(file = 'Raw Sensor Data/Wind_direction_corr_IISc_1_20.csv', header = TRUE, sep = ";")
  #   wTime.d <-  as.POSIXct(W_data.d$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  #   
  #   W_data.s <- read.csv(file = 'Raw Sensor Data/Wind_speed_corr_IISc_1_20.csv', header = TRUE, sep = ";")
  #   wTime.s <-  as.POSIXct(W_data.s$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  #   
  
  xts.c <- xts(x = C_data$Value, order.by = cTime)
  len_c <- length(xts.c)
  ws_c <- as.numeric(mean(diff(cTime)))*60 # sampling frequecy (seconds)
  
  xts.wd <- xts(x = W_data.d$Value, order.by = wTime.d)
  len_w.d <- length(xts.wd)
  ws_w.d <- as.numeric(mean(diff(wTime.d)))*60 # sampling frequecy (seconds)
  
  xts.ws <- xts(x = W_data.s$Value, order.by = wTime.s)
  len_w.s <- length(xts.ws)
  ws_w.s <- as.numeric(mean(diff(wTime.s)))*60 # sampling frequecy (seconds)
  
  xts.t <- xts(x = T_data$Value, order.by = tTime)
  len_t <- length(xts.t)
  ws_t <- as.numeric(mean(diff(tTime)))*60 # sampling frequecy (seconds)
  
  endTime = min(end(xts.wd), end(xts.c), end(xts.ws), end(xts.t))
  startTime = max(start(xts.wd), start(xts.c), start(xts.ws), start(xts.t))
  
  # Select the subset where all data is available 
  xts.ws = window(xts.ws, start= startTime, end = endTime)
  xts.c = window(xts.c, start= startTime, end = endTime)
  xts.wd = window(xts.wd, start= startTime, end = endTime)
  xts.t = window(xts.t, start= startTime, end = endTime)
  
  
  idx = endpoints(xts.ws,on  = "days")
  
  xts.ws = xts.ws[(idx[2]+1):idx[length(idx)-1]]
  
  idx = endpoints(xts.wd,on  = "days")
  xts.wd = xts.wd[(idx[2]+1):idx[length(idx)-1]]
  
  idx = endpoints(xts.c,on  = "days")
  xts.c = xts.c[(idx[2]+1):idx[length(idx)-1]]
  
  idx = endpoints(xts.t,on  = "days")
  xts.t = xts.t[(idx[2]+1):idx[length(idx)-1]]
  
  num_days = length(split(xts.ws, f = "days"))
  # Downsample
  xts.ds.c <- downSample(xts.c, avg_time)
  xts.ds.ws <- downSample(xts.ws, avg_time)
  xts.ds.wd <- downSample(xts.wd, avg_time, type = "periodic")
  xts.ds.t <- downSample(xts.t, avg_time)
    
  day_of_week <- character(num_days*(24/avg_time))
  week_of_set <- character(num_days*(24/avg_time))
  
  if (avg_time<24){
    for (i in 1:length(xts.ds.c)){
      day_of_week[i] <- weekdays(time(xts.ds.c[i]))
      week_of_set[i] <- ceiling(i/(7*24/avg_time))
      i = i+1
    }
  }
  
  c(length(xts.ds.c),length(week_of_set))
  time<- time(xts.ds.c)#, format="%H:%M:%S")
  df <- data.frame(time = time,
                   conc = xts.ds.c,
                   wdir = xts.ds.wd,
                   wspeed = xts.ds.ws,
                   temp = xts.ds.t,
                   minute = hour(time)*60+minute(time),
                   hour = hour(time),
                   dow = day_of_week,
                   wos = week_of_set)
  return(df)
  
}
```

-   Inputs:
    -   Name of file with wind direction data
    -   Name of file with concentration data,
    -   Name of file with wind speed data
    -   Averaging time in hours (float)
-   Outputs: Master data frame

### Usage

``` r
avg_time= 0.5 # Hours
df.new = get_ts_master_dataFrame('Raw Sensor Data/Wind_direction_corr_IISc_1_20.csv', 
                              'Raw Sensor Data/Climo_co_corr_IISc_1_10.csv', 
                              'Raw Sensor Data/Wind_speed_corr_IISc_1_20.csv', 
                              'Raw Sensor Data/Temperature_corr_IISc_29_11.csv',
                              avg_time)
head(df.new)
```

    ##                                    time      conc wdir    wspeed     temp
    ## 2018-09-01 00:30:00 2018-09-01 00:30:00 0.6780800  135 0.2266667 23.50180
    ## 2018-09-01 01:00:00 2018-09-01 01:00:00 0.5895200  270 0.3400000 23.39928
    ## 2018-09-01 01:30:00 2018-09-01 01:30:00 0.5497200  135 0.4571429 23.32840
    ## 2018-09-01 02:00:00 2018-09-01 02:00:00 0.5080800  135 0.4533333 23.25436
    ## 2018-09-01 02:30:00 2018-09-01 02:30:00 0.4929167  135 0.3200000 23.04583
    ## 2018-09-01 03:00:00 2018-09-01 03:00:00 0.4776800  135 0.3633333 22.93756
    ##                     minute hour      dow wos
    ## 2018-09-01 00:30:00     30    0 Saturday   1
    ## 2018-09-01 01:00:00     60    1 Saturday   1
    ## 2018-09-01 01:30:00     90    1 Saturday   1
    ## 2018-09-01 02:00:00    120    2 Saturday   1
    ## 2018-09-01 02:30:00    150    2 Saturday   1
    ## 2018-09-01 03:00:00    180    3 Saturday   1

Daily seasonality
-----------------

``` r
getSeasonality <- function(df.new, avg_time){
  refTime <- as.POSIXct(df.new$time)
  df.seasonal <- data.frame(conc = array(0, 24/avg_time), wdir = array(0, 24/avg_time), 
                            wspeed = array(0, 24/avg_time), temp = array(0, 24/avg_time))
  for(i in 1:(24/avg_time-1)){
    df.seasonal[i, ] <- unname(colMeans(df.new[df.new$minute==round(i*avg_time*60), 
                                               c(2,3,4, 5)],na.rm = TRUE))
  }
  i = 0
  df.seasonal[(24/avg_time), ] <- unname(colMeans(df.new[df.new$minute==round(i*avg_time*60), 
                                                         c(2,3,4,5)],na.rm = TRUE))
  df.seasonal$x <- refTime[1:(24/avg_time)]
  return(df.seasonal)
}
```

-   Inputs: Master Data frame, averaging time *τ*
-   Output: Data frame with $\\frac{24}{\\tau}$ rows, where *τ* is the averaging time.

**NOTE** This function is limited to daily averages. The similar function `getAverage_daily` is mean to extract daily and weekly seasonality and hence based on a dictionary implementation.

Usage
-----

``` r
df.seasonal <- getSeasonality(df.new, avg_time)
head(df.seasonal)
```

    ##        conc  wdir    wspeed     temp                   x
    ## 1 0.6567702 157.5 0.2158000 24.43474 2018-09-01 00:30:00
    ## 2 0.5644327 175.5 0.2854667 24.13142 2018-09-01 01:00:00
    ## 3 0.5454712 175.5 0.2018095 23.87182 2018-09-01 01:30:00
    ## 4 0.5004690 171.0 0.1663333 23.61484 2018-09-01 02:00:00
    ## 5 0.4819282 189.0 0.1321810 23.42170 2018-09-01 02:30:00
    ## 6 0.4504125 202.5 0.1539238 23.17674 2018-09-01 03:00:00

Compute and return Z score of an array
--------------------------------------

``` r
computeZscore <- function(vec){
  
  pop_sd <- sd(vec)*sqrt((length(vec)-1)/(length(vec)))
  pop_mean <- mean(vec)
  z <- (vec - pop_mean) / pop_sd
  return(z)
}
```

-   Input: Array *x*
-   Output: Z score of array
    $$Z = \\frac{x - \\bar{x}}{\\sigma(x)}$$

Dictionary of data
------------------

``` r
## Used to find regions of 
checkFlats <- function(sample_season){ 
  return(!(any(
    period.apply(sample_season, 
                 INDEX=endpoints(sample_season, on = fault_window),
                 FUN=sd) < std_tolerance))
  )
}
removeAnomaly <- function(data, threshold = daily_threshold, s_period = "daily", type = "Pollutant"){
  
  days_all<- split(data, f = "days") 
  data <- days_all[2:length(days_all)]
    
  if(s_period == "daily") {
    sea_dict <- list(Sunday = 0, Monday = 0, Tuesday = 0, Wednesday = 0, 
                     Thursday = 0, Friday = 0, Saturday = 0)
    sea_hist <- array(0, 7) # Histgram over each day of week
    names(sea_hist) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    flag = 0
  }
  
  else if (s_period == "weekly") {
    threshold = weekly_threshold
    week_idx <- array(0, 1)
    flag = 1
  }
  
  if (type == "Wind"){
    frequency_tolerance = frequency_tolerance.w
    std_tolerance = 100
  }
  
  
  if (type == "Pollutant"){
    frequency_tolerance = frequency_tolerance.c
  }
  
  num_correct = 1
  num_all = 1
  for(sample_season in data){
    if(length(sample_season)>(threshold - frequency_tolerance) && checkFlats(sample_season)){
      if (flag == 0){
        dow <- weekdays(start(sample_season)) # 
        sea_hist[dow] = sea_hist[dow] + 1
        sea_dict[[dow]] = c(sea_dict[[dow]], num_all)
      }
      
      else if (flag == 1){ # processing weekly data
        week_idx = c(week_idx, num_all)
      }
      num_correct = num_correct + 1
    }
    num_all = num_all + 1
  }
  
  if (flag == 0) {
    for (i in (1:7)){
      sea_dict[[i]] <- sea_dict[[i]][2:length(sea_dict[[i]])]
    }
    return(list(sea_hist, sea_dict))
  }
  
  if(flag == 1) {return(week_idx[2:length(week_idx)])}
  
}
```

Creates a dictionary with correct days classified based on the day of week

-   Inputs: Xts object
-   Outputs: list of two objects
    -   Named array of length 7 (representing the day of the week) and the value showing the number of days where is data is present for the corresponding name
    -   Named list of length 7 (representing the day of the week) where each item is a numeric array that shows the indexes of the xts object belonging to day

### Usage

``` r
days_summary <- removeAnomaly(xts.c)
days_summary
```

    ## [[1]]
    ##    Sunday    Monday   Tuesday Wednesday  Thursday    Friday  Saturday 
    ##         2         2         1         1         1         1         1 
    ## 
    ## [[2]]
    ## [[2]]$Sunday
    ## [1] 2 9
    ## 
    ## [[2]]$Monday
    ## [1]  3 10
    ## 
    ## [[2]]$Tuesday
    ## [1] 4
    ## 
    ## [[2]]$Wednesday
    ## [1] 5
    ## 
    ## [[2]]$Thursday
    ## [1] 6
    ## 
    ## [[2]]$Friday
    ## [1] 7
    ## 
    ## [[2]]$Saturday
    ## [1] 1

Get daily average based on dictionary
-------------------------------------

``` r
getAverage_daily<- function(days, day_dict, avg_time, week_flag, CI){
  
  missing.CI = missing(CI)
  missing.week_flag = missing(week_flag)
  
  ds <- array(0, c(24/avg_time, 7))
  daily_mean <- array(0, c(7, week_flag))
  
  if(!missing.CI) quant_inf <- array(0, c(24/avg_time, 7))
  
  for (i in 1:7){ 
    
    dow <- day_dict[[i]] 
    num_days = length(dow[1:week_flag])
    if(missing.week_flag) {
      week_flag <- num_days
    }
    
    if(!missing.CI) quant <- array(0, c(24/avg_time, week_flag))
    
    k = 1
    for (sample_day_idx in dow[1:week_flag]){
      
      C_ds <- downSample(days[[sample_day_idx]], avg_time)
      daily_mean[i, k] = mean(coredata(C_ds))
      ds[, i] = ds[, i] + coredata(C_ds)/num_days
      if(!missing.CI) quant[, k] <- coredata(C_ds)
      k = k + 1
    }
    if (!missing.CI){
      for(idx in 1:(24/avg_time)){
        quant_inf[idx, i] <- quantile(quant[idx, ], CI)
      }
    }
  }
  seasonal_dow <- xts(ds, order.by = time(C_ds))
  
  if (!missing.CI) {
    quantile_summ <- xts(quant_inf, order.by = time(C_ds))
    return(list(average = seasonal_dow, mean = daily_mean, quantile_summ = quantile_summ))
  }
  
  return(list(average = seasonal_dow, mean = daily_mean))
}
```

The function is used to compute the weekly seasonal data based on the dictionary implementation.

-   Inputs:
    -   Days: Xts object that has been split into lists by days (ie. `days=split(data, f = "days")`)
    -   Day\_dict: Named list of length 7 (representing the day of the week) where each item is a numeric array that shows the indexes of the xts object belonging to day. Also the output of the removeAnomaly function.
    -   Avg\_time: Averaging time in Hours
    -   Number of weeks of interest
    -   CI: Desired confidence interval. Should not be an array.
-   Outputs: List of two items
    -   Matrix of 7 columns (each column representing a day of the week) with $\\frac{24}{\\tau}$ number of rows, where *τ* is the averaging time

### Usage

``` r
day_h <- days_summary[[1]]
day_dict <- days_summary[[2]]
    
week_flag = min(day_h)
days_all<- split(xts.c, f = "days") # A list of lists storing data for each of the 38 days 
days <- days_all[2:length(days_all)]
    
data_summary <- getAverage_daily(days, day_dict, avg_time, week_flag)
seasonal_dow <- data_summary$average
```

Fetch seasonal data
-------------------

``` r
getSeasonalData_August <- function(avg_time){
  
  destfile=paste('TSA Cached data/sea_CO_aug_',
                 as.character(avg_time*60),'.csv', sep="")
  
  if (!file.exists(destfile)) {
    C_data <- read.csv(file = 'Raw Sensor Data/Climo_CO_23.csv', header = TRUE, sep = ";")
    cTime <-  as.POSIXct(C_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
    
    C_xts <- xts(x = C_data$Value, order.by = cTime)
    
    # A list of lists storing data for each of the 38 days 
    
    days_summary <- removeAnomaly(C_xts)
    day_h <- days_summary[[1]]
    day_dict <- days_summary[[2]]
    
    week_flag = min(day_h)
    days_all<- split(C_xts, f = "days") # A list of lists storing data for each of the 38 days 
    days <- days_all[2:length(days_all)]
    
    data_summary <- getAverage_daily(days, day_dict, avg_time, week_flag)
    seasonal_dow <- data_summary$average
    df.sea <- data.frame(Sunday = seasonal_dow[,1],
                         Monday = seasonal_dow[,2],
                         Tuesday = seasonal_dow[,3],
                         Wednesday = seasonal_dow[,4],
                         Thursday = seasonal_dow[,5],
                         Friday = seasonal_dow[,6],
                         Saturday = seasonal_dow[,7])
    t <- as.POSIXct(rownames(df.sea))
    df.sea$time <- t
    df.sea.m <- melt(df.sea,id.vars = "time",variable.name = "dow", value.name = "conc")
    t <- df.sea.m$time
    df.sea.m$minute = hour(t)*60+minute(t)
    df.sea.m$hour = hour(t)
    
    write.csv(df.sea.m, file = destfile, row.names = FALSE)
  }
  
  return(read.csv(file = destfile, header = TRUE, sep = ","))
  
}
```

For an averaging time *τ*, the function computes the 24 hour seasonality over the week from the August data set and also stores it in a .csv file with title where *x* = *τ* × 60 to avoid re computation -- similar to a cache.

-   Input: Averaging time *τ*
-   Output: Data frame of length $\\frac{ 7 \\times 24}{\\tau}$

### Usage

``` r
df.sea.w <- getSeasonalData_August(avg_time)
head(df.sea.w)
```

    ##                  time    dow      conc minute hour
    ## 1 2018-09-16 00:30:00 Sunday 0.6292700     30    0
    ## 2 2018-09-16 01:00:00 Sunday 0.5299137     60    1
    ## 3 2018-09-16 01:30:00 Sunday 0.5069832     90    1
    ## 4 2018-09-16 02:00:00 Sunday 0.5048700    120    2
    ## 5 2018-09-16 02:30:00 Sunday 0.4626000    150    2
    ## 6 2018-09-16 03:00:00 Sunday 0.4403979    180    3

Master data frame from September dataset
----------------------------------------

``` r
getMasterdf_September <- function(avg_time){
  destfile=paste('TSA Cached data/master_sep_',
                 as.character(avg_time*60),'.csv', sep="")
  
  if (!file.exists(destfile)) {
    df = get_ts_master_dataFrame('Raw Sensor Data/Wind_direction_corr_IISc_1_20.csv', 
                              'Raw Sensor Data/Climo_co_corr_IISc_1_10.csv', 
                              'Raw Sensor Data/Wind_speed_corr_IISc_1_20.csv', 
                              avg_time)
    
    write.csv(df, file = destfile, row.names = FALSE)
  }
  
  return(read.csv(file = destfile, header = TRUE, sep = ","))
}
```

Computes the master data frame from the early September data set and stores it in a .csv file where *x* = *τ* × 60 to avoid re computation.

-   Input: Averaging time *τ*
-   Output: Data frame

Pad data set with weekly Seasonality
------------------------------------

``` r
pad_df_weeklySea <- function(df, df.sea.w){
  
  sea.w <- array(0, nrow(df))
    
  for(dow in c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")){
    idx <- which((df$dow==dow) == TRUE)
    sea.w[idx] <- rep(df.sea.w$conc[idx], length(idx)/(24/avg_time))
  }
return(sea.w)
}
```

For a data set `df.new`, the function returns a column with weekly seasonality for each day of the `df.new` data frame. For eg. for a 3 day data-set with `{Sunday, Monday, Tuesday}`, the function returns an array `{Seasonal Sunday, Seasonal Monday, Seasonal Tuesday}`

### Usage

``` r
df.sea.w <- getSeasonalData_August(avg_time) # Get seasonal data

df.new$sea.w <- pad_df_weeklySea(df.new, df.sea.w)

df.new$desea.w <- df.new$conc - df.new$sea.w # Deseasoned by weekly sesonality

head(df.new)
```

    ##                                    time      conc wdir    wspeed     temp
    ## 2018-09-01 00:30:00 2018-09-01 00:30:00 0.6780800  135 0.2266667 23.50180
    ## 2018-09-01 01:00:00 2018-09-01 01:00:00 0.5895200  270 0.3400000 23.39928
    ## 2018-09-01 01:30:00 2018-09-01 01:30:00 0.5497200  135 0.4571429 23.32840
    ## 2018-09-01 02:00:00 2018-09-01 02:00:00 0.5080800  135 0.4533333 23.25436
    ## 2018-09-01 02:30:00 2018-09-01 02:30:00 0.4929167  135 0.3200000 23.04583
    ## 2018-09-01 03:00:00 2018-09-01 03:00:00 0.4776800  135 0.3633333 22.93756
    ##                     minute hour      dow wos     sea.w    desea.w
    ## 2018-09-01 00:30:00     30    0 Saturday   1 0.6292700 0.04881000
    ## 2018-09-01 01:00:00     60    1 Saturday   1 0.5299137 0.05960625
    ## 2018-09-01 01:30:00     90    1 Saturday   1 0.5069832 0.04273683
    ## 2018-09-01 02:00:00    120    2 Saturday   1 0.5048700 0.00321000
    ## 2018-09-01 02:30:00    150    2 Saturday   1 0.4626000 0.03031667
    ## 2018-09-01 03:00:00    180    3 Saturday   1 0.4403979 0.03728208
