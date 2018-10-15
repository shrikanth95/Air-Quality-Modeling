## This file contains the functions that are used in the main file

### Compute mode of an array
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
 
# getSummary <- function(days){
#   
#   day_hist <- array(0, 7) # days of week
#   names(day_hist) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
#   i = 1
#   day_list <- array(0)
#   for(day in days){
#     day_hist[weekdays(start(day))] = day_hist[weekdays(start(day))] + 1
#     day_list[i] = start(day)
#     i = i + 1
#   }
#   return(list(day_hist, day_list))
# }

## Checks if there are flats line in the data based on the standard deviation threshold
# Input: Sample day
# Output: TRUE or FALSE depending on the presence of faults in data
checkFlats <- function(sample_season){ 
  return(!(any(
    period.apply(sample_season, 
                 INDEX=endpoints(sample_season, on = fault_window),
                 FUN=sd) < std_tolerance))
  )
}

# Creates a dictionary with correct days classified based on the day of week
# 
# - Inputs: Xts object 
# - Outputs: list of two objects
#   - Named array of length 7 (representing the day of the week) and the value showing the number of days where is data is present for the corresponding name
#   - Named list of length 7 (representing the day of the week) where each item is a numeric array that shows the indexes of the xts object belonging to day

removeAnomaly <- function(data, threshold = daily_threshold, s_period = "daily", type = "Pollutant"){
  # data = C_xts
  data = split(data, f = "days")
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

### Downsample an xts object based on averaging time
# - Inputs: 
#   - data (a univariate xts object)
#   - Averaging time (in hours)
#   - Type of varaiable
#   - Periodic: Like wind directions, the downsampling is done by computing the mode.  
#   - Non-periodic: Like wind speed and concentration, downsampling is done by computing the mean.
# - Output:
#   - An xts object that is downsampled 

downSample <- function(data, avg_time, type = "non-periodic"){
  # data = days[[sample_day_idx]]
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

# The function is used to compute the weekly seasonal data based on the dictionary implementation.  
# 
# - Inputs:
#   - Days: Xts object that has been split into lists by days (ie. `days=split(data, f = "days")`)
#   - Day_dict: Named list of length 7 (representing the day of the week) where each item is a numeric array that shows the indexes of the xts object belonging to day.  Also the output of the removeAnomaly function.
#   - Avg_time: Averaging time in Hours
#   - Number of weeks of interest 
#   - CI: Desired confidence interval.  Sould not be an array.
# 
# - Outputs: List of two items
#   - Matrix of 7 columns (each column represeting a day of the week) with $\frac{24}{\tau}$ number of rows, where $\tau$ is the averaging time



getAverage_daily<- function(days, day_dict, avg_time, week_flag, CI){
  
  missing.CI = (CI==-1)
  missing.week_flag = missing(week_flag)
  
  ds <- array(0, c(24/avg_time, 7))
  daily_mean <- array(0, c(7, week_flag))
  
  if(!missing.CI) quant_inf <- array(0, c(24/avg_time, 7))
  
  for (i in 1:7){ 
    
    dow <- day_dict[[i]] 
    num_days = length(dow[1:week_flag])
    # if(missing.week_flag) {
    #   week_flag <- num_days
    # }
    
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

## 
# Daily seasonality
# 
# - Inputs: Master Data frame, averaging time $\tau$
#   - Output: Data frame with $\frac{24}{\tau}$ rows, where $\tau$ is the averaging time.

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


# Compute and return Z score of an array
# 
# - Input: Array $x$
# - Output: Z score of array \[Z = \frac{x - \bar{x}}{\sigma(x)}\]

computeZscore <- function(vec){
  
  pop_sd <- sd(vec)*sqrt((length(vec)-1)/(length(vec)))
  pop_mean <- mean(vec)
  z <- (vec - pop_mean) / pop_sd
  return(z)
}

# For a dataset `df.new`, the function returns a column with weekly seasonality for each day of the `df.new` data frame.  For  eg. for a 3 day data-set with `{Sunday, Monday, Tuesday}`, the funciton returns an array `{Seasonal Sunday, Seasonal Monday, Seasonal Tuesday}`

pad_df_weeklySea <- function(df.new, df.sea.w){
  
  sea.w <- array(0, nrow(df.new))
    
  for(dow in c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")){
    idx <- which((df.new$dow==dow) == TRUE)
    sea.w[idx] <- rep(df.sea.w$conc[df.sea.w$dow==dow], length(idx)/(24/avg_time))
  }
return(sea.w)
}

getLongDataFrame <- function(seasonal_dow, quantile_high, quantile_low){
  test <- cbind(seasonal_dow, quantile_high, quantile_low)
  
  means <- fortify(seasonal_dow)
  names(means)<- c("Time", names(day_dict))
  
  qlow <- fortify(quantile_low)
  names(qlow)<- c("Time", names(day_dict))
  
  qhigh <- fortify(quantile_high)
  names(qhigh)<- c("Time", names(day_dict))
  
  
  means.m <- melt(means,id.vars = "Time", value.name = "Concentrations", variable.name = "day")
  qlow.m <- melt(qlow, id.vars = "Time", value.name = "Concentrations", variable.name = "day")
  qhigh.m <- melt(qhigh,id.vars = "Time", value.name = "Concentrations", variable.name = "day")
  
  
  
  means.m$type <- "mean"
  qlow.m$type <- "5% Quantile"
  qhigh.m$type <- "95% Quantile"
  
  
  all_tmp <- merge(means.m, qlow.m, all= TRUE)
  all_data <- merge(qhigh.m, all_tmp, all = TRUE)
  return(all_data)
}

## NOTE: THIS FUNCTIONS FROM THIS POINT ARE ONLY FOR REFERENCE AND IS NOT USED

getDayofWeek_summ<- function(day_of_week, days, day_dict, avg_time, week_flag){
  
  if(missing(week_flag)){
    week_flag = length(day_dict[[day_of_week]])
  }
  
  
  daily_ds <- array(0, c(24/avg_time, week_flag))
  
  k = 1
  for (dow in day_dict[[day_of_week]]){ 
    C_ds <- downSample(days[[dow]], avg_time)
    daily_ds[, k] = coredata(C_ds)
    
    k = k + 1
  }
  
  seasonal_dow <- xts(daily_ds, order.by = time(C_ds))
  return(list(daily_ds = seasonal_dow))
}

## COmputes the moving average of a data set.  
compute_MA <- function(days, day_dict, MA_time, week_flag){
  missing.week_flag = missing(week_flag)
  # daily_threshold <- floor(24*3600/ws)
  
  sea_dict <- list(Sunday = 0, Monday = 0, Tuesday = 0, Wednesday = 0, 
                   Thursday = 0, Friday = 0, Saturday = 0)
  ma <- array(0, c(daily_threshold, 7))
  
  threshold <- floor(3800*MA_time/ws)
  sample_len = floor(MA_time*3600/ws)
  for (i in 1:7){ 
    num_all = 1
    dow <- day_dict[[i]] 
    num_days = length(dow)
    k = 1
    num_correct = 1
    for (sample_day_idx in dow[1:week_flag]){
      day <- days[[sample_day_idx]]
      sample_window <- MA_time*3800
      spill_window = which( time( days[[sample_day_idx - 1]]) > (start(day) - sample_window))
      spill_b <- days[[sample_day_idx - 1]][spill_window]
      spill_window = which( time( days[[sample_day_idx + 1]]) < (end(day) + sample_window))
      spill_f <- days[[sample_day_idx + 1]][spill_window]
      
      if((length(spill_f)+length(spill_b))>(2*threshold - 6)){
        
        ma_window <- rbind(spill_b, day, spill_f)
        ma_xts <- stats::filter (ma_window, filter = rep((1/sample_len), sample_len), sides = 2)
        ma_xts <- xts(ma_xts, order.by = time(ma_window))
        plot(cbind(ma_window, ma_xts))
        idx <- endpoints(ma_xts, on = "days")[2:3]
        C_ma <- ma_xts[idx[1]:idx[2]]
        ma[, i] = ma[, i] + coredata(C_ma[1:daily_threshold])/num_days
        num_correct = num_correct + 1
        
        day_of_week <- weekdays(start(day))  
        sea_dict[[day_of_week]] = c(sea_dict[[day_of_week]], num_all)
      }
      
      num_all = num_all + 1
    }
  }
  seasonal_ma <- xts(ma, order.by = time(C_ma[1:daily_threshold]))
  return(list(seasonal_ma = seasonal_ma, dict_ma = sea_dict))
}


get_day_labels <- function(day_of_week, days, day_dict, week_flag){
  day_lables <- day_dict[[day_of_week]]
  tmp = NULL
  for (lab in day_lables[1:week_flag]){
    tmp <- c(tmp, as.character(date(start(days[[lab]]))))
  }
  return(tmp)
}

getDayofWeek_summ<- function(day_of_week, days, day_dict, avg_time, week_flag){
  
  if(missing(week_flag)){
    week_flag = length(day_dict[[day_of_week]])
  }
  
  
  daily_ds <- array(0, c(24/avg_time, week_flag))
  
  k = 1
  for (dow in day_dict[[day_of_week]][1:week_flag]){
    C_ds <- downSample(days[[dow]], avg_time)
    daily_ds[, k] = coredata(C_ds)
    
    k = k + 1
  }
  
  seasonal_dow <- xts(daily_ds, order.by = time(C_ds))
  return(list(daily_ds = seasonal_dow))
}



