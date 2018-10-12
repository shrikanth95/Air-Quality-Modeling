
## Prepare master dataframe
# 
# - Inputs: 
#   - Name of file with wind direction data 
#   - Name of file with concentration data,
#   - Name of file with wind speed data 
#   - Averaging time in hours (float)
# - Outputs: Master data frame 

get_ts_master_dataFrame <- function(fn.ts.wdir, fn.ts.conc, fn.ts.ws, avg_time){
  
  C_data <- read.csv(file = fn.ts.conc, header = TRUE, sep = ";")
  cTime <-  as.POSIXct(C_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  
  W_data.d <- read.csv(file = fn.ts.wdir, header = TRUE, sep = ";")
  wTime.d <-  as.POSIXct(W_data.d$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  
  W_data.s <- read.csv(file = fn.ts.ws, header = TRUE, sep = ";")
  wTime.s <-  as.POSIXct(W_data.s$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
  
  
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
  
  endTime = min(end(xts.wd), end(xts.c), end(xts.ws))
  startTime = max(start(xts.wd), start(xts.c), start(xts.ws))
  
  # Select the subset where all data is available 
  xts.ws = window(xts.ws, start= startTime, end = endTime)
  xts.c = window(xts.c, start= startTime, end = endTime)
  xts.wd = window(xts.wd, start= startTime, end = endTime)
  
  
  
  idx = endpoints(xts.ws,on  = "days")
  
  xts.ws = xts.ws[(idx[2]+1):idx[length(idx)-1]]
  
  idx = endpoints(xts.wd,on  = "days")
  xts.wd = xts.wd[(idx[2]+1):idx[length(idx)-1]]
  
  idx = endpoints(xts.c,on  = "days")
  xts.c = xts.c[(idx[2]+1):idx[length(idx)-1]]
  
  num_days = length(split(xts.ws, f = "days"))
  # Downsample
  xts.ds.c <- downSample(xts.c, avg_time)
  xts.ds.ws <- downSample(xts.ws, avg_time)
  xts.ds.wd <- downSample(xts.wd, avg_time, type = "periodic")
  
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
                   minute = hour(time)*60+minute(time),
                   hour = hour(time),
                   dow = day_of_week,
                   wos = week_of_set)
  return(df)
  
}

# For an averaging time $\tau$, the function computes the 24 hour seasonality over the week from the August dataset and also stores it in a .csv file with title \textit{sea\_CO\_aug\_x.csv} where $x = \tau \times 60$ to avoid recomputation -- similar to a cache.
# 
# - Input: Averaging time $\tau$
# - Output: Data frame of length $\frac{ 7 \times 24}{\tau}$
  
  
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

# Computes the master data frame from the early September dataset and stores it in a .csv file \textit{master\_sep\_x.csv} where $x = \tau \times 60$ to avoid recomputation.
# 
# - Input: Averaging time $\tau$
# - Output: Data frame

getMasterdf_September <- function(avg_time){
  destfile=paste('TSA Cached data/master_sep_',
                 as.character(avg_time*60),'.csv', sep="")
  if (file.exists(destfile)) {
    return(read.csv(file = destfile, header = TRUE, sep = ","))
  }
  df = get_ts_master_dataFrame('Raw Sensor Data/Wind_direction_corr_IISc_1_20.csv', 
                               'Raw Sensor Data/Climo_co_corr_IISc_1_10.csv', 
                               'Raw Sensor Data/Wind_speed_corr_IISc_1_20.csv', 
                               avg_time)
  
  write.csv(df, file = destfile, row.names = FALSE)
  return(df)
}
