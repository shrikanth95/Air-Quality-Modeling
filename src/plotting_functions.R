# Functions used to recreate all the plots.

library(ggplot2)
library(reshape2)
library(corrplot)
library(plotrix)
# source("TSA_source.R")

## Plots the seasonality according to hour of the day of each day of the week.
# Note: works only for the August data frame which is the output of the getSeasonalData_August() function

# Inputs:
#   - master data frame

plot.All_seasonality <- function(df.sea.w, type = "overview", title="Daily and Weekly Characteristics", folder = "plots", formats=c("PDF", "PNG")){
  avg_time <- as.numeric(as.POSIXct(df.sea.w$time[2])-as.POSIXct(df.sea.w$time[1]))/60
  if(type == "all"){
    plt <- ggplot(df.sea.w, aes(x = hour, y =  conc, color= dow))+
      ylab('Conc. (ppm)')+
      xlab("Time (Hours)")+
      geom_line(size = 2)+geom_point(size = 2.5) + 
      theme(axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16), 
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15))+
      labs(color = "Day of Week")
  }
  if(type == "overview"){
    plt <- ggplot(df.sea.w, aes(x = hour, y =  conc))+
      ylab('Conc. (ppm)')+
      xlab("Time (Hours)")+
      geom_line(size = 1)+geom_point(size = 1.5) + 
      facet_wrap(.~dow, ncol= 3)+ theme_grey(base_size = 15) 
      # theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16))
  }
  plot.folder <- paste(folder,"/All_seasonality/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"type=",type,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}

## Plots the summary of the master data set
# Input:  Master and the seasonal data frame

plot.dataset<-function(df.new, df.seasonal, df.specs, avg_time, folder = "plots", formats=c("PDF", "PNG")){
  df.new$sea <- rep(df.seasonal$conc, nrow(df.new)/(24/avg_time))
  
  df.new$desea <- df.new$conc - df.new$sea
  
  # df.new.1 <- df.new[df.new$wos==, ] 
  # df.new.1 <- df.new.1[df.new.1$dow!="Saturday", ] 
  # df.new.1 <- df.new.1[df.new.1$dow!="Sunday",c(1,2,4,5, 11) ] 
  
  df.new.1 <- df.new[,c(1,2,4,5, 11) ] 
  df.new.1 <- df.new.1[,c(1,2,5,3,4)]
  df.new.1$time <- as.POSIXct(df.new.1$time)
  df.new.1.m <- melt(df.new.1, id.vars = "time")
  
  plt <- ggplot(df.new.1.m, aes(x = time, y = value))+
    geom_line(size = 1)+theme_gray(base_size = 22)+
    facet_wrap(.~variable,nrow = 4,
               scales = "free_y",
               strip.position = "left", labeller = as_labeller(c(conc = "Conc.", 
                                                                 desea = "De-Conc.", 
                                                                 wspeed = "Wind Speed", 
                                                                 temp = "Temperature")))+
    theme(strip.background = element_blank(),
          strip.placement = "outside") + ylab("") +xlab("Time" )
  # strip.text.x = element_blank())
  plot.folder <- paste(folder,"/dataset/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"type=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}

## Computes and plots the quantiles for weekly August data 
# Input: Averaging time

plot.weekQuantiles_Aug<-function(avg_time, loc = "IISc", folder = "plots", formats=c("PDF", "PNG")){
  destfile=paste('TSA Cached data/sea_CO_aug_all_',loc,"_",
                 as.character(avg_time*60),'.csv', sep="")
  
  if (!file.exists(destfile)) {
    if(loc == "IISc"){
      C_data <- read.csv(file = 'Raw Sensor Data/Climo_CO_23.csv', header = TRUE, sep = ";")
      cTime <-  as.POSIXct(C_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
    }
    else if(loc == "Electronic_City"){
      C_data <- read.csv(file = 'Raw Sensor Data/ECity_Climo_CO_23.csv', header = TRUE, sep = ";")
      cTime <-  as.POSIXct(C_data$Time)#(,"%Y-%m-%d %H:%M:%S", tz = "")
    }
    C_xts <- xts(x = C_data$Value, order.by = cTime)
    
    # A list of lists storing data for each of the 38 days 
    
    days_summary <- removeAnomaly(C_xts)
    day_h <- days_summary[[1]]
    day_dict <- days_summary[[2]]
    
    
    week_flag = min(day_h)
    days_all<- split(C_xts, f = "days") # A list of lists storing data for each of the 38 days
    days <- days_all[1:length(days_all)]
    # 
    #     for(i in 1:7){
    #       for(j in day_dict[[i]]){
    #         print(length(days[[j]]))
    #       }
    #     }
    
    data_summary <- getAverage_daily(days, day_dict, avg_time, week_flag, CI = -1)
    seasonal_dow <- data_summary$average
    
    data_summary <- getAverage_daily(days, day_dict, avg_time, week_flag, 0.95)
    quantile_high <- data_summary$quantile_summ
    
    data_summary <- getAverage_daily(days, day_dict, avg_time, week_flag, 0.05)
    quantile_low <- data_summary$quantile_summ
    
    df.master <- getLongDataFrame(seasonal_dow, quantile_high, quantile_low)
    
    write.csv(df.master, file = destfile, row.names = FALSE)
  }
  
  df.master <- read.csv(file = destfile, header = TRUE, sep = ",")
  df.temp <- df.master[df.master$day!='Sunday',]
  df.master.1 <- df.master
  df.master <- rbind(df.temp, df.master.1[df.master.1$day=="Sunday",])
  df.master$Time <- as.POSIXct(df.master$Time)
  plt <- ggplot(df.master, aes(x = Time, y = Concentrations, color = type )) +
    geom_line(size = 0.5) + scale_x_datetime(date_labels = "%H")+theme_grey(base_size = 18)+
    facet_wrap(.~factor(day, levels=unique(day)), ncol = 2)+ylab('Conc. (ppm)')+
    ylim(0,3.5) + xlab("Time of day (Hour)")+
    labs(color = "") + 
    theme(legend.position="bottom", legend.box = "horizontal") +
    theme(aspect.ratio = 0.67,legend.position=c(.73,.12), 
          legend.box = "horizontal",
          legend.background = element_rect(fill=alpha(colour = "white", alpha = 0.1)))+
    guides(color=guide_legend(direction = "horizontal",legend.text=element_text(size=18),nrow = 3))+
    # theme(axis.ticks.length=unit(-0.25, "cm"))+
    theme(axis.text.x = element_text(angle=0))#, legend.position = "bottom")
  
  plot.folder <- paste(folder,"/weekQuantiles_Aug/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
  plot.filename <- paste(plot.folder,"type=",loc,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
  return(plt)
  # 
}

## Plots the overall time series data
# Inputs: 
#   - xts.ts: time series object
#   - type: type of variable (eg. "CO Concentrations", "wind speed", )
#   - folder: name of folder for the plots to be saved in.

plot.TS_Overall <- function(xts.ts, type, title="Overall time series", folder = "plots", formats=c("PDF", "PNG"))
{	
  
  data <- data.frame(t=time(xts.ts), conc=coredata(xts.ts))
  plt <- ggplot(data=data, aes(x=t, y = conc))
  plt <- plt + geom_line(colour="steelblue")
  if(title=="Overall time series"){
    plt <- plt + ggtitle(paste(title, "of ", type, sep=""))
  }
  else{
    plt <- plt + ggtitle(title)
  }
  
  plt <- plt + xlab("Time") 
  plt <- plt + ylab(type)
  
  # create folder
  plot.folder <- paste(folder,"/TS_Overall/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"type=",type,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}

# Plot Dataframe with fautly rows in red
# Inputs: 
#   - df.new: master data frame with concentrations, windspeed and wind directions
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.

plot.DF_faults<-function(df.new, df.specs, avg_time, folder = "plots",formats=c("PDF", "PNG")){
  
  df.new.fault <- df.new[rowSums(is.na(df.new[,c(1,2,3)]))>0,]
  refTime <- as.POSIXct(df.new$time)
  
  fault <- df.new.fault$conc
  fault.time <- as.POSIXct(df.new.fault$time)
  Faults <- xts(fault, order.by = fault.time)
  Overall <- xts(df.new$conc, order.by = refTime)
  comparison<- merge(Overall, Faults, join = "left")
  df.comp <- data.frame(x = time(comparison), real.data <- coredata(comparison))
  df.m <- melt(df.comp, id.vars = "x")
  
  
  plt <- ggplot(df.m, aes(x = x,y = value, color = variable)) + 
    geom_line() + 
    scale_colour_manual(values=c(Overall = "black", Faults = "red")) + 
    labs(color = "Legend") +  
    ylab('Conc. (ppm)') + 
    xlab("Time")+
    theme_grey(base_size = 15)
  
  plot.folder <- paste(folder,"/DF_faults/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}

# Plots the daily seasonality w.r.t wind speed and wind direction
# - Input
#     - df.seasonal: data frame with the seasonal values
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.

plot.DF_Seasonal_c_ws<-function(df.seasonal, df.specs, avg_time, folder = "plots",formats=c("PDF", "PNG")){
  
  p1 <- ggplot(df.seasonal, aes(x = x, y = conc)) + 
    geom_line(size = 1) + geom_point(size = 1.5) + 
    ylab('Conc. (ppm)') + 
    theme_grey(base_size = 12)+ 
    scale_x_datetime(date_labels = "%H") + 
    theme_grey(base_size = 15) + 
    xlab("") + ggtitle("Seasonality of Concentrations")
  
  p2 <- ggplot(df.seasonal, aes(x = x, y = wspeed)) + 
    geom_line(size = 1) + geom_point(size = 1.5) + 
    ylab("Wind Speed (m/s)") + 
    xlab("Time of day (Hours)") + 
    theme_grey(base_size = 15) + 
    scale_x_datetime(date_labels = "%H") + ggtitle("Seasonality of Wind Speeds")

  plt<- plot_grid(p1, p2,nrow=2)  
  plot.folder <- paste(folder,"/DF_Seasonal_c_ws/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}
plot.DF_Seasonal_c_t<-function(df.seasonal, df.specs, avg_time, folder = "plots",formats=c("PDF", "PNG")){
  
  p1 <- ggplot(df.seasonal, aes(x = x, y = conc)) + 
    geom_line(size = 1) + geom_point(size = 1.5) + 
    ylab('Conc. (ppm)') + 
    theme_grey(base_size = 12)+ 
    scale_x_datetime(date_labels = "%H:%M") + 
    theme_grey(base_size = 15) + 
    xlab("") + ggtitle("Seasonality of Concentrations")
  
  p3 <- ggplot(df.seasonal, aes(x = x, y = temp)) + 
    geom_line(size = 1) + geom_point(size = 1.5) + 
    ylab("Temperature") + 
    xlab("Time of day") + 
    theme_grey(base_size = 15) + 
    scale_x_datetime(date_labels = "%H:%M") + 
    ggtitle("Seasonality of Temperatures")
  plt<- plot_grid(p1, p3, nrow=2)  
  plot.folder <- paste(folder,"/DF_Seasonal_c_t/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}

# Plots the correlatins of wind speed and concentrations and the Z score of the concentrations
# - Input
#   - df.seasonal: data frame with the seasonal values
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.


plot.Corr_ws_c<-function(df.seasonal, df.specs, avg_time, folder = "plots",formats=c("PDF", "PNG")){
  
  
  df.seasonal$z.conc <- computeZscore(df.seasonal$conc)
  p1 <- ggplot(df.seasonal, aes(x = wspeed, y = conc)) + 
    geom_point(size = 2) + xlab("")+
    ylab('Conc. (ppm)') + ggtitle("Concentrations vs Wind Speeds") +
    scale_color_gradient(low="red", high="blue") + theme_grey(base_size = 14) + labs(color = "Wind Directions") #+ theme(legend.position="none")
  p2 <- ggplot(df.seasonal, aes(x = wspeed, y = z.conc)) + 
    geom_point(size = 2) + ggtitle("Z-score of Concentrations vs Wind Speeds") +
    xlab("Wind Speed") + ylab("Z-score of Concentration") +
    scale_color_gradient(low="red", high="blue") + theme_grey(base_size = 14)
  
  plt<- plot_grid(p1, p2, nrow = 2)
  plot.folder <- paste(folder,"/Corr_ws_c/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}

plot.Corr_t_c<-function(df.seasonal, df.specs, avg_time, folder = "plots",formats=c("PDF", "PNG")){
  
  
  df.seasonal$z.conc <- computeZscore(df.seasonal$conc)
  p1 <- ggplot(df.seasonal, aes(x = temp, y = conc)) + 
    geom_point(size = 1.5) + 
    xlab("") + 
    ylab('Conc. (ppm)') + 
    ggtitle("Concentrations vs Temperature over all days") +
    scale_color_gradient(low="red", high="blue") + 
    theme_grey(base_size = 14) + 
    labs(color = "Wind Directions") #+ theme(legend.position="none")

  p2 <- ggplot(df.seasonal, aes(x = temp, y = z.conc)) + 
    geom_point(size = 1.5) + 
    xlab("Temperature") + ylab("Z-score of Concentration") +
    scale_color_gradient(low="red", high="blue") + 
    theme_grey(base_size = 14)
  
  plt<- plot_grid(p1, p2, nrow = 2)
  plot.folder <- paste(folder,"/Corr_t_c/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}


# Plots the correlations of wind speed on de-seasoned data
# - Input
#   - df.new: Master data frame
#   - df.seasonal: data frame with the seasonal values
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.


plot.Scat_ws_deseasoned<-function(df.new, df.seasonal, df.specs, avg_time, clustering = FALSE, folder = "plots", formats = c("PDF", "PNG")){
  
  df.new$sea <- rep(df.seasonal$conc, nrow(df.new)/(24/avg_time))
  
  df.new$desea <- df.new$conc - df.new$sea
  
  idx <- df.new$hour>21
  df.new.1 <- df.new
  df.new.1$Set[idx] <- "Night"
  
  idx <- df.new$hour<6
  df.new.1$Set[idx] <- "Night"
  
  idx <- is.na(df.new.1$Set)
  df.new.1$Set[idx] <- "Day"
  
  # df.clean <- df.new[complete.cases(df.new), ]
  if (clustering == FALSE){
    plt<-  ggplot(df.new.1, aes(x = wspeed, y = desea, color = Set, shape = Set)) + 
      geom_point(size = 3) + labs(size = "")+
      xlab("Wind Speed") +
      ylab("De-seasoned concentration") + 
      # ggtitle("Concentrations vs Wind Speeds over all days") +
      # scale_color_gradient(low="red", high="blue") +
      theme_gray(base_size = 14) #+ 
    
    plt <- plt + scale_shape_manual(values = c(0, 16)) + scale_colour_manual(values = c("chartreuse4", "slateblue4"))
  }
  else{
    plt<-  ggplot(df.new.1, aes(x = wspeed, y = desea, color = Set, shape = Set)) +
      facet_wrap(.~cluster)+theme_gray(base_size = 14)+ #+ 
      geom_point(size = 3) + labs(size = "", shape = "", color = "")+
      xlab("Wind Speed") +
      ylab("De-seasoned concentration")
      # ggtitle("Concentrations vs Wind Speeds over all days") +
      # scale_color_gradient(low="red", high="blue") +
    
    plt <- plt + scale_shape_manual(values = c(0, 16)) + scale_colour_manual(values = c("chartreuse4", "slateblue4"))
  }
  
  plot.folder <- paste(folder,"/Scat_ws_deseasoned/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }  
  print(plt)
  return(plt)
}


plot.Scat_t_deseasoned<-function(df.new, df.seasonal, df.specs, avg_time, clustering = FALSE, folder = "plots", formats = c("PDF", "PNG")){
  
  df.new$sea <- rep(df.seasonal$conc, nrow(df.new)/(24/avg_time))
  
  df.new$desea <- df.new$conc - df.new$sea
  
  # df.clean <- df.new[complete.cases(df.new), ]
  if(clustering == FALSE)
  {plt<-  ggplot(df.new, aes(x = temp, y = desea, color = hour)) + 
    geom_point(size = 3) + 
    xlab("Temperature") + 
    ylab("De-seasoned concentration") + 
    # ggtitle("Concentrations vs Wind Speeds over all days") +
    scale_color_gradient(low="red", high="blue") + 
    theme_grey(base_size = 14) + 
    labs(color = "Hour of day") #+ 
  }
  else{
    plt<-  ggplot(df.new, aes(x = temp, y = desea, color = hour)) + 
      facet_wrap(.~cluster)+
      geom_point(size = 3) + 
      xlab("Temperature") + 
      ylab("De-seasoned concentration") + 
      # ggtitle("Concentrations vs Wind Speeds over all days") +
      scale_color_gradient(low="red", high="blue") + 
      theme_grey(base_size = 14) + 
      labs(color = "Hour of day") #+ 
  }
  plot.folder <- paste(folder,"/Scat_t_deseasoned/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  # record plot
  #	print(data)
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }  
  print(plt)
  return(plt)
}

# Plots the histogram plots of concentrations and wind direction
# - Input
#   - df.new: Master data frame
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.


plot.Hist_conc_wd<-function(df.new, df.specs, avg_time, folder = "plots", formats = c("PDF", "PNG")){
  
  plt<-ggplot(df.new, aes(x = wdir, y = conc)) + geom_point(aes(color = wspeed)) + xlab("Wind Direction") + ylab('Conc. (ppm)') + labs(color = "Wind Speed") + scale_color_gradient(low="blue", high="red") + theme_grey(base_size = 14) + ggtitle("Concentration vs Wind Direction")
  
  plot.folder <- paste(folder,"/Hist_conc_wd/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }  
  print(plt)
}

## COncentrations and wind speed according to wind direciton
#- Input: 
#   - df.new: Master data frame
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.

plot.conc_ws_on_wd<-function(df.new, df.specs,avg_time, folder = "plots", formats = c("PDF", "PNG")){
  
  df.new.clean <- df.new[complete.cases(df.new), ]
  
  unique.x.norm <- unique(df.new.clean$wdir)
  cond.mean.y <- c()
  cond.quant_l.y <- c()
  cond.quant_h.y <- c()
  
  cond.mean.ws <- c()
  cond.quant_l.ws <- c()
  cond.quant_h.ws <- c()
  
  for(i in 1:length(unique.x.norm)){
    cond.mean.y <- c(cond.mean.y, mean(df.new.clean[df.new.clean$wdir == unique.x.norm[i],]$conc))
    cond.quant_l.y <- c(cond.quant_l.y, quantile(df.new.clean[df.new.clean$wdir == unique.x.norm[i],]$conc, 0.05))
    cond.quant_h.y <- c(cond.quant_h.y, quantile(df.new.clean[df.new.clean$wdir == unique.x.norm[i],]$conc, 0.95))
    
    cond.mean.ws <- c(cond.mean.ws, mean(df.new.clean[df.new.clean$wdir == unique.x.norm[i],]$wspeed))
    cond.quant_l.ws <- c(cond.quant_l.ws, quantile(df.new.clean[df.new.clean$wdir == unique.x.norm[i],]$wspeed, 0.05))
    cond.quant_h.ws <- c(cond.quant_h.ws, quantile(df.new.clean[df.new.clean$wdir == unique.x.norm[i],]$wspeed, 0.95))
  }
  
  df.c <- data.frame(uni.x = rep(unique.x.norm, 3), val = c(cond.mean.y, cond.quant_h.y, cond.quant_l.y), type = c(rep("Mean", length(unique.x.norm)), rep("95% Quantile", length(unique.x.norm)),rep("5% Quantile", length(unique.x.norm))))# class = "conc")
  
  df.ws <- data.frame(uni.x = rep(unique.x.norm, 3), val = c(cond.mean.ws, cond.quant_h.ws, cond.quant_l.ws), type = c(rep("Mean", length(unique.x.norm)), rep("95% Quantile", length(unique.x.norm)),rep("5% Quantile", length(unique.x.norm))))#, class = "ws")
  
  df <- rbind(df.c, df.ws)
  
  p1.2 <- ggplot(df.c, aes(x = uni.x, y = val, color = type)) + 
    geom_line(size = 1) + 
    ylim(min(range(df.c$val)[1], 0),max(df.c$val)) + 
    labs(color = "Legend")+ 
    xlab("") + 
    ylab("Avg. Concentration") + 
    geom_point(size = 2) + 
    theme_grey(base_size = 16) + 
    ggtitle("Concentration vs Wind Direction")+
    theme(legend.position="none")
  p2.2 <- ggplot(df.ws, aes(x = uni.x, y = val, color = type)) + 
    geom_line(size  = 1) + 
    ylim(min(range(df.ws$val)[1], 0),max(df.ws$val)) + 
    xlab("Wind Direction ") + 
    ylab("Avg. Wind Speed") + 
    geom_point(size= 2) + 
    labs(color = "")+
    theme_grey(base_size = 16)+
    ggtitle("Wind Speed vs Wind Direction")+
    theme(legend.position="bottom", legend.box = "horizontal")
  
  plt <- plot_grid(p1.2, p2.2, align = "v", nrow = 2, rel_heights = c(0.45, 0.55))
  
  plot.folder <- paste(folder,"/conc_ws_on_wd/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }  
  print(plt)
}

# Plots the deseasoned concentrtions against the wind speed with each subplot corresponding to a wind direction
# - Input
#   - df.seasonal: data frame with the seasonal averaged data
#   - df.new: Master data frame
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.

plot.scat_ws_decon<- function(df.new, df.seasonal, df.specs, avg_time, folder = "plots", formats = c("PDF", "PNG")){
  df.new$sea <- rep(df.seasonal$conc, nrow(df.new)/(24/avg_time))
  
  df.new$desea <- df.new$conc - df.new$sea
  
  # df.new <- df.new[complete.cases(df.new), ]
  plt <- ggplot(df.new, aes(x = wspeed, y = desea)) + 
    facet_wrap(.~wdir) + 
    geom_point(size  = 1) + 
    theme_grey(base_size = 14) + 
    xlab("Wind speed(m/s)") + 
    ylab("Deseasoned concentrations") + 
    ggtitle("Deseasoned Concentration vs Wind Speed wraped for each wind direction")
  
  plot.folder <- paste(folder,"/scat_ws_decon/",sep="")
  dir.create(plot.folder,showWarnings=FALSE,recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
}

# Plots two figures
# - The noise variance and the concentration characteristics for a data frame
# - The noise variance of concentrations as function of time of day
# - Input
#   - df.new: Master data frame
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.

plot.NV_conc_char<-function(df.new, df.seasonal, df.specs, avg_time, folder = "plots", formats = c("PDF", "PNG")){
  
  df.new$sea <- rep(df.seasonal$conc, nrow(df.new)/(24/avg_time))
  
  df.new$desea <- df.new$conc - df.new$sea
  # df.new <- df.new[complete.cases(df.new), ]
  hour.set <- 0:23
  desea.xts <- xts(df.new$desea, order.by = as.POSIXct(df.new$time))
  conc.xts <- xts(df.new$conc, order.by = as.POSIXct(df.new$time))
  
  desea.split <- split(desea.xts, f = "hours")
  conc.split <- split(conc.xts, f = "hours")
  
  len = length(desea.split)
  desea.var.1hr <- array(0, len)
  conc.var.1hr <- array(0, len)
  conc.1hr <- array(0, len)
  desea <- array(0, len)
  time.day <- array(0, len)#.POSIXct(character(len))
  
  for(i in 1:length(desea.split)){
    desea.var.1hr[i] = var(desea.split[[i]])
    conc.var.1hr[i] = var(conc.split[[i]])
    desea[i] = mean(desea.split[[i]])
    conc.1hr[i] = mean(conc.split[[i]])
    time.day[i] <- hour(start(conc.split[[i]]))
  }
  df.var.1 <- data.frame(time = time.day, conc = conc.1hr, de.var = desea.var.1hr, conc.var = conc.var.1hr)
  
  df.var.2 <- data.frame(time = time.day, desea = desea, de.var = desea.var.1hr, conc.var = conc.var.1hr)
  
  df.final <- data.frame(time = time.day, de.var = desea.var.1hr, conc.var = conc.var.1hr)
  
  p.char1 <- ggplot(df.var.2, aes(x=desea,y=de.var)) + 
    geom_point(size = 1.5) +  xlim(-1, 1.5) + ylab(expression(paste(sigma,'(De-seasoned Concentrations)')))+
    xlab("Deseasoned Concentrations")+ theme_grey(base_size = 13)
  p.char2 <- ggplot(df.var.1, aes(x=conc,y=conc.var)) + 
    geom_point(size = 1.5) + 
    xlab("Concentrations") + ylab(expression(paste(sigma,'(Concentrations)')))+
    theme_grey(base_size = 13) # + stat_smooth(method = "loess")
  # p4 <- ggplot(df.var.2, aes(x=desea,y=conc.var)) + 
  #   geom_point(size = 1.5) + 
  #   ylab("") + ylim(0, 1) + theme_grey(base_size = 13)#+ stat_smooth(method = "loess")
  
  plt <- plot_grid(p.char1, p.char2, nrow = 2)
  # title <- ggdraw() + draw_label("Noise Variance and Concentration Characteristics")
  
  # plt<- plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
  
  
  plot.folder <- paste(folder,"/NV_conc_char/", sep="")
  dir.create(plot.folder, showWarnings=FALSE, recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()
  }
  print(plt)
  p1 <- ggplot(df.var.2, aes(x = time, y = de.var)) +  geom_point(size = 1.5,alpha = 0.5, color = "black")+ ylim(0, 0.5)+
    stat_summary(aes(y = de.var, group=1), fun.y=mean, colour="black", geom="line",group=1, size = 1)+
    xlab("")+ylab(expression(paste(sigma,'(De-seasoned Concentrations)')))
  
  p2 <- ggplot(df.var.1, aes(x = time, y = conc.var)) + geom_point(size = 1.5,alpha = 0.5, color = "black")+ ylim(0, 0.5)+
    stat_summary(aes(y = conc.var, group=1), fun.y=mean, colour="black", geom="line",group=1, size = 1)+
    xlab("Time of day (Hours)")+ylab(expression(paste(sigma,'(Concentrations)')))
  plt <- plot_grid(p1, p2, nrow = 2)
  #   
  #   geom_poin(data = df.var.2, aes(x = time, y = de.var), size = 1.5, alpha = 0.5, color = "blue")+
  #     stat_summary(aes(y = de.var, group=1), fun.y=mean, colour="blue", geom="line",group=1, size = 1)+
  #     scale_colour_manual(values =c("Conc" = "red", "de.Conc" = "blue"), 
  #                       labels = c("Concentrations", "De-seasoned concentrations"))
  #   
  #   df.final.m <- melt(cbind(df.final[1], as.matrix(df.final[-1])), id = "time")
  #   
  #   ggplot(df.var.1, aes( x = time, y = value, color = variable))+geom_point(size = 1, alpha = 0.5)+
  #     stat_summary(aes(y = value, group=1), fun.y=mean, colour="red", geom="line",group=1, size = 1)
  #     
  #   
  #   ggplot(df.var.1, aes(x = time, y = conc)) + geom_point(size = 1.5) + geom_line(aes(x = time, y = conc.var))
  # # + geom_line(aes(x  =0:23,  y = ql.c, linetype="dotted"), colour = '5th percentile')+geom_line(aes(x  =0:23, y = qh.c, linetype="dotted",colour = '95th percentile') ) + 
  # #geom_line(aes(y = ql.dc, linetype="dotted"), colour = '5th percentile - desea')+geom_line(aes(y = qh.dc, linetype="dotted",colour = '95th percentile - desea') )
  # 
  plot.folder <- paste(folder,"/NV_conc_char_ToD/", sep="")
  dir.create(plot.folder, showWarnings = FALSE, recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=ToD_",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()      
  }
  print(plt) 
}

# Plots the concentration and the variance as a function of wind
# - Input
#   - df.new: Master data frame
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.

plot.conc_on_ws<- function(df.new, df.seasonal, df.specs, avg_time, clustering = FALSE, folder = "plots", formats = c("PDF", "PNG")){
  
  df.new$sea <- rep(df.seasonal$conc, nrow(df.new)/(24/avg_time))
  
  df.new$desea <- df.new$conc - df.new$sea
  
  windSpeeds <- seq(0, max(df.new.clean$wspeed), 0.1)
  
  batch.m <- array(0, (length(windSpeeds)-1))
  batch.v <- array(0, (length(windSpeeds)-1))
  batch.ql <- array(0, (length(windSpeeds)-1))
  batch.qh <- array(0, (length(windSpeeds)-1))
  batch.ws <- array(0, (length(windSpeeds)-1))
  batch.c <- array(0, (length(windSpeeds) - 1))
  
  for(i in 1:(length(windSpeeds)-1)){
    batch.m[i] <- mean(df.new$desea[which(df.new$wspeed>windSpeeds[i] & df.new$wspeed<windSpeeds[i+1])])
    batch.c[i] <- mean(df.new$conc[which(df.new$wspeed>windSpeeds[i] & df.new$wspeed<windSpeeds[i+1])])
    
    batch.ql[i] <- quantile(df.new$desea[which(df.new$wspeed>windSpeeds[i] & df.new$wspeed<windSpeeds[i+1])], 0.05)
    batch.qh[i] <- quantile(df.new$desea[which(df.new$wspeed>windSpeeds[i] & df.new$wspeed<windSpeeds[i+1])], 0.95)
    batch.v[i] <- var(df.new$desea[which(df.new$wspeed>windSpeeds[i] & df.new$wspeed<windSpeeds[i+1])])
    batch.ws[i] <- mean(windSpeeds[c(i, (i+1))])
  }
  df.batch <- data.frame(ws = batch.ws, mean = batch.m, var = batch.v, ql = batch.ql, qh = batch.qh)#, conc = batch.c)
  
  df.m <- melt(df.batch, id.vars = "ws")
  
  plt <- ggplot(df.m[df.m$variable!="var",], 
               aes(x = ws, y= value, color = variable)) + 
    geom_line(size = 1)+geom_point(size = 1.5)+
    # ylab(expression(paste("Concentration (",mu,"g/m")))  + xlab("Wind Speed (m/s)")+
    ylab('Conc. (ppm)')  + xlab("Wind Speed (m/s)")+
    theme_grey(base_size = 14) + 
    scale_colour_manual(values =c('mean'='black','ql'='green', "qh" = "red"), 
                        labels = c(#"Avg. de-seasoned concentration",
                                   "5% Quantile of de-seasoned concentration", 
                                   "95% Quantile de-seasoned concentration",
                                   "Avg. concentration")) +
    theme(legend.position="bottom", legend.box = "horizontal")+
    labs(color = "") + guides(col = guide_legend(nrow = 4))
  #p2 <- ggplot(df.batch, aes(x = ws, y= batch.v)) + geom_point(size = 2)+xlab("Wind Speed (m/s)")+ylab("Variance") +   theme_grey(base_size = 15)
  
  # plt <- plot_grid(p1, p2 , nrow = 2,rel_heights = c(0.55, 0.45))
  plot.folder <- paste(folder,"/conc_on_ws/", sep="")
  dir.create(plot.folder, showWarnings=FALSE, recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()      
  }  
  
  print(plt)
}

# Plots the scatter plot with the Z scores of both the concentrations and the wind speed
# - Input
#   - df.new: Master data frame
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.


plot.Zconc_Zws<- function(df.new, df.specs, avg_time,folder = "plots", formats = c("PDF", "PNG")){
  
  df.new <- df.new[complete.cases(df.new),]
  df.new$z.conc <- computeZscore(df.new$conc)
  df.new$z.ws <- computeZscore(df.new$wspeed)
  
  plt<- ggplot(df.new[which( df.new$hour > 8 | df.new$hour < 18),], 
         aes(x = z.ws, y = z.conc)) + 
    geom_point(size = 1) + 
    xlab("Z-score(Wind Speed)") + 
    ylab("Z-score(Concentrations)")  +
    theme_grey(base_size = 14) + 
    ggtitle("Z-score(Conc) vs Z-score(Wind speed) from 9AM to 6PM")
  
  plot.folder <- paste(folder,"/Zconc_Zws/", sep="")
  dir.create(plot.folder, showWarnings=FALSE, recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()      
  }
  print(plt)
}

# Plots the deseasoned concentrtions against the wind speed with each subplot corresponding to a wind direction
# - Input
#   - df.sea.d: data frame with the daily seasonal averaged data
#   - df.new: Master data frame
#   - df.sea.w: data frame with the weekly seasonal averaged data
#   - df.specs: name of data set (char)
#   - folder: name of folder for the plots to be saved in.
  
plot.scat_ws_wdecon<- function(df.new, df.sea.d,df.sea.w, avg_time, folder = "plots", formats = c("PDF", "PNG")){
  # ggplot(df.seasonal, aes(x = (1:(24/avg_time)/2), y = wspeed)) + geom_line()
  
  df.new$sea.d <- rep(df.sea.d$conc, nrow(df.new)/(24/avg_time))
  df.new$sea.w <- pad_df_weeklySea(df.new, df.sea.w)
  
  
  df.new$desea.d <- df.new$conc - df.new$sea.d
  df.new$desea.w <- df.new$conc - df.new$sea.w
  
  plt <- ggplot(df.new[complete.cases(df.new), ], aes(x = wspeed, y = desea.w)) + 
    facet_wrap(.~wdir) + 
    geom_point(size  = 1) + 
    theme_grey(base_size = 14) + 
    xlab("Wind speed(m/s)") + 
    ylab("Deseasoned concentrations")
  
  plot.folder <- paste(folder,"/scat_ws_wdecon/", sep="")
  dir.create(plot.folder, showWarnings=FALSE, recursive=TRUE)
  
  for(format in formats){
    plot.filename <- paste(plot.folder,"dfName=",df.specs,".",format,sep="")
    if(!is.na(format)){
      if(format=="PDF")
        pdf(file=plot.filename,bg="white")
      else if(format=="PNG")
        png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
    }
    
    print(plt) #suppressMessages(print(plt))
    
    if(!is.na(format))
      dev.off()      
  }
  print(plt)
  
}