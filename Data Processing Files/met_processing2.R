met_processing2 <- function(raw_data){
  
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(fs)
  library(tidyverse)
  library(stringr)
  library(chron)
  library(openair)
  library(naniar)
  library(ggplot2)
  library(pracma)
  library(splitstackshape)
  library(lubridate)
  #Check which is processed vs. unprocessed data
  if("Ambient.Avg.Temperature" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "Ambient.Avg.Temperature"] <- "Outdoor.Temperature"}
  
  if("Wind.speed" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "Wind.speed"] <- "Wind.Speed"}
  
  if("PM2.5...Local.Conditions" %in% colnames(raw_data) & "Outdoor.Temperature" %in% colnames(raw_data)){
    names(raw_data)[names(raw_data) == "PM2.5...Local.Conditions"] <- "pm25_p.val"
    keeps <- c("Date", "Outdoor.Temperature", "Wind.Direction", "Wind.Speed", "pm25_p.val")
  }else if("PM2.5...Local.Conditions" %in% colnames(raw_data) & !("Outdoor.Temperature" %in% colnames(raw_data))){
    names(raw_data)[names(raw_data) == "PM2.5...Local.Conditions"] <- "pm25_p.val"
    keeps <- c("Date","Wind.Direction", "Wind.Speed", "pm25_p.val")
  }else if (!("PM2.5...Local.Conditions" %in% colnames(raw_data)) & !("Outdoor.Temperature" %in% colnames(raw_data))){
    keeps <- c("Date", "Wind.Direction", "Wind.Speed")
  }else {keeps <- c("Date", "Outdoor.Temperature", "Wind.Direction", "Wind.Speed")}
  
  
  raw_data <- raw_data[keeps]
  
  names(raw_data)[names(raw_data) == "Date"] <- "date"
  if("Outdoor.Temperature" %in% colnames(raw_data)){
    names(raw_data)[names(raw_data) == "Outdoor.Temperature"] <- "temperature.val"}
  names(raw_data)[names(raw_data) == "Wind.Direction"] <- "wd.val"
  names(raw_data)[names(raw_data) == "Wind.Speed"] <- "ws.val"
  
  
  
  #Setting Up Date
  raw_data$date <- as.POSIXct(raw_data$date, tz="UTC", format = '%m/%d/%Y %T')
  raw_data$date <- with_tz(raw_data$date, "MST")
  
  #Adding flags
  if("temperature.val" %in% colnames(raw_data)){
    raw_data$temperature.flag <- 0}
  if("pm25_p.val" %in% colnames(raw_data)){
    raw_data$pm25_p.flag <- 0}
  raw_data$wd.flag <- 0
  raw_data$ws.flag <- 0
  
  
  #Averaging the data to 1 hour interval, threshold of 75%
  # raw_data <- timeAverage(raw_data, avg.time = "hour", data.thresh = 75)
  
  raw_data <- merged.stack(raw_data, var.stubs = c("val", "flag"), 
                           sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
                             ".", "", .time_1, fixed = TRUE)][]
  
  names(raw_data)[2] <- "parameter"
  #Add ID and sensor type to file, make everything in good format
  raw_data$ID <- shortname
  #raw_data$SensorType <- "State"
  
  
  raw_data <- subset(raw_data, select = c("date","ID", "parameter", "val", "flag"))
  
  
  #Add units of each of the parameters
  raw_data <- mutate(raw_data, parameter_units = 
                       ifelse(parameter == "ws", "mph",
                              ifelse(parameter == "wd", "deg", 
                                     ifelse(parameter == "temperature", "F",
                                            ifelse(parameter == "pm25_p", "ug/m3", "")))))
  
  
  #Update the flags to be high, missing, low. The flags show that there were high or low values that were removed during the hour.
  #A flag of "high missing" means that there were enough values in the hour that were high to not meet the 75% threshold
  #It also means that the values were generally high, as the flags are averaged to give an hourly flag
  #If there are just missing values and it is not high, then only "missing" will be shown on the flag
  raw_data$flag <- replace(raw_data$flag, raw_data$flag == 0, "") 
  
  return(raw_data)
}