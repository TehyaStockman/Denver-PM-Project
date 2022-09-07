airnow_processing <- function(raw_data){

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

  ##This part would create columns of values for the sensor ID, sensor type, and lat/long

  
  keeps <- c("Date", "PM2.5")
  raw_data <- raw_data[keeps]
  
  names(raw_data)[names(raw_data) == "Date"] <- "date"
  names(raw_data)[names(raw_data) == "PM2.5"] <- "val"
  
  raw_data$date <- as.POSIXct(raw_data$date, tz="UTC", format = '%m/%d/%Y %T')
  raw_data$date <- with_tz(raw_data$date, "MST")
  
  # #Flagging data to be cleaned, adding columns of flags for low, normal, and high as -1, 0, and 1.
  # #This way these flags can be averaged to understand why NA values appear for a value averaged over an hour.
  raw_data <- mutate(raw_data, flag =
                       ifelse(val < 0, -1,
                              ifelse(val > 500, 1,
                                     0)))

  #Averaging the data to 1 hour interval
  raw_data <- timeAverage(raw_data, avg.time = "hour")

  raw_data$parameter <- "pm25_p"
  
  
  #Round the values so there are not so many sigfigs -- can modify to match the uncertainty of the instrument
  raw_data$pm25_p.val <- signif(raw_data$val, 3)
  

  # #After the files are merged that should be, additional information about the sensor can be added to other columns that are
  # #not included in the long format.
  # #Add ID and sensor type to file, make everything in good format
  #raw_data$SensorType <- "Airnow"
  raw_data$ID <- shortname
  
  # #This piece of code just makes sure that everything is lined up in the order that makes the most sense
  raw_data <- subset(raw_data, select = c("date","ID", "parameter", "val", "flag"))
  
  
  #Add units of each of the parameters
  raw_data <- mutate(raw_data, parameter_units = 
                       ifelse(parameter == "pm25_p", "ug/m3","NA"))
  
  # #Update the flags to be high, missing, low. The flags show that there were high or low values that were removed during the hour.
  # #A flag of "high missing" means that there were enough values in the hour that were high to not meet the 75% threshold
  # #It also means that the values were generally high, as the flags are averaged to give an hourly flag
  # #If there are just missing values and it is not high, then only "missing" will be shown on the flag
  
  raw_data$flag2[is.na(raw_data$val) & is.na(raw_data$flag)] <-"missing"
  raw_data$flag2[raw_data$flag < 0 & is.na(raw_data$val)] <-"low, missing"
  raw_data$flag2[raw_data$flag > 0 & is.na(raw_data$val)] <-"high, missing"
  raw_data$flag2[raw_data$flag < 0 & !is.na(raw_data$val)] <-"low"
  raw_data$flag2[raw_data$flag > 0 & !is.na(raw_data$val)] <-"high" 
  raw_data$flag2[is.na(raw_data$flag2)] <-""
  
  raw_data <- subset(raw_data, select = c("date","ID", "parameter", "val", "flag2", "parameter_units"))
  names(raw_data)[names(raw_data) == "flag2"] <- "flag"
  

  return(raw_data)
  return(shortname)
  
}