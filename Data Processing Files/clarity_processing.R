clarity_processing <- function(raw_data){
  
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

  #-------------------------------
  ##This part would create columns of values for the sensor ID, sensor type, and lat/long

  keeps <- c("Date","pm10ConcMass", "pm2_5ConcMass", "relHumid","temperature")
  raw_data <- raw_data[keeps]
  
  #renaming columns
  names(raw_data)[1] <- "date"
  names(raw_data)[2] <- "pm10.val"
  names(raw_data)[3] <- "pm25_p.val"
  names(raw_data)[4] <- "humidity.val"
  names(raw_data)[5] <- "temperature.val"
  
  #make temperature in ferenheight
  raw_data$temperature.val <- raw_data$temperature.val*9/5 + 32
  
  #Saying what date is

  raw_data$date <- round_date(as.POSIXct(raw_data$date, tz="UTC", format = '%m/%d/%Y %H:%M'), unit = "1 minute")
  #browser()
  raw_data$date <- with_tz(raw_data$date, "MST")
  
  
  # #Flagging data to be cleaned, adding columns of flags for low, normal, and high as -1, 0, and 1.
  # #This way these flags can be averaged to understand why NA values appear for a value averaged over an hour.
  raw_data <- mutate(raw_data, pm25_p.flag =
                       ifelse(pm25_p.val < -5, -1,
                              ifelse(pm25_p.val > 500, 1,
                                     0)))
  
  raw_data <- mutate(raw_data, pm10.flag =
                       ifelse(pm10.val < -5, -1,
                              ifelse(pm10.val > 500, 1,
                                     0)))
  
  raw_data <- mutate(raw_data, temperature.flag =
                       ifelse(temperature.val == -1, -1,
                              ifelse(temperature.val < -10, -1,
                                     ifelse(temperature.val > 100, 1,
                                            0))))
  
  raw_data <- mutate(raw_data, humidity.flag =
                       ifelse(humidity.val < 0, -1,
                              ifelse(humidity.val > 100, 1,
                                     0)))
  

  # #Now it is time to clean up the data. Data cleaning comes after data flagging so that everything will be flagged and then
  # #values that are deemed too high or low can be removed. This process is done before averaging the data so that bad data
  # #is not included in the average.
  #
  #Removing Negative Values and replacing with NA
  raw_data$temperature.val <- replace(raw_data$temperature.val, raw_data$temperature.val < 0, NA)
  raw_data$pm10.val <- replace(raw_data$pm10.val, raw_data$pm10.val < -5, NA)
  raw_data$pm25_p.val <- replace(raw_data$pm25_p.val, raw_data$pm25_p.val < -5, NA)
  raw_data$humidity.val <- replace(raw_data$humidity.val, raw_data$humidity.val < 0, NA)
  
  #Removing High Values and replacing with NA
  raw_data$pm10.val <- replace(raw_data$pm10.val, raw_data$pm10.val > 500, NA)
  raw_data$pm25_p.val <- replace(raw_data$pm25_p.val, raw_data$pm25_p.val >500, NA)
  raw_data$humidity.val <- replace(raw_data$humidity.val, raw_data$humidity.val > 100, NA)
  
  #-------------------------------
  
  #Averaging the data to 1 hour interval, threshold of 75%
  raw_data <- timeAverage(raw_data, avg.time = "15 min")
  # # #print(raw_data)
  raw_data <- timeAverage(raw_data, avg.time = "hour", data.thresh = 75, interval = "15 min")
  # #print(raw_data)
  
  #This step just makes sure that the sigfigs are consistent and that the file does not get too long.
  #Make all sigfigs consistent
  raw_data$pm25_p.val <- signif(raw_data$pm25_p.val, 3)
  raw_data$pm10.val <- signif(raw_data$pm10.val, 3)
  raw_data$temperature.val <- signif(raw_data$temperature.val, 3)
  raw_data$humidity.val <- signif(raw_data$humidity.val, 3)


  #-------------------------------

  # #This step takes the wide format data and merges the columns into a long format. The column with all of the parameters has to be
  # #renamed from what the function gives otherwise.
  
  #Merge all of the data into a long format
  raw_data <- merged.stack(raw_data, var.stubs = c("val", "flag"),
                           sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
                             ".", "", .time_1, fixed = TRUE)][]
  
  names(raw_data)[2] <- "parameter"
  
  
  # #After the files are merged that should be, additional information about the sensor can be added to other columns that are
  # #not included in the long format.
  # #Add ID and sensor type to file, make everything in good format
  #raw_data$SensorType <- "Clarity"
  raw_data$ID <- shortname
  
  # #This piece of code just makes sure that everything is lined up in the order that makes the most sense
  raw_data <- subset(raw_data, select = c("date","ID", "parameter", "val", "flag"))
  
  
  #Add units of each of the parameters
  raw_data <- mutate(raw_data, parameter_units = 
                       ifelse(parameter == "pm25_p", "ug/m3",
                              ifelse(parameter == "pm10", "ug/m3",
                                     ifelse(parameter == "temperature", "F", "NA"))))
  
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
  
  raw_data <- subset(raw_data, select = c("date","ID","parameter", "val", "flag2", "parameter_units"))
  names(raw_data)[names(raw_data) == "flag2"] <- "flag"
  

  return(raw_data)
}
