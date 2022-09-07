lunar_processing <- function(raw_data) {
  
  raw_data <- Filter(function(x)!all(is.na(x)), raw_data)
  
  if("PM_2_5_P" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "PM_2_5_P"] <- "P25"}
  
  if("PM2_5_P" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "PM2_5_P"] <- "P25"}
  
  if("PM2_5_p" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "PM2_5_p"] <- "P25"}
  
  if("PM2_5_p_1" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "PM2_5_p_1"] <- "P25"}
  
  if("pm25_r" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "pm25_r"] <- "PM2_5"}
  
  if("pm25_p" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "pm25_p"] <- "P25"}
  
  if(!("P25" %in% colnames(raw_data)))
  {raw_data$P25 <- NA}
  
  if("PM2_52"  %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "PM2_52"] <- "pm25_r2"}
  
  if("IT" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "IT"] <- "Temp"
  names(raw_data)[names(raw_data) == "IH"] <- "Hmdty"}
  
  if("tempf" %in% colnames(raw_data))
  {names(raw_data)[names(raw_data) == "tempf"] <- "Temp"
  names(raw_data)[names(raw_data) == "humidity"] <- "Hmdty"}
  
  if("pm25_r2" %in% colnames(raw_data)){
  keeps <- c("Date", "PM10", "PM2_5", "P25", "Temp", "pm25_r2", "Hmdty")
  }else{  keeps <- c("Date", "PM10", "PM2_5", "P25", "Temp", "Hmdty")}
  
  raw_data <- raw_data[keeps]
  
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


  # #Files are named with "blabla".val because this will help lining up a similar naming structure with the corresponding flags
  # #By making the naming consistent, merging the data into a long format is simple to do
  names(raw_data)[1] <- "date"
  names(raw_data)[names(raw_data) == "PM10"] <- "pm10.val"
  names(raw_data)[names(raw_data) == "PM2_5"] <- "pm25_r.val"
  names(raw_data)[names(raw_data) == "pm25_r2"] <- "pm25_r2.val"
  names(raw_data)[names(raw_data) == "P25"] <- "pm25_p.val"
  names(raw_data)[names(raw_data) == "Temp"] <- "temperature.val"
  names(raw_data)[names(raw_data) == "Hmdty"] <- "humidity.val"
  
  
  raw_data$date <- as.POSIXct(raw_data$date, tz="UTC", format = '%m/%d/%Y %H:%M') # round_time, unit = "1 minute"
  
  
  raw_data$date <- with_tz(raw_data$date, "MST")

  # #Flagging data to be cleaned, adding columns of flags for low, normal, and high as -1, 0, and 1.
  # #This way these flags can be averaged to understand why NA values appear for a value averaged over an hour.
  raw_data <- mutate(raw_data, pm25_p.flag =
                        ifelse(pm25_p.val < -5, -1,
                               ifelse(pm25_p.val > 500, 1,
                                      0)))
  
  raw_data <- mutate(raw_data, pm25_r.flag =
                        ifelse(pm25_r.val < -5, -1,
                               ifelse(pm25_r.val > 500, 1,
                                      0)))
  
  if("pm25_r2" %in% colnames(raw_data)){
  raw_data <- mutate(raw_data, pm25_r2.flag =
                       ifelse(pm25_r2.val < -5, -1,
                              ifelse(pm25_r.val > 500, 1,
                                     0)))
  }
  
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

  # # #Update column names -- column names are not consistent throughout the files, so make sure that the file headers are labeled
  # # #consistently is important
  #
  # #Now it is time to clean up the data. Data cleaning comes after data flagging so that everything will be flagged and then
  # #values that are deemed too high or low can be removed. This process is done before averaging the data so that bad data
  # #is not included in the average.
  #
  #Removing Negative Values and replacing with NA
  raw_data$temperature.val <- replace(raw_data$temperature.val, raw_data$temperature.val < 0, NA)
  raw_data$pm10.val <- replace(raw_data$pm10.val, raw_data$pm10.val < -5, NA)
  raw_data$pm25_p.val <- replace(raw_data$pm25_p.val, raw_data$pm25_p.val < -5, NA)
  raw_data$pm25_r.val <- replace(raw_data$pm25_r.val, raw_data$pm25_r.val < -5, NA)
  if("pm25_r2" %in% colnames(raw_data)){
  raw_data$pm25_r2.val <- replace(raw_data$pm25_r2.val, raw_data$pm25_r2.val < -5, NA)}
  raw_data$humidity.val <- replace(raw_data$humidity.val, raw_data$humidity.val < 0, NA)
  
  #Removing High Values and replacing with NA
  raw_data$pm10.val <- replace(raw_data$pm10.val, raw_data$pm10.val > 500, NA)
  raw_data$pm25_p.val <- replace(raw_data$pm25_p.val, raw_data$pm25_p.val >500, NA)
  raw_data$pm25_r.val <- replace(raw_data$pm25_r.val, raw_data$pm25_r.val >500, NA)
  
  if("pm25_r2" %in% colnames(raw_data)){
  raw_data$pm25_r2.val <- replace(raw_data$pm25_r2.val, raw_data$pm25_r2.val >500, NA)}
  
  raw_data$humidity.val <- replace(raw_data$humidity.val, raw_data$humidity.val > 100, NA)
  
  #print(raw_data)
  #Now that the flags are in place and the data is cleaned, it can be averaged. The following code uses an averaging function
  #from OpenAir, an R package that was specifically developed for Air Quality data analysis.
  #Because the data is not taken exactly every minute (and this messes up the function), the data is first averaged to every minute
  #Then the data can be averaged to every hour.

  #Averaging the Time to 1 hour
  raw_data <- timeAverage(raw_data, avg.time = "2 min")
  raw_data <- timeAverage(raw_data, avg.time = "hour", data.thresh = 75, interval = "2 min")
  
  #print(raw_data)
  #This step just makes sure that the sigfigs are consistent and that the file does not get too long.
  #Make all sigfigs consistent
  raw_data$pm25_p.val <- signif(raw_data$pm25_p.val, 3)
  raw_data$pm10.val <- signif(raw_data$pm10.val, 3)
  raw_data$temperature.val <- signif(raw_data$temperature.val, 3)
  raw_data$humidity.val <- signif(raw_data$humidity.val, 3)
  raw_data$pm25_r.val <- signif(raw_data$pm25_r.val, 3)
  
  if("pm25_r2" %in% colnames(raw_data)){
  raw_data$pm25_r2.val <- signif(raw_data$pm25_r2.val, 3)}
  
  # #This step takes the wide format data and merges the columns into a long format. The column with all of the parameters has to be
  # #renamed from what the function gives otherwise.
  
  #Merge all of the data into a long format
  raw_data <- merged.stack(raw_data, var.stubs = c("val", "flag"),
                            sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
                              ".", "", .time_1, fixed = TRUE)][]

  names(raw_data)[2] <- "parameter"
  
  #
  # #After the files are merged that should be, additional information about the sensor can be added to other columns that are
  # #not included in the long format.
  # #Add ID and sensor type to file, make everything in good format
  #raw_data$SensorType <- "Lunar"
  raw_data$ID <- shortname
  #
  # #This piece of code just makes sure that everything is lined up in the order that makes the most sense
  raw_data <- subset(raw_data, select = c("date","ID", "parameter", "val", "flag"))
  #
  # #Add units of each of the parameters
  library(dplyr)
  raw_data <- mutate(raw_data, parameter_units =
                        ifelse(parameter == "pm25_p", "ug/m3",
                               ifelse(parameter == "pm25_r", "ug/m3",
                                      ifelse(parameter == "pm25_r2", "ug/m3",
                                        ifelse(parameter == "pm10", "ug/m3",
                                            ifelse(parameter == "temperature", "F", ""))))))

  
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
}