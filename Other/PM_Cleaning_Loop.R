install.packages("data.table")
install.packages("dplyer")
install.packages("readr")
install.packages("lubridate")
install.packages("fs")
install.packages("tidyverse")
install.packages("chron")
install.packages("stringr")
install.packages("openair")
install.packages("naniar")
install.packages("pracma")
install.packages("splitstackshape")

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


# file_paths_Jan <- fs::dir_ls("202101_copy")
# file_paths_Jan %>% map(function(path) {
#   read_csv(path)
# })
# 
# file_paths <- fs::dir_ls("202101_copy")

setwd("/Users/tehyastockman/DDPHE/PM_Data_Cleaning/202102_copy")
AQ_files <- list.files()

#P25, PM25_p, pm25_p are processed data
#PM2_5 and pm25_r are raw data

#____________________________ Looping Through Data _____________________________________
#Creating a dictionary of file names to look up that link to data tables
data <- list()
AQ_data <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(AQ_data) <- c("date","ID", "SensorType", "Lat", "Long", "parameter", "val", "flag", "parameter_units")

for (filename in AQ_files){
  shortname <- str_match(filename, "(.*)_[0-9]*-[0-9]*\\.csv")
  shortname <- shortname[2]
  #print(shortname)
  temp_data <- read.csv(filename,skip = 3, header = T)
  
  if("PM_2_5_P" %in% colnames(temp_data))
    {names(temp_data)[names(temp_data) == "PM2_5_P"] <- "P25"}
  
  if(!("P25" %in% colnames(temp_data)))
    {temp_data$P25 <- NA}
  
  #Putting this on here first so that we can only keep columns we want and can more easily
  #get rid of columns we do not want
  print(colnames(temp_data))
  keeps <- c("Date", "Lat", "Long", "PM10", "PM2_5", "P25", "Temp", "Hmdty")
  temp_data <- temp_data[keeps]

  
  # #Files are named with "blabla".val because this will help lining up a similar naming structure with the corresponding flags
  # #By making the naming consistent, merging the data into a long format is simple to do
  names(temp_data)[1] <- "date"
  names(temp_data)[names(temp_data) == "PM10"] <- "pm10.val"
  names(temp_data)[names(temp_data) == "PM2_5"] <- "pm25_r.val"
  names(temp_data)[names(temp_data) == "P25"] <- "pm25_p.val"
  names(temp_data)[names(temp_data) == "Temp"] <- "temperature.val"
  names(temp_data)[names(temp_data) == "Hmdty"] <- "humidity.val"
  
  
  #Setting Up Date/Time Separately
  #The date and time are broken up and then put back together in a different format because the package OpenAir uses British 
  #date/time format. If we do not do this, then it gets the month/day mixed up and will not average correctly.
  temp_data$Date<- as.Date(sapply(temp_data[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}),
                           format = "%m/%d/%Y")
  temp_data$Time<- chron(times = sapply(temp_data[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}),
                                format = "hh:mm:ss")

  #Concatenating date and time back together and formatting the column to be a date/time
  temp_data$date <- str_c(temp_data$Date," ", temp_data$Time)
  temp_data$date <- as.POSIXct(temp_data$date)

  #Getting rid of date/time columns separated
  temp_data <- temp_data[-c(9:10)]
  # #
  # #Flagging data to be cleaned, adding columns of flags for low, normal, and high as -1, 0, and 1.
  # #This way these flags can be averaged to understand why NA values appear for a value averaged over an hour.
  temp_data <- mutate(temp_data, pm25_p.flag =
                               ifelse(pm25_p.val < 0, -1,
                                      ifelse(pm25_p.val > 500, 1,
                                             0)))

  temp_data <- mutate(temp_data, pm25_r.flag =
                               ifelse(pm25_r.val < 0, -1,
                                      ifelse(pm25_r.val > 500, 1,
                                             0)))

  temp_data <- mutate(temp_data, pm10.flag =
                               ifelse(pm10.val < 0, -1,
                                      ifelse(pm10.val > 500, 1,
                                             0)))

  temp_data <- mutate(temp_data, temperature.flag =
                               ifelse(temperature.val < 0, -1,
                                      ifelse(temperature.val > 100, 1,
                                             0)))

  temp_data <- mutate(temp_data, humidity.flag =
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
  temp_data$temperature.val <- replace(temp_data$temperature.val, temp_data$temperature.val < 0, NA)
  temp_data$pm10.val <- replace(temp_data$pm10.val, temp_data$pm10.val < 0, NA)
  temp_data$pm25_p.val <- replace(temp_data$pm25_p.val, temp_data$pm25_p.val < 0, NA)
  temp_data$pm25_r.val <- replace(temp_data$pm25_r.val, temp_data$pm25_r.val < 0, NA)
  temp_data$humidity.val <- replace(temp_data$humidity.val, temp_data$humidity.val < 0, NA)

  #Removing High Values and replacing with NA
  temp_data$pm10.val <- replace(temp_data$pm10.val, temp_data$pm10.val > 500, NA)
  temp_data$pm25_p.val <- replace(temp_data$pm25_p.val, temp_data$pm25_p.val >500, NA)
  temp_data$pm25_r.val <- replace(temp_data$pm25_r.val, temp_data$pm25_r.val >500, NA)
  temp_data$humidity.val <- replace(temp_data$humidity.val, temp_data$humidity.val > 100, NA)


  #Now that the flags are in place and the data is cleaned, it can be averaged. The following code uses an averaging function
  #from OpenAir, an R package that was specifically developed for Air Quality data analysis.
  #Because the data is not taken exactly every minute (and this messes up the function), the data is first averaged to every minute
  #Then the data can be averaged to every hour.

  #Averaging the Time to 1 hour
  temp_data <- timeAverage(temp_data, avg.time = "min")
  temp_data <- timeAverage(temp_data, avg.time = "hour", data.thresh = 75, interval = "min")


  #This step just makes sure that the sigfigs are consistent and that the file does not get too long.
  #Make all sigfigs consistent
  temp_data$pm25_p.val <- signif(temp_data$pm25_p.val, 3)
  temp_data$pm10.val <- signif(temp_data$pm10.val, 3)
  temp_data$temperature.val <- signif(temp_data$temperature.val, 3)
  temp_data$humidity.val <- signif(temp_data$humidity.val, 3)
  temp_data$pm25_r.val <- signif(temp_data$pm25_r.val, 3)

  # #This step takes the wide format data and merges the columns into a long format. The column with all of the parameters has to be
  # #renamed from what the function gives otherwise.

  #Merge all of the data into a long format
  temp_data <- merged.stack(temp_data, var.stubs = c("val", "flag"),
                                      sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
                                        ".", "", .time_1, fixed = TRUE)][]

  names(temp_data)[4] <- "parameter"
  #
  # #After the files are merged that should be, additional information about the sensor can be added to other columns that are
  # #not included in the long format.
  # #Add ID and sensor type to file, make everything in good format
  temp_data$SensorType <- "Lunar"
  temp_data$ID <- shortname
  #
  # #This piece of code just makes sure that everything is lined up in the order that makes the most sense
  temp_data <- subset(temp_data, select = c("date","ID", "SensorType", "Lat", "Long", "parameter", "val", "flag"))
  #
  # #Add units of each of the parameters
  library(dplyr)
  temp_data <- mutate(temp_data, parameter_units =
                                  ifelse(parameter == "pm25_p", "ug/m3",
                                         ifelse(parameter == "pm25_r", "ug/m3",
                                                ifelse(parameter == "pm10", "ug/m3",
                                                       ifelse(parameter == "temperature", "F", "")))))

  # #Update the flags to be high, missing, low. The flags show that there were high or low values that were removed during the hour.
  # #A flag of "high missing" means that there were enough values in the hour that were high to not meet the 75% threshold
  # #It also means that the values were generally high, as the flags are averaged to give an hourly flag
  # #If there are just missing values and it is not high, then only "missing" will be shown on the flag
  temp_data$flag <- replace(temp_data$flag,temp_data$flag < 0 & is.na(temp_data$val), "low, missing")
  temp_data$flag <- replace(temp_data$flag,temp_data$flag > 0 & is.na(temp_data$val), "high, missing")
  temp_data$flag <- replace(temp_data$flag,temp_data$flag > 0 & !is.na(temp_data$val), "high")
  temp_data$flag <- replace(temp_data$flag,temp_data$flag < 0 & !is.na(temp_data$val), "low")
  temp_data$flag <- replace(temp_data$flag,is.na(temp_data$val) & temp_data$flag == 0, "missing")
  temp_data$flag <- replace(temp_data$flag,!is.na(temp_data$val) & temp_data$flag == 0, "")


  #Add Data to list
  data[shortname] <- list(temp_data)
  AQ_data <- rbind(AQ_data, temp_data)
}

write.csv(AQ_data,"../Feb2021.csv", row.names = FALSE)

