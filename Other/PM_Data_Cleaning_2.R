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


setwd("202101")

##If there is a problem with the following code, there are a few things that you should check.
#Make sure the working directory is set.
#Make sure all of the libraries are loaded and up to date

#--------------------------------------Canary Sensor Example ---------------------------------
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

#La Casa Collocated Data -- reading in the csv file
LaCasa_Collo <- read.csv("167_La-Casa-Collo-(CS5)_20210101-20210131.csv", skip = 3, header = T)

#Check which is processed vs. unprocessed data
keeps <- c("Date", "Lat", "Long", "PM10", "PM2_5", "P25", "Temp", "Hmdty")
LaCasa_Collo_167 <- LaCasa_Collo[keeps]

#Update column names -- column names are not consistent throughout the files, so make sure that the file headers are labeled 
#consistently is important
#Files are named with "blabla".val because this will help lining up a similar naming structure with the corresponding flags
#By making the naming consistent, merging the data into a long format is simple to do
names(LaCasa_Collo_167)[1] <- "date"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "PM10"] <- "pm10.val"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "PM2_5"] <- "pm25_r.val"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "P25"] <- "pm25_p.val"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "Temp"] <- "temperature.val"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "Hmdty"] <- "humidity.val"



#Setting Up Date/Time Separately
#The date and time are broken up and then put back together in a different format because the package OpenAir uses British 
#date/time format. If we do not do this, then it gets the month/day mixed up and will not average correctly.
LaCasa_Collo_167$Date<- as.Date(sapply(LaCasa_Collo_167[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                                format = "%m/%d/%Y")
LaCasa_Collo_167$Time<- chron(times = sapply(LaCasa_Collo_167[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}), 
                              format = "hh:mm:ss")

#Concatenating date and time back together and formatting the column to be a date/time
LaCasa_Collo_167$date <- str_c(LaCasa_Collo_167$Date," ", LaCasa_Collo_167$Time)
LaCasa_Collo_167$date <- as.POSIXct(LaCasa_Collo_167$date)

LaCasa_Collo_167 <- LaCasa_Collo_167[-c(9:10)]


#Flagging data to be cleaned, adding columns of flags for low, normal, and high as -1, 0, and 1. 
#This way these flags can be averaged to understand why NA values appear for a value averaged over an hour.

LaCasa_Collo_167 <- mutate(LaCasa_Collo_167, pm25_p.flag = 
                        ifelse(pm25_p.val < 0, -1,
                               ifelse(pm25_p.val > 500, 1,
                                      0))) 

LaCasa_Collo_167 <- mutate(LaCasa_Collo_167, pm25_r.flag = 
                             ifelse(pm25_r.val < 0, -1,
                                    ifelse(pm25_r.val > 500, 1,
                                           0))) 

LaCasa_Collo_167 <- mutate(LaCasa_Collo_167, pm10.flag = 
                        ifelse(pm10.val < 0, -1,
                               ifelse(pm10.val > 500, 1,
                                      0))) 

LaCasa_Collo_167 <- mutate(LaCasa_Collo_167, temperature.flag = 
                        ifelse(temperature.val < 0, -1,
                               ifelse(temperature.val > 100, 1,
                                      0)))

LaCasa_Collo_167 <- mutate(LaCasa_Collo_167, humidity.flag = 
                        ifelse(humidity.val < 0, -1,
                               ifelse(humidity.val > 100, 1,
                                      0))) 


#Now it is time to clean up the data. Data cleaning comes after data flagging so that everything will be flagged and then
#values that are deemed too high or low can be removed. This process is done before averaging the data so that bad data 
#is not included in the average.

#Removing Negative Values and replacing with NA
LaCasa_Collo_167$temperature.val <- replace(LaCasa_Collo_167$temperature.val, LaCasa_Collo_167$temperature.val < 0, NA)
LaCasa_Collo_167$pm10.val <- replace(LaCasa_Collo_167$pm10.val, LaCasa_Collo_167$pm10.val < 0, NA)
LaCasa_Collo_167$pm25_p.val <- replace(LaCasa_Collo_167$pm25_p.val, LaCasa_Collo_167$pm25_p.val < 0, NA)
LaCasa_Collo_167$humidity.val <- replace(LaCasa_Collo_167$humidity.val, LaCasa_Collo_167$humidity.val < 0, NA)

#Removing High Values and replacing with NA
LaCasa_Collo_167$pm10.val <- replace(LaCasa_Collo_167$pm10.val, LaCasa_Collo_167$pm10.val > 500, NA)
LaCasa_Collo_167$pm25_p.val <- replace(LaCasa_Collo_167$pm25_p.val, LaCasa_Collo_167$pm25_p.val >500, NA)
LaCasa_Collo_167$humidity.val <- replace(LaCasa_Collo_167$humidity.val, LaCasa_Collo_167$humidity.val > 100, NA)

#Now that the flags are in place and the data is cleaned, it can be averaged. The following code uses an averaging function
#from OpenAir, an R package that was specifically developed for Air Quality data analysis.
#Because the data is not taken exactly every minute (and this messes up the function), the data is first averaged to every minute
#Then the data can be averaged to every hour.

#Averaging the Time to 1 hour
LaCasa_Min <- timeAverage(LaCasa_Collo_167, avg.time = "min")
LaCasa_Collo_hourly <- timeAverage(LaCasa_Min, avg.time = "hour", data.thresh = 75, interval = "min")

#This step just makes sure that the sigfigs are consistent and that the file does not get too long.
#Make all sigfigs consistent
LaCasa_Collo_hourly$pm25_p.val <- signif(LaCasa_Collo_hourly$pm25_p.val, 3)
LaCasa_Collo_hourly$pm10.val <- signif(LaCasa_Collo_hourly$pm10.val, 3)
LaCasa_Collo_hourly$temperature.val <- signif(LaCasa_Collo_hourly$temperature.val, 3)
LaCasa_Collo_hourly$humidity.val <- signif(LaCasa_Collo_hourly$humidity.val, 3)
LaCasa_Collo_hourly$pm25_r.val <- signif(LaCasa_Collo_hourly$pm25_r.val, 3)

#This step takes the wide format data and merges the columns into a long format. The column with all of the parameters has to be
#renamed from what the function gives otherwise.

#Merge all of the data into a long format
install.packages("splitstackshape")
library(splitstackshape)

LaCasa_Collo_hourly <- merged.stack(LaCasa_Collo_hourly, var.stubs = c("val", "flag"), 
                                   sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
                                     ".", "", .time_1, fixed = TRUE)][]

names(LaCasa_Collo_hourly)[4] <- "parameter"

#After the files are merged that should be, additional information about the sensor can be added to other columns that are
#not included in the long format.
#Add ID and sensor type to file, make everything in good format
LaCasa_Collo_hourly$ID <- "La Casa Collo"
LaCasa_Collo_hourly$SensorType <- "Lunar"

#This piece of code just makes sure that everything is lined up in the order that makes the most sense
LaCasa_Collo_hourly <- subset(LaCasa_Collo_hourly, select = c("date","ID", "SensorType", "Lat", "Long", "parameter", "val", "flag"))


#Add units of each of the parameters
library(dplyr)
LaCasa_Collo_hourly <- mutate(LaCasa_Collo_hourly, parameter_units = 
                               ifelse(parameter == "pm25_p", "ug/m3",
                                      ifelse(parameter == "pm25_r", "ug/m3",
                                          ifelse(parameter == "pm10", "ug/m3",
                                             ifelse(parameter == "temperature", "F", "")))))

#Update the flags to be high, missing, low. The flags show that there were high or low values that were removed during the hour.
#A flag of "high missing" means that there were enough values in the hour that were high to not meet the 75% threshold
#It also means that the values were generally high, as the flags are averaged to give an hourly flag
#If there are just missing values and it is not high, then only "missing" will be shown on the flag
LaCasa_Collo_hourly$flag <- replace(LaCasa_Collo_hourly$flag,LaCasa_Collo_hourly$flag < 0 & is.na(LaCasa_Collo_hourly$val), "low, missing")
LaCasa_Collo_hourly$flag <- replace(LaCasa_Collo_hourly$flag,LaCasa_Collo_hourly$flag > 0 & is.na(LaCasa_Collo_hourly$val), "high, missing")
LaCasa_Collo_hourly$flag <- replace(LaCasa_Collo_hourly$flag,LaCasa_Collo_hourly$flag > 0 & !is.na(LaCasa_Collo_hourly$val), "high")
LaCasa_Collo_hourly$flag <- replace(LaCasa_Collo_hourly$flag,LaCasa_Collo_hourly$flag < 0 & !is.na(LaCasa_Collo_hourly$val), "low")
LaCasa_Collo_hourly$flag <- replace(LaCasa_Collo_hourly$flag,is.na(LaCasa_Collo_hourly$val) & LaCasa_Collo_hourly$flag == 0, "missing")
LaCasa_Collo_hourly$flag <- replace(LaCasa_Collo_hourly$flag,!is.na(LaCasa_Collo_hourly$val) & LaCasa_Collo_hourly$flag == 0, "")


#---------------------Reference Monitors------------------------#
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
#Variables we want:
#  Date, Time, Location (lat/long), PM2.5, PM10, Humidity, 
#   Temperature, Wind Direction, Wind Speed

#CAMP Reference Monitor Data
CAMP_MET <- read.csv("170_CAMP_20210101-20210131.csv", skip = 3, header = T)

#Check which is processed vs. unprocessed data
keeps <- c("Date", "Outdoor.Temperature", "Wind.Direction", "Wind.Speed")
CAMP_MET <- CAMP_MET[keeps]
names(CAMP_MET)[1] <- "date"
names(CAMP_MET)[2] <- "temperature.val"
names(CAMP_MET)[3] <- "wd.val"
names(CAMP_MET)[4] <- "ws.val"
#"Outdoor.Temperature", "Wind.Direction", "Wind.Speed"

#Setting Up Date/Time Separately
CAMP_MET$Date<- as.Date(sapply(CAMP_MET[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                        format = "%m/%d/%Y")
CAMP_MET$Time<- chron(times = sapply(CAMP_MET[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}), 
                      format = "hh:mm:ss")

#Concatenating date and time back together and formatting the column to be a date/time
CAMP_MET$date <- str_c(CAMP_MET$Date," ", CAMP_MET$Time)
CAMP_MET$date <- as.POSIXct(CAMP_MET$date)

CAMP_MET <- CAMP_MET[-c(5:6)]

#Adding flags
CAMP_MET$temperature.flag <- 0
CAMP_MET$wd.flag <- 0
CAMP_MET$ws.flag <- 0


#Averaging the data to 1 hour interval, threshold of 75%
CAMP_MET_hourly <- timeAverage(CAMP_MET, avg.time = "hour", data.thresh = 75)

#Merge all of the data into a long format
install.packages("splitstackshape")
library(splitstackshape)

CAMP_MET_hourly <- merged.stack(CAMP_MET_hourly, var.stubs = c("val", "flag"), 
                                    sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
                                      ".", "", .time_1, fixed = TRUE)][]

names(CAMP_MET_hourly)[2] <- "parameter"

#Add ID and sensor type to file, make everything in good format
CAMP_MET_hourly$ID <- "CAMP MET"
CAMP_MET_hourly$SensorType <- "State"

#Lat/Long
CAMP_MET_hourly$Lat <- 39.752068
CAMP_MET_hourly$Long <- -104.988365

CAMP_MET_hourly <- subset(CAMP_MET_hourly, select = c("date","ID", "SensorType", "Lat","Long", "parameter", "val", "flag"))


#Add units of each of the parameters
CAMP_MET_hourly <- mutate(CAMP_MET_hourly, parameter_units = 
                                ifelse(parameter == "ws", "mph",
                                       ifelse(parameter == "wd", "deg", 
                                                     ifelse(parameter == "temperature", "F", ""))))

#Update the flags to be high, missing, low. The flags show that there were high or low values that were removed during the hour.
#A flag of "high missing" means that there were enough values in the hour that were high to not meet the 75% threshold
#It also means that the values were generally high, as the flags are averaged to give an hourly flag
#If there are just missing values and it is not high, then only "missing" will be shown on the flag
CAMP_MET_hourly$flag <- replace(CAMP_MET_hourly$flag, CAMP_MET_hourly$flag == 0, "")



#------------------------------------
#LaCasa Reference Monitor
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
#Variables we want:
#  Date, Time, Location (lat/long), PM2.5, PM10, Humidity, 
#   Temperature, Wind Direction, Wind Speed

LaCasa_70 <- read.csv("70_La-Casa_20210101-20210131.csv", skip = 3, header = T)
keeps <- c("Date","PM10", "PM2.5")
LaCasa_70 <- LaCasa_70[keeps]

names(LaCasa_70)[names(LaCasa_70) == "PM10"] <- "pm10.val"
names(LaCasa_70)[names(LaCasa_70) == "PM2.5"] <- "pm25_p.val"
names(LaCasa_70)[names(LaCasa_70) == "Date"] <- "date"

#Flagging data to be cleaned, adding columns of flags for low, normal, and high as -1, 0, and 1. 
#This way these flags can be averaged to understand why NA values appear
###CHANGE VALUES for FINAL
LaCasa_70 <- mutate(LaCasa_70, pm25_p.flag = 
                             ifelse(pm25_p.val < 0, -1,
                                    ifelse(pm25_p.val > 500, 1,
                                           0))) 

LaCasa_70 <- mutate(LaCasa_70, pm25_r.flag = 
                             ifelse(pm25_r.val < 0, -1,
                                    ifelse(pm25_r.val > 500, 1,
                                           0))) 

LaCasa_70 <- mutate(LaCasa_70, pm10.flag = 
                             ifelse(pm10.val < 0, -1,
                                    ifelse(pm10.val > 500, 1,
                                           0))) 


#Removing Negative Values and replacing with NA
LaCasa_70$pm10.val <- replace(LaCasa_70$pm10.val, LaCasa_70$pm10.val < 0, NA)
LaCasa_70$pm25_p.val <- replace(LaCasa_70$pm25_p.val, LaCasa_70$pm25_p.val < 0, NA)


#Removing High Values and replacing with NA
LaCasa_70$pm10.val <- replace(LaCasa_70$pm10.val, LaCasa_70$pm10.val > 500, NA)
LaCasa_70$pm25_p.val <- replace(LaCasa_70$pm25_p.val,LaCasa_70$pm25_p.val >500, NA)


#Setting Up Date/Time Separately
LaCasa_70$Date<- as.Date(sapply(LaCasa_70[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                         format = "%m/%d/%Y")
LaCasa_70$Time<- chron(times = sapply(LaCasa_70[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}), 
                       format = "hh:mm:ss")

#Concatenating date and time back together and formatting the column to be a date/time
LaCasa_70$date <- str_c(LaCasa_70$Date," ", LaCasa_70$Time)
LaCasa_70$date <- as.POSIXct(LaCasa_70$date)

#Averaging the data to 1 hour interval, threshold of 75%
LaCasa_Airnow_hourly <- timeAverage(LaCasa_70, avg.time = "hour", data.thresh = 75)

LaCasa_Airnow_hourly <- LaCasa_Airnow_hourly[-c(6:7)]

#Round the values so there are not so many sigfigs -- can modify to match the uncertainty of the instrument
LaCasa_Airnow_hourly$pm25_p.val <- signif(LaCasa_Airnow_hourly$pm25_p.val, 3)
LaCasa_Airnow_hourly$pm10.val <- signif(LaCasa_Airnow_hourly$pm10.val, 3)


#Merge all of the data into a long format
library(splitstackshape)

LaCasa_Airnow_hourly <- merged.stack(LaCasa_Airnow_hourly, var.stubs = c("val", "flag"), 
                                    sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
                                      ".", "", .time_1, fixed = TRUE)][]

names(LaCasa_Airnow_hourly)[2] <- "parameter"

#Add ID and sensor type to file, make everything in good format
LaCasa_Airnow_hourly$ID <- "La Casa AirNow"
LaCasa_Airnow_hourly$SensorType <- "AirNow"

#Add Lat/long values
LaCasa_Airnow_hourly$Lat <- 39.78450
LaCasa_Airnow_hourly$Long <- -105.02116


LaCasa_Airnow_hourly <- subset(LaCasa_Airnow_hourly, select = c("date","ID", "SensorType", "Lat", "Long", "parameter", "val", "flag"))


#Add units of each of the parameters
library(dplyr)
LaCasa_Airnow_hourly <- mutate(LaCasa_Airnow_hourly, parameter_units = 
                                ifelse(parameter == "pm25_p", "ug/m3",
                                              ifelse(parameter == "pm10", "ug/m3", ""
                                                     )))

#Update the flags to be high, missing, low. The flags show that there were high or low values that were removed during the hour.
#A flag of "high missing" means that there were enough values in the hour that were high to not meet the 75% threshold
#It also means that the values were generally high, as the flags are averaged to give an hourly flag
#If there are just missing values and it is not high, then only "missing" will be shown on the flag
LaCasa_Airnow_hourly$flag <- replace(LaCasa_Airnow_hourly$flag,LaCasa_Airnow_hourly$flag < 0 & is.na(LaCasa_Airnow_hourly$val), "low, missing")
LaCasa_Airnow_hourly$flag <- replace(LaCasa_Airnow_hourly$flag,LaCasa_Airnow_hourly$flag > 0 & is.na(LaCasa_Airnow_hourly$val), "high, missing")
LaCasa_Airnow_hourly$flag <- replace(LaCasa_Airnow_hourly$flag,LaCasa_Airnow_hourly$flag > 0 & !is.na(LaCasa_Airnow_hourly$val), "high")
LaCasa_Airnow_hourly$flag <- replace(LaCasa_Airnow_hourly$flag,LaCasa_Airnow_hourly$flag < 0 & !is.na(LaCasa_Airnow_hourly$val), "low")
LaCasa_Airnow_hourly$flag <- replace(LaCasa_Airnow_hourly$flag,is.na(LaCasa_Airnow_hourly$val) & LaCasa_Airnow_hourly$flag == 0, "missing")
LaCasa_Airnow_hourly$flag <- replace(LaCasa_Airnow_hourly$flag,!is.na(LaCasa_Airnow_hourly$val) & LaCasa_Airnow_hourly$flag == 0, "")

#---------------------------------------------------------------------------------------
#------------------------------Clarity Sensor Example -----------------------------------
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

CollegeView <- read.csv("2512_College-View-Elem-(A2JCKC55_2.3)_20210101-20210131.csv", skip = 3, header = T)
keeps <- c("Date","pm10ConcMass", "pm2_5ConcMass", "relHumid","temperature")
CollegeView <- CollegeView[keeps]
names(CollegeView)[1] <- "date"
names(CollegeView)[2] <- "pm10.val"
names(CollegeView)[3] <- "pm25_p.val"
names(CollegeView)[4] <- "humidity.val"
names(CollegeView)[5] <- "temperature.val"

CollegeView$temperature.val <- CollegeView$temperature.val*9/5 + 32

#Setting Up Date/Time Separately
CollegeView$Date<- as.Date(sapply(CollegeView[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}),
                           format = "%m/%d/%Y")
CollegeView$Time<- chron(times = sapply(CollegeView[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}),
                         format = "hh:mm:ss")

#Concatenating date and time back together and formatting the column to be a date/time
CollegeView$date <- str_c(CollegeView$Date," ", CollegeView$Time)
CollegeView$date <- as.POSIXct(CollegeView$date)

CollegeView <- CollegeView[-c(6:7)]

#-----------------------------

#Flagging data to be cleaned, adding columns of flags for low, normal, and high as -1, 0, and 1. 
#This way these flags can be averaged to understand why NA values appear
###CHANGE VALUES for FINAL

CollegeView <- mutate(CollegeView, pm25_p.flag = 
                               ifelse(pm25_p.val < 0, -1,
                                      ifelse(pm25_p.val > 500, 1,
                                             0))) 
CollegeView <- mutate(CollegeView, pm10.flag = 
                        ifelse(pm10.val < 0, -1,
                               ifelse(pm10.val > 500, 1,
                                      0))) 

CollegeView <- mutate(CollegeView, temperature.flag = 
                        ifelse(temperature.val < 0, -1,
                               ifelse(temperature.val > 100, 1,
                                      0)))

CollegeView <- mutate(CollegeView, humidity.flag = 
                        ifelse(humidity.val < 0, -1,
                               ifelse(humidity.val > 100, 1,
                                      0))) 

#Cleaning the data to make sure we are not showing values that do not make sense
#Removing Negative Values and replacing with NA
CollegeView$temperature.val <- replace(CollegeView$temperature.val, CollegeView$temperature.val < 0, NA)
CollegeView$pm10.val <- replace(CollegeView$pm10.val, CollegeView$pm10.val < 0, NA)
CollegeView$pm25_p.val <- replace(CollegeView$pm25_p.val, CollegeView$pm25_p.val < 0, NA)
CollegeView$humidity.val <- replace(CollegeView$humidity.val, CollegeView$humidity.val < 0, NA)

#Removing High Values and replacing with NA
CollegeView$pm10.val <- replace(CollegeView$pm10.val, CollegeView$pm10.val > 500, NA)
CollegeView$pm25_p.val <- replace(CollegeView$pm25_p.val, CollegeView$pm25_p.val > 500, NA)
CollegeView$humidity.val <- replace(CollegeView$humidity.val, CollegeView$humidity.val > 100, NA)

#-------------------------------
#Averaging the data to 1 hour interval, threshold of 75%
CollegeView_15min <- timeAverage(CollegeView, avg.time = "15 min", start.date = "2021-01-01 00:00:00")
CollegeView_hourly <- timeAverage(CollegeView_15min, avg.time = "hour", data.thresh = 0, interval = "15 min")

#Rounding hourly data (shrinks file size)
#Round the values so there are not so many sigfigs -- can modify to match the uncertainty of the instrument
CollegeView_hourly$pm25_p.val <- signif(CollegeView_hourly$pm25_p.val, 3)
CollegeView_hourly$pm10.val <- signif(CollegeView_hourly$pm10.val, 3)
CollegeView_hourly$temperature.val <- signif(CollegeView_hourly$temperature.val, 4)
CollegeView_hourly$humidity.val <- signif(CollegeView_hourly$humidity.val, 3)


#-------------------------------
#Reorganize
CollegeView_hourly <- subset(CollegeView_hourly, select = c("date", "pm25_p.val", "pm10.val", "temperature.val", "humidity.val", "Lat.val", "Long.val", "pm25_p.flag", "pm10.flag", "temperature.flag", "humidity.flag", "Lat.flag", "Long.flag"))

#Merge everything to be long format, separating everything with the values and flags for each parameter
install.packages("splitstackshape")
library(splitstackshape)

CollegeView_hourly <- merged.stack(CollegeView_hourly, var.stubs = c("val", "flag"), 
             sep = "var.stubs", atStart = FALSE)[, .time_1 := sub(
               ".", "", .time_1, fixed = TRUE)][]

names(CollegeView_hourly)[2] <- "parameter"


#Add ID and sensor type to file, make everything in good format
CollegeView_hourly$ID <- "College View"
CollegeView_hourly$SensorType <- "Clarity"
#-------------------------------
##This part would create columns of values for the sensor ID, sensor type, and lat/long
CollegeView_hourly$Lat <- 39.668157000
CollegeView_hourly$Long <- -105.023141000

CollegeView_hourly <- subset(CollegeView_hourly, select = c("date","ID", "SensorType", "Lat", "Long", "parameter", "val", "flag"))


#Add units of each of the parameters
library(dplyr)
CollegeView_hourly <- mutate(CollegeView_hourly, parameter_units = 
                               ifelse(parameter == "pm25_p", "ug/m3",
                                      ifelse(parameter == "pm10", "ug/m3",
                                             ifelse(parameter == "temperature", "F", ""))))

#Update the flags to be high, missing, low. The flags show that there were high or low values that were removed during the hour.
#A flag of "high missing" means that there were enough values in the hour that were high to not meet the 75% threshold
#It also means that the values were generally high, as the flags are averaged to give an hourly flag
#If there are just missing values and it is not high, then only "missing" will be shown on the flag
CollegeView_hourly$flag <- replace(CollegeView_hourly$flag,CollegeView_hourly$flag < 0 & is.na(CollegeView_hourly$val), "low, missing")
CollegeView_hourly$flag <- replace(CollegeView_hourly$flag,CollegeView_hourly$flag > 0 & is.na(CollegeView_hourly$val), "high, missing")
CollegeView_hourly$flag <- replace(CollegeView_hourly$flag,CollegeView_hourly$flag > 0 & !is.na(CollegeView_hourly$val), "high")
CollegeView_hourly$flag <- replace(CollegeView_hourly$flag,CollegeView_hourly$flag < 0 & !is.na(CollegeView_hourly$val), "low")
CollegeView_hourly$flag <- replace(CollegeView_hourly$flag,is.na(CollegeView_hourly$val) & CollegeView_hourly$flag == 0, "missing")
CollegeView_hourly$flag <- replace(CollegeView_hourly$flag,!is.na(CollegeView_hourly$val) & CollegeView_hourly$flag == 0, "")



Jan2021 <- rbind(CollegeView_hourly, LaCasa_Airnow_hourly, LaCasa_Collo_hourly, CAMP_MET_hourly)

write.csv(Jan2021,"Jan2021.csv", row.names = FALSE)
Jan2021_new <- read.csv("Jan2021.csv", header = TRUE)


















