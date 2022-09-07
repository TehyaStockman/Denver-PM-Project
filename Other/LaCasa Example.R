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


#---------------- la Casa ------------------

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


#-----------------
