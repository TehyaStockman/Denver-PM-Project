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

file_paths_Jan <- fs::dir_ls("202101")
file_paths_Jan %>% map(function(path) {
  read_csv(path)
  })

file_paths <- fs::dir_ls("202101")

setwd("202101")
Jan_files <- list.files()

#P25, PM25_p, pm25_p are processed data
#PM2_5 and pm25_r are raw data

#____________________________ Looping Through Data _____________________________________
#Creating a dictionary of file names to look up that link to data tables
data <- list()

for (filename in Jan_files){
  shortname <- str_match(filename, "(.*)_[0-9]*-[0-9]*\\.csv")
  shortname <- shortname[2]
  #print(shortname)
  temp_data <- read.csv(filename,skip = 3, header = T)
  
  temp_data$Date<- as.Date(sapply(temp_data[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                                  format = "%m/%d/%Y")
  # temp_data$Time<- chron(times = sapply(temp_data[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}), 
  #                               format = "hh:mm:ss")
  temp_data$Sensor_ID <- shortname
  data[shortname] <- list(temp_data)
}

#------------Non-Reference Monitors----------------
#Variables we want:
#  Date, Time, Location (lat/long), PM2.5 (raw and processed), PM10, Humidity, Temperature

#CAMP Collocated Data
CAMP_Collo_177 <- read.csv("177_CAMP-Collo_20210101-20210131.csv", skip = 3, header = T)

keeps <- c("Date", "Lat", "Long", "PM10", "PM2_5","P25","Temp", "Hmdty")
CAMP_Collo_177 <- CAMP_Collo_177[keeps]

#Update column names
names(CAMP_Collo_177)[1] <- "date"
names(CAMP_Collo_177)[names(CAMP_Collo_177) == "PM10"] <- "pm10"
names(CAMP_Collo_177)[names(CAMP_Collo_177) == "PM2_5"] <- "pm25_r"
names(CAMP_Collo_177)[names(CAMP_Collo_177) == "P25"] <- "pm25_p"
names(CAMP_Collo_177)[names(CAMP_Collo_177) == "Temp"] <- "temperature"
names(CAMP_Collo_177)[names(CAMP_Collo_177) == "Hmdty"] <- "humidity"


#Removing Negative Values and replacing with NA
CAMP_Collo_177$temperature <- replace(CAMP_Collo_177$temperature, CAMP_Collo_177$temperature < 0, NA)
CAMP_Collo_177$pm10 <- replace(CAMP_Collo_177$pm10, CAMP_Collo_177$pm10 < 0, NA)
CAMP_Collo_177$pm25_p <- replace(CAMP_Collo_177$pm25_p, CAMP_Collo_177$pm25_p < 0, NA)
CAMP_Collo_177$humidity <- replace(CAMP_Collo_177$humidity, CAMP_Collo_177$humidity < 0, NA)

#Removing High Values and replacing with NA
CAMP_Collo_177$pm10 <- replace(CAMP_Collo_177$pm10, CAMP_Collo_177$pm10 > 500, NA)
CAMP_Collo_177$pm25_p <- replace(CAMP_Collo_177$pm25_p, CAMP_Collo_177$pm25_p >500, NA)
CAMP_Collo_177$humidity <- replace(CAMP_Collo_177$humidity, CAMP_Collo_177$humidity > 100, NA)


#Setting Up Date/Time Separately
CAMP_Collo_177$Date<- as.Date(sapply(CAMP_Collo_177[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                  format = "%m/%d/%Y")
CAMP_Collo_177$Time<- chron(times = sapply(CAMP_Collo_177[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}), 
                format = "hh:mm:ss")

#Concatenating date and time back together and formatting the column to be a date/time
CAMP_Collo_177$date <- str_c(CAMP_Collo_177$Date," ", CAMP_Collo_177$Time)
CAMP_Collo_177$date <- as.POSIXct(CAMP_Collo_177$date)

CAMP_Collo_177 <- CAMP_Collo_177[-c(9:10)]

CAMP_Collo_Min <- timeAverage(CAMP_Collo_177, avg.time = "min")
CAMP_Collo_hourly <- timeAverage(CAMP_Collo_Min, avg.time = "hour", data.thresh = 75, interval = "min")
CAMP_Collo_daily <- timeAverage(CAMP_Collo_hourly, avg.time = "day", data.thresh = 75)

CAMP_Collo_hourly$ID <- "CAMP Collo 177"
CAMP_Collo_hourly$SensorType <- "LunarOutpost"

#--------------------------------------Canary Sensor Example ---------------------------------
#La Casa Collocated Data
LaCasa_Collo_167 <- read.csv("167_La-Casa-Collo-(CS5)_20210101-20210131.csv", skip = 3, header = T)

#Check which is processed vs. unprocessed data
keeps <- c("Date", "Lat", "Long", "PM10", "PM2_5", "P25", "Temp", "Hmdty")
LaCasa_Collo_167 <- LaCasa_Collo_167[keeps]

#Update column names
names(LaCasa_Collo_167)[1] <- "date"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "PM10"] <- "pm10"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "PM2_5"] <- "pm25_r"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "P25"] <- "pm25_p"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "Temp"] <- "temperature"
names(LaCasa_Collo_167)[names(LaCasa_Collo_167) == "Hmdty"] <- "humidity"

#Removing Negative Values and replacing with NA
LaCasa_Collo_167$temperature <- replace(LaCasa_Collo_167$temperature, LaCasa_Collo_167$temperature < 0, NA)
LaCasa_Collo_167$pm10 <- replace(LaCasa_Collo_167$pm10, LaCasa_Collo_167$pm10 < 0, NA)
LaCasa_Collo_167$pm25_p <- replace(LaCasa_Collo_167$pm25_p, LaCasa_Collo_167$pm25_p < 0, NA)
LaCasa_Collo_167$humidity <- replace(LaCasa_Collo_167$humidity, LaCasa_Collo_167$humidity < 0, NA)

#Removing High Values and replacing with NA
LaCasa_Collo_167$pm10 <- replace(LaCasa_Collo_167$pm10, LaCasa_Collo_167$pm10 > 500, NA)
LaCasa_Collo_167$pm25_p <- replace(LaCasa_Collo_167pm25_p, LaCasa_Collo_167$pm25_p >500, NA)
LaCasa_Collo_167$humidity <- replace(LaCasa_Collo_167$humidity, LaCasa_Collo_167$humidity > 100, NA)


#Setting Up Date/Time Separately
LaCasa_Collo_167$Date<- as.Date(sapply(LaCasa_Collo_167[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                                format = "%m/%d/%Y")
LaCasa_Collo_167$Time<- chron(times = sapply(LaCasa_Collo_167[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}), 
                              format = "hh:mm:ss")

#Concatenating date and time back together and formatting the column to be a date/time
LaCasa_Collo_167$date <- str_c(LaCasa_Collo_167$Date," ", LaCasa_Collo_167$Time)
LaCasa_Collo_167$date <- as.POSIXct(LaCasa_Collo_167$date)

LaCasa_Collo_167 <- LaCasa_Collo_167[-c(9:10)]

LaCasa_Min <- timeAverage(LaCasa_Collo_167, avg.time = "min")
LaCasa_Collo_hourly <- timeAverage(LaCasa_Min, avg.time = "hour", data.thresh = 75, interval = "min")
#La_Casa_daily <- timeAverage(La_Casa_hourly, avg.time = "day", data.thresh = 75)

LaCasa_Collo_hourly$pm25_p <- signif(LaCasa_Collo_hourly$pm25_p, 4)
LaCasa_Collo_hourly$pm10 <- signif(LaCasa_Collo_hourly$pm10, 4)
LaCasa_Collo_hourly$temperature <- signif(LaCasa_Collo_hourly$temperature, 4)
LaCasa_Collo_hourly$humidity <- signif(LaCasa_Collo_hourly$humidity, 4)
LaCasa_Collo_hourly$pm25_r <- signif(LaCasa_Collo_hourly$pm25_r, 4)

# LaCasa_Collo_hourly$ID <- "LaCasa Collo 167"
# LaCasa_Collo_hourly$SensorType <- "LunarOutpost"

LaCasa_Collo_hourly <- subset(LaCasa_Collo_hourly, select = c("date", "pm25_p", "pm10", "temperature", "humidity", "pm25_r"))
LaCasa_Collo_info <- matrix(c("LaCasa Collo", "Lunar Outpost", LaCasa_Collo_167$Lat[2],LaCasa_Collo_167$Long[2] ), ncol = 1, byrow = TRUE)
rownames(LaCasa_Collo_info) <- c("ID", "Sensor Type", "Lat", "Long")

write.table(LaCasa_Collo_info,"LaCasa_Collo_hourly.csv", sep = ",", row.names = TRUE, col.names = FALSE)
write.table(LaCasa_Collo_hourly,"LaCasa_Collo_hourly.csv", append = TRUE, sep = ",", col.names = TRUE, row.names = FALSE)




#---------------------Reference Monitors------------------------#

#Variables we want:
#  Date, Time, Location (lat/long), PM2.5, PM10, Humidity, 
#   Temperature, Wind Direction, Wind Speed

#CAMP Reference Monitor Data
CAMP_170 <- read.csv("170_CAMP_20210101-20210131.csv", skip = 3, header = T)

#Check which is processed vs. unprocessed data
keeps <- c("Date", "Outdoor.Temperature", "Wind.Direction", "Wind.Speed")
CAMP_170 <- CAMP_170[keeps]
names(CAMP_170)[1] <- "DateTime"
#, "Outdoor.Temperature", "Wind.Direction", "Wind.Speed"

# missing<- which(is.na(CAMP_170$PM2_5) | is.na(CAMP_170$Hmdty | is.na(CAMP_170$Temp)))
# high<- which((CAMP_170$PM2_5 > 1000) | (CAMP_170$PM10 > 1000))
# neg<- which((CAMP_170$PM2_5 < 0) | (CAMP_170$Temp < 0) | (CAMP_170$Hmdty < 0) | (CAMP_170$PM10 < 0))
# 
# CAMP_170 <- CAMP_170[-c(high, missing, neg),] 

#Setting Up Date/Time Separately
CAMP_170$Date<- as.Date(sapply(CAMP_170[,1], function(x){strsplit(as.character(x), " ")[[1]][1]}), 
                                format = "%m/%d/%Y")
CAMP_170$Time<- chron(times = sapply(CAMP_170[,1], function(x){paste0(strsplit(as.character(x), " ")[[1]][2])}), 
                              format = "hh:mm:ss")

#Concatenating date and time back together and formatting the column to be a date/time
CAMP_170$date <- str_c(CAMP_170$Date," ", CAMP_170$Time)
CAMP_170$date <- as.POSIXct(CAMP_170$date)

#Averaging the data to 1 hour interval, threshold of 75%
CAMP_170_hourly <- timeAverage(CAMP_170, avg.time = "hour", data.thresh = 75)

#------------------------------------
#LaCasa Reference Monitor
#Variables we want:
#  Date, Time, Location (lat/long), PM2.5, PM10, Humidity, 
#   Temperature, Wind Direction, Wind Speed

LaCasa_70 <- read.csv("70_La-Casa_20210101-20210131.csv", skip = 3, header = T)
keeps <- c("Date","PM10", "PM2.5")
LaCasa_70 <- LaCasa_70[keeps]

names(LaCasa_70)[names(LaCasa_70) == "PM10"] <- "pm10"
names(LaCasa_70)[names(LaCasa_70) == "PM2.5"] <- "pm25_p"
names(LaCasa_70)[names(LaCasa_70) == "Date"] <- "date"

#Removing Negative Values and replacing with NA
LaCasa_70$pm10 <- replace(LaCasa_70$pm10, LaCasa_70$pm10 < 0, NA)
LaCasa_70$pm25_p <- replace(LaCasa_70$pm25_p, LaCasa_70$pm25_p < 0, NA)


#Removing High Values and replacing with NA
LaCasa_70$pm10 <- replace(LaCasa_70$pm10, LaCasa_70$pm10 > 500, NA)
LaCasa_70$pm25_p <- replace(LaCasa_70pm25_p,LaCasa_70$pm25_p >500, NA)


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

LaCasa_Airnow_hourly <- LaCasa_Airnow_hourly[-c(4:5)]

#Round the values so there are not so many sigfigs -- can modify to match the uncertainty of the instrument
LaCasa_Airnow_hourly$pm25_p <- signif(LaCasa_Airnow_hourly$pm25_p, 3)
LaCasa_Airnow_hourly$pm10 <- signif(LaCasa_Airnow_hourly$pm10, 3)

LaCasa_Airnow_hourly <- subset(LaCasa_Airnow_hourly, select = c("date", "pm25_p", "pm10"))
LaCasa_Airnow_info <- matrix(c("LaCasa sftp", "Airnow", LaCasa_Collo_167$Lat[2], LaCasa_Collo_167$Long[2]), ncol = 1, byrow = TRUE)
rownames(LaCasa_Airnow_info) <- c("ID", "Sensor Type", "Lat", "Long")

#Write the matrix and the data frame to a single csv file
write.table(LaCasa_Airnow_info,"LaCasa_Airnow_hourly.csv", sep = ",", row.names = TRUE, col.names = FALSE)
write.table(LaCasa_Airnow_hourly,"LaCasa_Airnow_hourly.csv", append = TRUE, sep = ",", col.names = TRUE, row.names = FALSE)


#---------------------------------------------------------------------------------------
#------------------------------Clarity Sensor Example -----------------------------------

CollegeView <- read.csv("2512_College-View-Elem-(A2JCKC55_2.3)_20210101-20210131.csv", skip = 3, header = T)
keeps <- c("Date","pm10ConcMass", "pm2_5ConcMass", "relHumid","temperature")
CollegeView <- CollegeView[keeps]
names(CollegeView)[1] <- "date"
names(CollegeView)[2] <- "pm10"
names(CollegeView)[3] <- "pm25_p"
names(CollegeView)[4] <- "humidity"

CollegeView$temperature <- CollegeView$temperature*9/5 + 32

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

#Cleaning the data to make sure we are not showing values that do not make sense
#Removing Negative Values and replacing with NA
CollegeView$temperature <- replace(CollegeView$temperature, CollegeView$temperature < 0, NA)
CollegeView$pm10 <- replace(CollegeView$pm10, CollegeView$pm10 < 0, NA)
CollegeView$pm25_p <- replace(CollegeView$pm25_p, CollegeView$pm25_p < 0, NA)
CollegeView$humidity <- replace(CollegeView$humidity, CollegeView$humidity < 0, NA)

#Removing High Values and replacing with NA
CollegeView$pm10 <- replace(CollegeView$pm10, CollegeView$pm10 > 500, NA)
CollegeView$pm25_p <- replace(CollegeView$pm25_p, CollegeView$pm25_p > 500, NA)
CollegeView$humidity <- replace(CollegeView$humidity, CollegeView$humidity > 100, NA)

#-------------------------------

#Averaging the data to 1 hour interval, threshold of 75%
CollegeView_15min <- timeAverage(CollegeView, avg.time = "15 min")
CollegeView_hourly <- timeAverage(CollegeView_15min, avg.time = "hour", data.thresh = 75, interval = "15 min")

#Rounding hourly data (shrinks file size)
#Round the values so there are not so many sigfigs -- can modify to match the uncertainty of the instrument
CollegeView_hourly$pm25_p <- signif(CollegeView_hourly$pm25_p, 3)
CollegeView_hourly$pm10 <- signif(CollegeView_hourly$pm10, 3)
CollegeView_hourly$temperature <- signif(CollegeView_hourly$temperature, 4)
CollegeView_hourly$humidity <- signif(CollegeView_hourly$humidity, 3)

#-------------------------------
##This part would create columns of values for the sensor ID, sensor type, and lat/long

CollegeView_hourly$Lat <- 39.668157000
CollegeView_hourly$Long <- -105.023141000

#-------------------------------
#Create matrix of ID name, sensor type, and lat/long -- this is to go at the top of the csv file
CollegeView_hourly <- subset(CollegeView_hourly, select = c("date", "pm25_p", "pm10", "temperature", "humidity", "Lat", "Long"))
# CollegeView_info <- matrix(c("College View", "Clarity", 39.668157000, -105.023141000), ncol = 1, byrow = TRUE)
# rownames(CollegeView_info) <- c("ID", "Sensor Type", "Lat", "Long")

CollegeView_hourly <- CollegeView_hourly %>% gather(parameter, value, -c(date))
CollegeView_hourly$ID <- "College View"
CollegeView_hourly$SensorType <- "Clarity"
CollegeView_hourly <- subset(CollegeView_hourly, select = c("date","ID", "SensorType", "parameter", "value"))


#Add units of each of the parameters
CollegeView_hourly <- mutate(CollegeView_hourly, parameter_units = 
                               ifelse(parameter == "pm25_p", "ug/m3",
                                      ifelse(parameter == "pm10", "ug/m3",
                                             ifelse(parameter == "temperature", "F", "NA"))))

CollegeView_daily <- timeAverage(CollegeView_hourly, avg.time = "day", data.thresh = 75, interval = "1 hour")

#Write the matrix and the data frame to a single csv file
# write.table(CollegeView_info,"CollegeView_hourly.csv", sep = ",", row.names = TRUE, col.names = FALSE)
# write.table(CollegeView_hourly,"CollegeView_hourly.csv", append = TRUE, sep = ",", col.names = TRUE, row.names = FALSE)



#Plotting



