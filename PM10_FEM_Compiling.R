#Compiling PM10 data
library(caret)
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(chron)
library(lubridate)
library(shipunov)
library(ggpubr)
library(naniar)
library(mlbench)
library(Metrics)
library(rlist)
library(foreach)
library(doParallel)

library(openair)

#read in data files
wd <- getwd()
setwd(wd)

years <- c('2018', '2019', '2020', '2021', '2022')
data_dir <- paste(wd, 'PM10_Data', sep = '/')
AQ_files <- list.files(data_dir)
all_columns <- c()

AQ_data <- data.frame(matrix(ncol = 8, nrow = 0))

for(aq_file in AQ_files){
  temp_data <- read.csv(paste(data_dir, aq_file, sep = '/'), header = TRUE)
  
  temp_cols <- colnames(temp_data)
  all_columns <- append(all_columns, temp_cols)
  #get rid of columns that are not needed
  processed_data <- subset(temp_data, select = c("X.Hour..MST.", "CAMP", "CASA", "I25DEN", "I25GLO", "X..SWAN", "Date", "NJH"))
  #Compile PM10 data files together
  
  names(processed_data)[names(processed_data) == "X.Hour..MST."] <- "Time"
  processed_data$Date <- as.Date(processed_data$Date)
  
  processed_data$Time <- format(strptime(processed_data$Time, "%I:%M %p"), tz = "MDT", format="%H:%M:%S")
  
  
  format <- "%Y-%m-%d %H:%M"
  
  processed_data$Date_Time <- as.POSIXct(paste(processed_data$Date, processed_data$Time), format=format)
  processed_data <- subset(processed_data, select = c("Date_Time", "CAMP", "CASA", "I25DEN", "I25GLO", "X..SWAN", "NJH"))
  names(processed_data)[names(processed_data) == "Date_Time"] <- "date"
  names(processed_data)[names(processed_data) == "X..SWAN"] <- "SWAN"
  
  AQ_data$I25DEN <- as.numeric(AQ_data$I25DEN)
  processed_data$date <- with_tz(processed_data$date, "MST")
  #processed_data$date <- processed_data$date - 2*60*60
  
  AQ_data <- rbind(AQ_data, processed_data)
  
  
}

AQ_data <- drop_na(AQ_data$date)

summaryPlot(AQ_data)

timePlot(AQ_data, pollutant = c('CAMP', 'CASA', 'NJH', "I25DEN", "I25GLO","SWAN"), y.relation = "same")
timePlot(AQ_data, pollutant = c("I25DEN"), y.relation = "free")
#Put the PM10 data into the correct processed data files
##Separate by data site and combine into current site data

#Calculate PM2.5/PM10 ratios

I25GLO_PM10 <- subset(AQ_data, select = c("date", "I25GLO"))
names(I25GLO_PM10)[names(I25GLO_PM10) == "I25GLO"] <- "PM10"
I25GLO_data <- read.csv(paste(alg_data_dir, "I-25 Globeville State Site.csv", sep = '/'))
I25GLO_data$date <- as.POSIXct(I25GLO_data$date, format = "%Y-%m-%d %H:%M")
I25GLO_data_all <- merge(I25GLO_data, I25GLO_PM10)
I25GLO_data_all$Ratio <- I25GLO_data_all$val.pm25_p.y/I25GLO_data_all$PM10
#I25GLO_data_all <- I25GLO_data_all[!rowSums(I25GLO_data_all$Ratio<1.0),]
#nrow(I25GLO_data_all[I25GLO_data_all$Ratio>1.0, ])
I25GLO_data_all <- subset(I25GLO_data_all, Ratio<= 1.0) 
write.csv(I25GLO_data_all, paste(new_alg_data_dir, 'I-25 Globeville State Site.csv', sep = '/'))


I25GLO_data_all <- subset(I25GLO_data_all, is.na(date) == F) 


timePlot(I25GLO_data_all, pollutant = c("Ratio"))

I25DEN_PM10 <- subset(AQ_data, select = c("date", "I25DEN"))
names(I25DEN_PM10)[names(I25DEN_PM10) == "I25DEN"] <- "PM10"
I25DEN_data <- read.csv(paste(alg_data_dir, "I-25 Denver State Site.csv", sep = '/'))
I25DEN_data$date <- as.POSIXct(I25DEN_data$date, format = "%Y-%m-%d %H:%M")
I25DEN_data_all <- merge(I25DEN_data, I25DEN_PM10)
I25DEN_data_all$Ratio <- I25DEN_data_all$val.pm25_p.y/I25DEN_data_all$PM10
I25DEN_data_all <- subset(I25DEN_data_all, Ratio<= 1.0) 
I25DEN_data_all <- subset(I25DEN_data_all, is.na(date) == F) 
write.csv(I25DEN_data_all, paste(new_alg_data_dir, "I-25 Denver State Site.csv", sep = '/'))


CAMP_PM10 <- subset(AQ_data, select = c("date", "CAMP"))
names(CAMP_PM10)[names(CAMP_PM10) == "CAMP"] <- "PM10"
CAMP_data <- read.csv(paste(alg_data_dir, "CAMP State Site.csv", sep = '/'))
CAMP_data$date <- as.POSIXct(CAMP_data$date, format = "%Y-%m-%d %H:%M")
CAMP_data_all <- merge(CAMP_data, CAMP_PM10, by = 'date')
CAMP_data_all$Ratio <- CAMP_data_all$val.pm25_p.y/CAMP_data_all$PM10
CAMP_data_all <- subset(CAMP_data_all, Ratio<= 1.0) 
CAMP_data_all <- subset(CAMP_data_all, is.na(date) == F) 
write.csv(CAMP_data_all, paste(new_alg_data_dir, "CAMP State Site.csv", sep = '/'))


SWAN_PM10 <- subset(AQ_data, select = c("date", "SWAN"))
names(SWAN_PM10)[names(SWAN_PM10) == "SWAN"] <- "PM10"
SWAN_data <- read.csv(paste(alg_data_dir, "Swansea GRIMM.csv", sep = '/'))
SWAN_data$date <- as.POSIXct(SWAN_data$date, format = "%Y-%m-%d %H:%M")
SWAN_data_all <- merge(SWAN_data, SWAN_PM10, by = 'date')
SWAN_data_all$Ratio <- SWAN_data_all$val.pm25_p.y/SWAN_data_all$PM10
SWAN_data_all <- subset(SWAN_data_all, Ratio<= 1.0) 
SWAN_data_all <- subset(SWAN_data_all, is.na(date) == F) 
write.csv(SWAN_data_all, paste(new_alg_data_dir, "Swansea GRIMM.csv", sep = '/'))


CASA_PM10 <-subset(AQ_data, select = c("date", "CASA"))
names(CASA_PM10)[names(CASA_PM10) == "CASA"] <- "PM10"
CASA_data <- read.csv(paste(alg_data_dir, "La Casa State Site.csv", sep = '/'))
CASA_data$date <- as.POSIXct(CASA_data$date, format = "%Y-%m-%d %H:%M")
CASA_data_all <- merge(CASA_data, CASA_PM10, by = 'date')
CASA_data_all$Ratio <- CASA_data_all$val.pm25_p.y/CASA_data_all$PM10
CASA_data_all <- subset(CASA_data_all, Ratio<= 1.0) 
CASA_data_all <- subset(CASA_data_all, is.na(date) == F) 
write.csv(CASA_data_all, paste(new_alg_data_dir, "La Casa State Site.csv", sep = '/'))


NJH_PM10 <- subset(AQ_data, select = c("date", "NJH"))
names(NJH_PM10)[names(NJH_PM10) == "NJH"] <- "PM10"
NJH_data <- read.csv(paste(alg_data_dir, "National Jewish Health State Site.csv", sep = '/'))
NJH_data$date <- as.POSIXct(NJH_data$date, format = "%Y-%m-%d %H:%M")
NJH_data_all <- merge(NJH_data, NJH_PM10, by = 'date')
NJH_data_all$Ratio <- NJH_data_all$val.pm25_p.y/NJH_data_all$PM10
NJH_data_all <- subset(NJH_data_all, Ratio<= 1.0) 
NJH_data_all <- subset(NJH_data_all, is.na(date) == F) 
write.csv(NJH_data_all, paste(new_alg_data_dir, "National Jewish Health State Site.csv", sep = '/'))




#Make new correction algs for PM10 incorporation

