#PM2.5 Cleaning Data -- overarching script for processing
#Created by Tehya Stockman
#Last Updated: March 22, 2022

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
install.packages('foreach')
install.packages('doParallel')

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
library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)



#SET THE WORKING DIRECTORY HERE
#wd <- getwd()
#setwd(wd)

#if getwd() does not work, you can instead un-comment the following line with the correct directory
#of your computer
#setwd("/Users/tehyastockman/DDPHE/PM_Data_Cleaning/")

#Make sure these files are included in the same folder as this current script
source(paste(proc_files_dir, "lunar_processing.R", sep = '/'))
source(paste(proc_files_dir,"clarity_processing.R", sep = '/'))
source(paste(proc_files_dir,"airnow_processing.R", sep = '/'))
source(paste(proc_files_dir,"met_processing2.R", sep = '/'))
site_metadata <- read.csv("Sensor_MetaData_Compiled.csv")

#CHANGE THE FOLDER HERE, this folder should be same folder as this current script
#Folders are organized for this script by month and year, so you should modify the month and 
#year to change the folder of the data that you want
folder_year <- "2022"
folder_month <- "202206"
# This is to paste the folder_year and folder_month together to make the appropriate file path
file_folder <- paste(folder_year, folder_month, sep = "/")
data_dir <- paste(aq_raw_dir, file_folder, sep = "/")

#For the output data, I created a folder structure within /Processed_Data of the year for each 
#year of data, each month data is saved to the corresponding year
#data_dir <- "troubleshoot"
output_dir <- paste(aq_data_dir, folder_year, sep = "/")

AQ_files <- list.files(data_dir)


#P25, PM25_p, pm25_p are processed data
#PM2_5 and pm25_r are raw data

#____________________________ Looping Through Data _____________________________________
#Creating a dictionary of file names to look up that link to data tables
data <- list()
AQ_data <- data.frame(matrix(ncol = 9, nrow = 0))


air_data <- foreach(filename = AQ_files, .combine = rbind) %dopar%{
  #shortname <- str_match(filename, "[0-9]*_(.*)_[0-9]*-[0-9]*\\.csv")
  #shortname <- shortname[2]
  #shortname <- substring(readLines(file(paste(data_dir,filename, sep = "/")), 1), 17)
  file_path <- paste(data_dir,filename, sep = "/")
  file_ptr <- file(file_path)
  shortname <- trimws(strsplit(readLines(file_ptr, 1), ",")[[1]][2])
  close(file_ptr)
  #print(shortname)
  temp_data <- read.csv(paste(data_dir,filename, sep = "/"),skip = 3, header = T)
  
  if("PM2_5" %in% colnames(temp_data)) {
    processed_data <- lunar_processing(temp_data) }
  else if("pm25_r" %in% colnames(temp_data)) {
    processed_data <- lunar_processing(temp_data) }
  else if("pm2_5ConcMass" %in% colnames(temp_data)) {
    processed_data <- clarity_processing(temp_data)}
  else if("PM2.5" %in% colnames(temp_data)) {
    #shortname <- paste(shortname,"airnow")
    processed_data <- airnow_processing(temp_data)}
  else if("Wind.Direction" %in% colnames(temp_data)){
    shortname <- paste(shortname, "MET")
    processed_data <- met_processing2(temp_data)}
  # else if("PM2.5.Total.Atmospheric" %in% colnames(temp_data)){
  #   shortname <- paste(shortname, "MET")
  #   processed_data <- met_processing2(temp_data)}
  else{
    print(shortname)
    return()
  }
  
  processed_data <- merge(processed_data, site_metadata, by = "ID", all.x = TRUE) 
  processed_data <- subset(processed_data, select = c("date","ID", "Site", "SensorType", "Lat", "Long", "parameter", "val", "flag", "parameter_units"))
  processed_data$ID[processed_data$SensorType == "Airnow"] <- paste(shortname,"airnow")
  
  
  #Add Data to list
  data[shortname] <- list(processed_data)
  processed_data
}

#CHANGE THE FILE NAME HERE
# write.csv(AQ_data,"May2020.csv", row.names = FALSE)
# setwd(paste("/Users/tehyastockman/DDPHE/PM_Data_Cleaning", output_dir, sep = "/"))

new_file_path <- paste(output_dir, folder_month, sep = "/")
write.csv(air_data, paste(new_file_path,"csv", sep = "."), row.names = FALSE)

#This is to double check the monthly data to make sure everything looks good
# AQ_monthly_data <- AQ_data
# df <- AQ_monthly_data %>% count(ID,parameter, flag)
# df <- merge(df, site_metadata, by = "ID", all = TRUE) 
# 
# max_min <- AQ_monthly_data[AQ_monthly_data$parameter == "pm25_p", ] %>%
#   group_by(ID) %>%
#   summarize(pm_max.pt = max(val, na.rm = T), pm_min.pt = min(val, na.rm = T), pm_avg.pt = mean(val, na.rm = T))
# 


