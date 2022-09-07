#Processing the data so that each sensor has its own file for entire length of time
#I am doing this so that each sensor can be more easily compared over the entire time period
#Taking in monthly files --> create yearly files so they are more beneficial
#Create long format file for each sensor/monitor
#Create wide format file for each sensor/monitor --> this is to use with OpenAir

library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(chron)
library(lubridate)


#SET THE WORKING DIRECTORY HERE
wd <- getwd()
setwd(wd)

#The Following Code is to make a MEGA File of All of the Data that can then be filtered
folder_list <- c('2018', '2019', '2020', '2021', '2022')
AQ_data <- data.frame(matrix(ncol = 9, nrow = 0))
output_dir <- 'Processed_Data/All_Data'

for (folder_year in folder_list){
  data_dir <- paste('Processed_Data', folder_year, sep = '/')
  AQ_files <- list.files(data_dir)
  print(AQ_files)
  year_data <- data.frame(matrix(ncol = 9, nrow = 0))
  for(filename in AQ_files){
    file_path <- paste(data_dir, filename, sep = '/')
    temp_data <- read.csv(file_path)
    year_data <- rbind(year_data, temp_data)
  }
  write.csv(year_data, paste(output_dir, folder_year, '.csv', sep = ''))
  AQ_data <- rbind(AQ_data, year_data)
}

#Writing to a Mega-File Containing All Processed Data
write.csv(AQ_data, paste(output_dir, 'All_PM_Data.csv', sep = '/'))


#The Following Code is to Make Sensor-Specific Files
AQ_data$ID <- str_replace_all(AQ_data$ID, '/', ' ')

unique_sites <- unique(AQ_data$ID)

sensor_dir <- "Processed_Data/Data_By_Sensor"

for (site in unique_sites){
  site_data <- filter(AQ_data, ID == site)
  file_path <- paste(sensor_dir, site, sep = '/')
  write.csv(site_data, paste(file_path, 'csv', sep = '.'))
  
}



