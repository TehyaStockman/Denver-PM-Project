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


#read in data files
wd <- getwd()
setwd(wd)

years <- c('2018', '2019', '2020', '2021', '2022')
data_dir <- paste(wd, 'PM10_Data', sep = '/')
AQ_files <- list.files(data_dir)
all_columns <- c()

for(aq_file in AQ_files){
  temp_data <- read.csv(paste(data_dir, aq_file, sep = '/'), header = TRUE)
  
  temp_cols <- colnames(temp_data)
  all_columns <- append(all_columns, temp_cols)
  #get rid of columns that are not needed
  processed_data <- subset(processed_data, select = c("X.Hour..MST.", "CAMP", "CASA", "I25DEN", "I25GLO", "X..SWAN", "Date", "NJH"))
  #Compile PM10 data files together
  
}



#Put the Pm10 data into the correct processed data files
##Separate by data site and combine into current site data

#Calculate PM2.5/PM10 ratios

#Make new correction algs for PM10 incorporation

