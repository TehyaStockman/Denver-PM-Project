#Create Correction Algorithms Using Reference Monitors
install.packages('rlist')

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

#Algorithms include:
#Random Forest Tree
#Multi-linear Regression
#Raw Data
#K-Nearest Neighbor
#Processed Data from Current Algorithm (pm25_p)


#Upload the sensor files
#Upload the reference files
#Merge the sensor (pm2.5_r, T, Rh, age) and corresponding Reference monitor together
#Merge based on site name or lat/long
#Use carat package to implement various models
#Add different variables to each model
#Create loop that goes through each sensor pair
#Create loop to add various variables to each model
#Create loop to run through each of the variable combinations for each model
#Create file for each corrected data type
#Perform analysis method for each model variation
#Loop to apply the model to the other sensor pairs




#Load sensor and reference file1
#Merge files together --> by date, make sure that PM25_r is all kept
#Remove columns that are not needed for the analysis
#Keep: temp, Rh, pm25_r, pm25_p, sensor age, ref monitor pm25_p
#Create list of names for each model -- maybe loop through, need consistency for reporting later
#Save file of raw data -- matched up -- matched up with ref monitor
#Save file of previously processed data -- matched up with ref monitor
#Create RFT model with: T, Rh, pm25_r -- save file
#Create RFT model with: T, Rh, pm25_r, sensor age -- save file
#Create KNN model with: T, pm25_p, Rh -- save file
#CoD analysis between Ref Monitor and Sensor for each model
#Pearson Correlation Coefficient between Sensor and Ref Monitor for each model
#RMSE between each Sensor model and the Ref Monitor data

##--------###
file_dir <- paste(aq_data_dir, "Data_By_Sensor_Wide", sep = '/')

#Reference monitors and sensors to be looped through
site_name_list <- c('CAMP State Site', 'I-25 Denver State Site', 
                    'I-25 Globeville State Site', 'La Casa State Site',
                    'National Jewish Health State Site', 'Swansea GRIMM')
#filter metadata by sensor type and then by site name, then add the files in the 
#folder to the list using these identifiers
#Use wide format

#airnow sites
ref_monitor_list <- c('wide_CAMP airnow.csv', 'wide_I-25 Denver airnow.csv',
                      'wide_I-25 Globeville airnow.csv', 'wide_La Casa airnow.csv',
                      'wide_NJH airnow.csv', 'wide_SAMS MET.csv') 

#canary sensors, can also do clarity in the future
sensor_list <- c('wide_CAMP COllo (CS13).csv', 'wide_I-25 Denver Collo (CS16).csv',
                 'wide_I-25 Glo Collo (CS2).csv', 'wide_La Casa Collo (CS5).csv',
                 'wide_NJH CS Collo (CS1).csv', 'wide_Swansea Elementary (CS7).csv') 

#Add smoke events -- 1,0 for smoke or no smoke
#Go into SAMS data and take out column for date and event, then merge to dataframe
#if "smoke": 1, "else": 0 <- figure out what should be included in if statement
smoke_events <- read.csv(paste(meta_data_dir, 'smokey_events.csv', sep = '/'))
smoke_events <- smoke_events[,-1]

column_list <- c('date', 'age_days.x', 'age_weeks.x', 'val.humidity', 
                 'val.temperature', 'val.pm25_r', 'val.pm25_p.y', 'flag')

i <- 1

for (monitor in ref_monitor_list){
  ref_data <- read.csv(paste(file_dir, monitor, sep = '/'))
  sensor_data <- read.csv(paste(file_dir, sensor_list[i], sep = '/'))
  sitename <- site_name_list[i]
  
  print(sitename)
  
  
  #merge ref data and sensor data
  all_data <- merge(sensor_data, ref_data, by = 'date', all.x = TRUE)
  
  #Remove NA values from the data set
  #all_data <- drop_na(all_data)
  
  #merge smokey data with ref data
  all_data2 <- merge(x = all_data, y = smoke_events, by = 'date', all = TRUE)
  
  #Select subset of data that is needed for this analysis
  all_data2 <- subset(all_data2, select = column_list)
  
  #write data to file
  write.csv(all_data2, paste(alg_data_dir, '/', sitename, '.csv', sep = ''))
  
  i = i +1
}

#Each algorithm created will utilize different variable combinations
#dictionary of variables and what to name each combination
var_dict <- list('all' = c('val.pm25_r', 'val.humidity', 'val.temperature', 'age_weeks.x', 'smokey'),
    'age' = c('val.pm25_r', 'val.humidity', 'val.temperature', 'age_weeks.x'),
    'smoke' = c('val.pm25_r', 'val.humidity', 'val.temperature', 'smokey'),
    'met' = c('val.pm25_r', 'val.humidity', 'val.temperature'),
    'raw' = c('val.pm25_r'))

var_list <- list('all', 'age', 'smoke', 'met', 'raw')

ref_pm <- 'val.pm25_p.y'
alg_list <- c('lm', 'rft', 'knn')

#Create For loop to make merged data files and save them
#Make sure the dates are in the correct format

AQ_files <- list.files(alg_data_dir)

aq_models <- c()


for(file in AQ_files){
  #temp_data <- list()
  
  
  for (alg in alg_list){
    
    for(var in seq(1,length(var_dict))){
      aq_data_file <- file
      model_name <- gsub('.csv', '', file) #remove ".csv" from name
      
      model_name <- paste(model_name, alg, names(var_dict[var]))
      
      temp_data<- list(model_name, aq_data_file, alg, var_dict[var])
      
      aq_models <- list.append(aq_models, temp_data)
      
    }
    
  }
  
}


foreach(i = length(aq_models)) %dopar%{
  model_index <- aq_models[i]
  
  #read in csv file
  name_model <- model_index[[1]][1]
  data_for_model <- read.csv(model_index[[1]][2])
  model_type <- model_index[[1]][3]
  model_variables <- model_index[[1]][[4]][1]
  
  
  model <- create_model(name_model, 
                      data_for_model, model_type, model_variables)
  
}


# c(filename, c(data, function, variables))

#([function, filename, variables])

#output: write files with the following name: sitename_merged_collo.csv

###---------------------------------------------------------###
##NEXT STEP ## Take merged data and create algorithms




#sitename_algtype#.csv


#https://cran.r-project.org/web/packages/foreach/vignettes/nested.html
#foreach-- looping through site pairs, algorithm types, variable types
#initialize same random starting number
#add to 

#make if statements to point to different functions
