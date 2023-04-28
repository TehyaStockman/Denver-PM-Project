#Create Correction Algorithms Using Reference Monitors
install.packages('rlist')
install.packages('tools')

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
library(tools)


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
  
  
  #merge smokey data with ref data
  all_data2 <- merge(x = all_data, y = smoke_events, by = 'date', all = TRUE)
  
  all_data2$flag2[all_data2$flag == 'smoke'] <- 1
  all_data2$flag2[all_data2$flag == 'smoke?'] <- 1
  all_data2$flag2[is.na(all_data2$flag)] <-0
  
  names(all_data2)[names(all_data2) == "flag"] <- "flag3"
  names(all_data2)[names(all_data2) == "flag2"] <- "flag"
  
  #make sure columns are all correctly labeled
  if("val.temperature.x" %in% colnames(all_data2))
  {names(all_data2)[names(all_data2) == "val.temperature.x"] <- "val.temperature"}
  
  #Select subset of data that is needed for this analysis
  all_data2 <- subset(all_data2, select = column_list)

  #Remove NA values from the data set
  all_data2 <- drop_na(all_data2)
  
  all_data2 <- distinct(all_data2, .keep_all = TRUE)
  
  #write data to file
  write.csv(all_data2, paste(alg_data_dir, '/', sitename, '.csv', sep = ''))
  
  i = i +1
}

########################################################
##################!!!!!!!!!!!!!!!!!####################
#Each algorithm created will utilize different variable combinations
#dictionary of variables and what to name each combination

#Initialize Environment before running the rest

var_dict <- list('all' = c('val.pm25_r', 'val.humidity', 'val.temperature', 'age_weeks.x', 'flag'),
    'age' = c('val.pm25_r', 'val.humidity', 'val.temperature', 'age_weeks.x'),
    'smoke' = c('val.pm25_r', 'val.humidity', 'val.temperature', 'flag'),
    'met' = c('val.pm25_r', 'val.humidity', 'val.temperature'),
    'pm_raw' = c('val.pm25_r'))

var_list <- list('all', 'age', 'smoke', 'met', 'pm_raw')

ref_pm <- 'val.pm25_p.y'
alg_list <- c('lm', 'rft', 'knn')

#Create For loop to make merged data files and save them
#Make sure the dates are in the correct format

AQ_files <- list.files(alg_data_dir)

aq_models <- list()


for(file in AQ_files){
  #temp_data <- list()
  
  
  for (alg in alg_list){
    
    for(var in seq(1,length(var_dict))){
      aq_data_file <- file
      model_name <- gsub('.csv', '', file) #remove ".csv" from name
      
      model_name <- paste(model_name, alg, names(var_dict[var]))
      
      temp_data<- list(model_name, aq_data_file, alg, var_dict[[var]])
      
      aq_models <- list.append(aq_models, temp_data)
      
    }
  }
}

source(paste(create_corr_alg_dir, 'ml_models.R', sep = '/'))



#try with less data
aq_models_short <- aq_models[1:90] #21
model_names <- c()

all_model_data <- foreach(model_index = aq_models_short) %dopar%{
  #model_index <- aq_models_short[i]
  #source('initialize_environment.R')
  #source(paste(create_corr_alg_dir, 'ml_models.R', sep = '/'))
  
  #initialize_env()
  #read in csv file
  name_model <- model_index[[1]]
  data_for_model <- read.csv(paste(alg_data_dir, model_index[[2]], sep = '/'))
  data_for_model <- data_for_model[-1]
  data_for_model <- data_for_model[-1]
  data_for_model <- data_for_model[-1]
  data_for_model <- data_for_model[-1]
  
  filename <- model_index[[2]]
  model_type <- model_index[[3]]
  model_variables <- model_index[[4]]
  
  
  model <- create_model(name_model,
                     data_for_model, model_type, model_variables)
  
  
  model_filename <- paste(name_model, '.rds', sep = '')
  model_file <- paste(corr_alg_dir, model_filename, sep = '/')
  
  saveRDS(model, model_file)
  model_variables
}


#Create evaluation files and loop for script to evaluate files

corr_alg_files <- list.files(corr_alg_dir)
AQ_files <- list.files(alg_data_dir)

aq_corr_alg_models <- list()

for(alg_file in corr_alg_files){
  
  for(file in AQ_files){
    
    #do not include any file from the list that match
    if(grepl(file_path_sans_ext(file), alg_file, fixed = TRUE))
    { next
      } else{
        temp_list<- list(file, alg_file)}
    
    aq_corr_alg_models <- list.append(aq_corr_alg_models, temp_list)
    
  }
  
}




#Call the evaluation function to create
alg_stats_of_site <- data.frame(matrix(ncol = 5, nrow = 0))
source(paste(create_corr_alg_dir, 'ml_models.R', sep = '/'))


all_corr_alg_models <- foreach(alg_model_index = aq_corr_alg_models, .combine='rbind') %dopar% {
  library(tools)
  site_data_name <- alg_model_index[[1]]
  corr_alg_name <- alg_model_index[[2]]
  
  site_data <- read.csv(paste(alg_data_dir, site_data_name, sep = '/'))
  site_data <- site_data[-1]
  
  corr_alg <- readRDS(paste(corr_alg_dir, corr_alg_name, sep = '/'))
  
  alg_site_name <- paste(file_path_sans_ext(site_data_name), 
                         file_path_sans_ext(corr_alg_name))

  
  
  alg_stats_of_site <- evaluate_model(alg_site_name,
                                      site_data, corr_alg)
  
  
  alg_stats_of_site
}

#test for loop
for(alg_model_index in aq_corr_alg_models){

  site_data_name <- alg_model_index[[1]]
  corr_alg_name <- alg_model_index[[2]]
  
  site_data <- read.csv(paste(alg_data_dir, site_data_name, sep = '/'))
  site_data <- site_data[-1]
  
  corr_alg <- readRDS(paste(corr_alg_dir, corr_alg_name, sep = '/'))
  
  alg_site_name <- paste(file_path_sans_ext(site_data_name), 
                         file_path_sans_ext(corr_alg_name))
  
  
  
  alg_stats <- evaluate_model(alg_site_name,
                                      site_data, corr_alg)
  
  
  alg_stats_of_site <- rbind(alg_stats_of_site, alg_stats)
}

write.csv(alg_stats_of_site, 'algorithm_stats_updated.csv')

#Test Evaluation of Model
'wide_SAMS MET.csv'
'wide_Swansea Elementary (CS7).csv'

data_dir <- paste(aq_data_dir, 'Data_By_Sensor_Wide', sep = '/')

SAMS_data <- read.csv(paste(data_dir, 'wide_SAMS MET.csv', sep = '/'))
SAMS_data <- SAMS_data[-1]

swansea_data <- read.csv(paste(data_dir, 'wide_Swansea Elementary (CS7).csv', sep = '/'))


column_list <- c('date', 'age_days.x', 'age_weeks.x',  
                  'val.pm25_p.x', 'val.pm25_p.y')

#merge ref data and sensor data
all_data <- merge(swansea_data, SAMS_data, by = 'date', all.x = TRUE)


#make sure columns are all correctly labeled
if("val.temperature.x" %in% colnames(all_data))
{names(all_data)[names(all_data) == "val.temperature.x"] <- "val.temperature"}

#Select subset of data that is needed for this analysis
all_data <- subset(all_data, select = column_list)

#Remove NA values from the data set
all_data2 <- drop_na(all_data)

all_data2 <- distinct(all_data2, .keep_all = TRUE)

#write data to file
write.csv(all_data2, paste(alg_data_dir, '/', 'all SAMS', '.csv', sep = ''))





####Stuff
data_folder <- alg_data_dir

site_data <- read.csv(paste(data_folder, 'all SAMS.csv', sep = '/'))
site_data <- site_data[-1]

corr_alg <- readRDS(paste(corr_alg_dir, 'La Casa State Site rft met.rds', sep = '/'))

coef(corr_alg$finalModel)

site_data$pm25_fit1<-predict(corr_alg, site_data)

cor(site_data$val.pm25_p.y, site_data$val.pm25_p.x)
rmse(site_data$val.pm25_p.y, site_data$val.pm25_p.x)
mae(site_data$val.pm25_p.y, site_data$val.pm25_p.x)
bias(site_data$val.pm25_p.y, site_data$val.pm25_p.x)

#each of these are 1 number
cor(site_data$val.pm25_p.y, site_data$pm25_fit1)
rmse(site_data$val.pm25_p.y, site_data$pm25_fit1)
mae(site_data$val.pm25_p.y, site_data$pm25_fit1)
bias(site_data$val.pm25_p.y, site_data$pm25_fit1)

library(openair)
library(ggplot2)
scatterPlot(site_data$val.pm25_p.y, site_data$pm25_fit1)

ggplot(site_data, aes(x=val.pm25_p.y, y=val.pm25_p.x)) + geom_point() +  geom_smooth(method=lm)+
 stat_regline_equation()


# #Test the code
# model_test <- aq_models_short[4]
# 
# test_name <- model_test[[1]][[1]]
# test_data <- read.csv(paste(alg_data_dir, model_test[[1]][[2]], sep = '/'))
# test_data <- test_data[-1]
# 
# test_model_type <- model_test[[1]][[3]]
# model_variables <- model_test[[1]][[4]]
# 
# set.seed(1234)
# temp_data <-subset(test_data, select= c(val.humidity, val.pm25_r, val.pm25_p.y))
# fit_ln <-train(val.pm25_p.y ~., data=temp_data, method="lm",
#                trControl=trainControl(method="cv", verboseIter =T),
#                na.action = na.pass)
# 
# library(ranger)
# set.seed(1234)
# read.csv(filename) ## Make complete
# ##Create a function that takes in the variables and returns the model
# temp_data <-site_data[c(model_variables, 'val.pm25_p.y')]
# fit_rft <- train(val.pm25_p.y ~., data=temp_data, method="ranger",
#                  trControl=trainControl(method="cv", verboseIter =F),
#                  num.trees=100,
#                  importance="permutation", na.action = na.pass)
# 
# 
# 
# model <- create_model(test_name,
#                       test_data, test_model_type, model_variables)
# 
# 
# model_filename <- paste(test_name, '.rds', sep = '')
# model_file <- paste(corr_alg_dir, model_filename, sep = '/')
# saveRDS(model, model_file)




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




