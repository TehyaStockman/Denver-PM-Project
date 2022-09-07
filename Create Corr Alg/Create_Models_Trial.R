#File to Create Different Machine Learning Algorithms with the Collocated Sensor Data
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




#First: Try doing one pair: sensor/ref monitor with the various algorithms
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

install.packages('caret')
install.packages('mlbench')
install.packages('Metrics')

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

file_dir <- 'Processed_Data/Data_By_Sensor_Wide'


LaCasa_sensor <- read.csv(paste(file_dir, 'wide_La Casa Collo (CS5).csv', sep = '/'))
LaCasa_ref <- read.csv(paste(file_dir, 'wide_La Casa airnow.csv', sep = '/'))

#Make sure the dates are in the correct format
LaCasa_all <- merge(LaCasa_sensor, LaCasa_ref, by = 'date', all.x = TRUE)
column_list <- c('date', 'age_days.x', 'age_weeks.x', 'val.humidity', 
                 'val.temperature', 'val.pm25_r', 'val.pm25_p.y')

#Select subset of data that is needed for this analysis
LaCasa_all <- subset(LaCasa_all, select = column_list)

#Remove NA values from the data set
LaCasa_all <- drop_na(LaCasa_all)

#Use caret package to create multilinear regression with appropriate variables
#Outcome ~ Predictors
#processed_data = c1*raw_data + c2*temp + c3*rh + d

#Create partition of data to make the same for each file

linear_fit <- lm(val.pm25_p.y ~ val.pm25_r + val.temperature + val.humidity, LaCasa_all)
linear_fit2 <- lm(val.pm25_p.y ~ val.pm25_r + val.temperature + val.humidity + age_weeks.x, LaCasa_all)

summary(linear_fit)
summary(linear_fit2)

temp_data <-subset(LaCasa_all, select=c(val.pm25_r, val.pm25_p.y))
fit1 <-train(val.pm25_p.y ~., data=temp_data, method="lm",
            trControl=trainControl(method="cv", verboseIter =T),
            na.action = na.pass)

coef(fit1$finalModel)
LaCasa_all$pm25_fit1<-predict(fit1, LaCasa_all)
cor(LaCasa_all$val.pm25_p.y, LaCasa_all$pm25_fit1)
rmse(LaCasa_all$val.pm25_p.y, LaCasa_all$pm25_fit1)
mae(LaCasa_all$val.pm25_p.y, LaCasa_all$pm25_fit1)
bias(LaCasa_all$val.pm25_p.y, LaCasa_all$pm25_fit1)

data_cs_hr$pm_fit1<-predict(fit1, data_cs_hr)


#Random Forest Tree
require(caret)
require(hydroGOF)
require(Metrics)
library(ranger)

#Make sure that the "randomness" is the same each time, set the same seed for each loop to be same "randomness" and have
#the same results each time
set.seed(1234)


install.packages('ranger')

temp_data <-subset(LaCasa_all, select=c(val.pm25_r, val.pm25_p.y, val.temperature, val.humidity, age_weeks.x))
fit17<-train(val.pm25_p.y ~., data=temp_data, method="ranger",
             trControl=trainControl(method="cv", verboseIter =F),
             num.trees=100,
             importance="permutation", na.action = na.pass)
coef(fit17$finalModel)
LaCasa_all$pm25_fit17<-predict(fit17, LaCasa_all)

summary(fit17)
varImp(fit17)







#Setting a seed: pseudo-random number generator so that when you run your code you get the same values each time to 
#have consistency between runs


  