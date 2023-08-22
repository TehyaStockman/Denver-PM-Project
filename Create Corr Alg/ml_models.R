#Algorithm Functions
#What do I want the fuctions to input and output?

#input a dataframe from each site
#input list of variables to perform operation

create_model <- function(name_model, data_for_model, model_type, model_variables){
  #bring in data/models to pass through
  #choose model function redirects to one of the 3 models depending on the inputs
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
  
  if(model_type == 'lm'){
    model <- linear_reg(name_model, data_for_model, model_variables)
    #model <- "cheese"
    return(model)
  }
  else if(model_type == 'knn'){
    #model <- knn_model(name_model, data_for_model, model_variables)
    return('')
    
  }
  else if(model_type == 'rft'){
    if (grepl('pm_raw', name_model)){
      return()
    } 
    else{
    model <- rft_model(name_model, data_for_model, model_variables)
    return(model)}
    
  }
  else{
    return('did not pass')
  }
  #return(name_model)

}

evaluate_model <- function(alg_site_name, site_data, corr_alg, eval_site_name, corr_site_name){
  #purpose of this function is to evaluate each model that is created
  #bring data files of merged data with the various fits created
  #List of fits and the different sites used to evaluate the data
  library(openair)
  #need a list of all of the combinations, or two for loops
  #create name for the new dataframe
  #create column name based on the model being used
  
  model_stats <- data.frame(matrix(ncol = 0, nrow = 1))
  
  model_stats$model_name <- alg_site_name
  
  coef(corr_alg$finalModel)
  
  site_data_temp <- site_data
  
  site_data_temp$pm25_fit<-predict(corr_alg, site_data)
  site_data$date <- as.POSIXct(site_data$date, tz="MST", format = '%Y-%m-%d %H:%M:%S')
  
  model_stats$EVAL_SITE <- eval_site_name
  model_stats$CORR_SITE <- corr_site_name
  
  ##Model stats for evaluating models
  
  #model_stats$COR <- cor(site_data_temp$val.pm25_p.y, site_data_temp$pm25_fit)
  #model_stats$RMSE <- sqrt(mean((site_data_temp$val.pm25_p.y - site_data_temp$pm25_fit)^2)) #rmse(site_data_temp$val.pm25_p.y, site_data_temp$pm25_fit)
  #model_stats$MAE <- mean(abs(site_data_temp$val.pm25_p.y - site_data_temp$pm25_fit))
  #model_stats$BIAS <- mean(site_data_temp$val.pm25_p.y - site_data_temp$pm25_fit) #bias(site_data_temp$val.pm25_p.y, site_data_temp$pm25_fit)
  
  ##Model stats for whole sensor network
  model_stats$MAX <- max(site_data_temp$pm25_fit)
  model_stats$MIN <- min(site_data_temp$pm25_fit)
  model_stats$MEDIAN <- median(site_data_temp$pm25_fit)
  model_stats$AVG <- mean(site_data_temp$pm25_fit)
  model_stats$STDEV <- sd(site_data_temp$pm25_fit)
  model_stats$NROWS <- nrow(site_data_temp)
  
  #Cutting data into different time blocks
  site_data_temp$date <- as.POSIXct(site_data_temp$date, tz="UTC", format = '%m/%d/%Y %H:%M')
  site_data_temp <- cutData(site_data_temp, type = 'hour')
  site_data_temp2 <- cutData(site_data_temp, type = 'weekend')

  ##Writing data with predicted data to a folder
  predict_filename <- paste(eval_site_name, '_LC_smoke', '.csv', sep = '')
  write.csv(site_data_temp2, paste(predicted_pm_dir, predict_filename, sep = '/'))
  
  #output a row that can be added to a table to combine all together###
  return(model_stats)
  
}

linear_reg <- function(model_name, site_data, variables){
  set.seed(1234)
  temp_data <-site_data[c(variables, 'val.pm25_p.y')]
  fit_ln <-train(val.pm25_p.y ~., data=temp_data, method="lm",
               trControl=trainControl(method="cv", verboseIter =T),
               na.action = na.pass)
  print('model_name')
  return(fit_ln)
}


knn_model <- function(site_data, variables){
  set.seed(1234)
  temp_data <-subset(site_data, select= variables)
  
}

rft_model <- function(model_name, site_data, variables){
  
  library(ranger)
  set.seed(1234)
  ##Create a function that takes in the variables and returns the model
  temp_data <-site_data[c(variables, 'val.pm25_p.y')]
  fit_rft <- train(val.pm25_p.y ~., data=temp_data, method="ranger",
               trControl=trainControl(method="cv", verboseIter =F),
               num.trees=100,
               importance="permutation", na.action = na.pass)
  fit_rft
}






