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

evaluate_model <- function(site_data_file, corr_alg_file){
  #purpose of this function is to evaluate each model that is created
  #bring data files of merged data with the various fits created
  #List of fits and the different sites used to evaluate the data
  
  #need a list of all of the combinations, or two for loops
  #create name for the new dataframe
  #create column name based on the model being used
  
  model_name <- ''
  
  coef(corr_alg$finalModel)
  
  data_model_all$pm25_fit1<-predict(corr_alg, site_data)
  
  cor(data_model_all$val.pm25_p.y, data_model_all$pm25_fit1)
  rmse(data_model_all$val.pm25_p.y, data_model_all$pm25_fit1)
  mae(data_model_all$val.pm25_p.y, data_model_all$pm25_fit1)
  bias(data_model_all$val.pm25_p.y, data_model_all$pm25_fit1)
  
  data_cs_hr$pm_fit1<-predict(fit1, data_cs_hr)
  
  #output a row that can be added to a table to combine all together###
  model_stats <- c()
  
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






