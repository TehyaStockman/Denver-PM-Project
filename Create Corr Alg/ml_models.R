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

evaluate_model <- function(){
  #purpose of this function is to evaluate each model that is created
  #
  
  
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






