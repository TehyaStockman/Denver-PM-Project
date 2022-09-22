#Algorithm Functions
#What do I want the fuctions to input and output?

#input a dataframe from each site
#input list of variables to perform operation

create_model <- function(name_model, data_for_model, model_type, model_variables){
  #bring in data/models to pass through
  #choose model function redirects to one of the 3 models depending on the inputs
  if(model_type == 'ln'){
    #model <- linear_reg(name_model, site_data, variables)
    return('ln_cheese')
  }
  else if(model_type == 'knn'){
    #model <- knn_model(name_model, site_data, variables)
    return('knn_steak')
  }
  else if(model_type == 'rft'){
    #model <- rft_model(name_model, site_data, variables)
    return('rft_beef')
  }
  else{
    next
  }
  #return(model)
  #return(name_model)

}

evaluate_model <- function(){
  #purpose of this function is to evaluate each model that is created
  #
  
  
}

linear_reg <- function(model_name, site_data, variables){

  set.seed(1234)
  temp_data <-subset(site_data, select= variables)
  fit_ln <-train(val.pm25_p.y ~., data=site_data, method="lm",
               trControl=trainControl(method="cv", verboseIter =T),
               na.action = na.pass)
  fit_ln
}


knn_model <- function(site_data, variables){
  set.seed(1234)
  temp_data <-subset(site_data, select= variables)
  
}

rft_model <- function(site_data, variables){
  
  library(ranger)
  set.seed(1234)
  read.csv(filename) ## Make complete
  ##Create a function that takes in the variables and returns the model
  fit_rft <- train(val.pm25_p.y ~., data=site_data, method="ranger",
               trControl=trainControl(method="cv", verboseIter =F),
               num.trees=100,
               importance="permutation", na.action = na.pass)
  fit_rft
}






