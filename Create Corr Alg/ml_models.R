#Algorithm Functions
#What do I want the fuctions to input and output?

#input a dataframe from each site
#input list of variables to perform operation

create_model <- function(){
  #choose model function redirects to one of the 3 models depending on the inputs
  
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






