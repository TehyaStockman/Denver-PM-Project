#Pearson Correlation Coefficient and CoD -- trying with the Reference Monitors First
install.packages('shipunov')
install.packages("ggpubr")

library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(chron)
library(lubridate)
library(shipunov)
library(ggpubr)


wd <- getwd()
setwd(wd)


data_dir <- 'Processed_Data/Data_By_Sensor_Wide'
AQ_files <- list.files(data_dir)

#Reference Monitor Files
site_metadata <- read.csv("Sensor_MetaData_Compiled.csv", header = TRUE)
ref_sites <- filter(site_metadata, SensorType == "Airnow")
ref_sites_list <- unique(ref_sites$ID)
ref_sites_list2 <- c()
ref_sites_list3 <- c()

ref_sites_list <- str_replace_all(ref_sites_list, '/', ' ')

##MODIFY LIST -- check for names that are not in the file list first before using list
for (site in ref_sites_list){
  filename <- paste('wide_', site, ' airnow' ,'.csv', sep = '')
  if(filename %in% AQ_files){
    ref_sites_list3 <- append(ref_sites_list3, site)
  }else{
   next
  }
}


#initialize dataframe to store variables
cod_values <- data.frame(matrix(nrow = 0, ncol = 0))
pearson_values <- data.frame(matrix(nrow = 0, ncol = 0))


for (site in ref_sites_list3){
  #open and read csv file, create data frame
  filename <- paste('wide_', site, ' airnow' ,'.csv', sep = '')
  site1_data <- read.csv(paste(data_dir,filename, sep = "/"), header = T)
  site1_data <- subset(site1_data, select = c('date', 'val.pm25_p'))
  print(filename)
  
  for (site2 in ref_sites_list3){
    if(site2 %in% ref_sites_list2){
      next
    } else {
      filename <- paste('wide_', site2, ' airnow' , '.csv', sep = '')
      site2_data <- read.csv(paste(data_dir,filename, sep = "/"), header = T)
      site2_data <- subset(site2_data, select = c('date', 'val.pm25_p'))
      
      data_both <- merge(site1_data, site2_data, by = 'date', all = TRUE)
      #call CoD function here
      cod <- K(data_both$val.pm25_p.x, data_both$val.pm25_p.y, na.rm = TRUE)
      cod_values[site, site2] <- cod
      
      r_vals <- cor(data_both$val.pm25_p.x, data_both$val.pm25_p.y,  method = "pearson", use = "complete.obs")
      pearson_values[site, site2] <- r_vals
    }
    
  }
  ref_sites_list2 <- append(ref_sites_list2, site) 

}



#combn -- loop up this function