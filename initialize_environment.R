#Initiate Environment Variables

#Date: 09/07/2022

#Run this file first to create working directory folders

#The purpose of this script is to initialize the environment for all of the other
#processing fore this project

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

#All libraryies required
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



wd <- getwd()
setwd(wd)

aq_data_dir <- paste(wd, 'Processed_Data', sep = '/')
aq_raw_dir <- paste(wd, "Raw_Data", sep = '/')

traffic_data_dir <- paste(wd, 'Denver_Traffic_Data', sep = '/')
asthma_data_dir <- paste(wd, 'Asthma_Hospitalization_Rate_(Census_Tracts)', sep = '/')
proc_files_dir <- paste(wd, 'Data Processing Files', sep = '/')
create_corr_alg_dir <- paste(wd, 'Create Corr Alg', sep = '/')

ped_routes_dir <- paste(wd, 'pedestrian_routes', sep = '/')
streets_dir <- paste(wd, 'street_routes', sep = '/')

meta_data_dir <- paste(wd, 'Meta_Data', sep = '/')
alg_data_dir <- paste(wd, 'Data_from_Alg', sep = '/')
corr_alg_dir <- paste(wd, 'Corr_Alg_Dir', sep = '/')

