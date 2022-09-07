#Plotting and Checking Data

#SET THE WORKING DIRECTORY HERE
setwd("/Users/tehyastockman/DDPHE/PM_Data_Cleaning/")

#Make sure these files are included in the same folder as this current script
source("lunar_processing.R")
source("clarity_processing.R")
source("airnow_processing.R")
source("met_processing.R")
site_metadata <- read.csv("Site_MetaData_2.csv")

AQ_monthly_data <- read.csv("Apr2021.csv", header = TRUE)


df <- AQ_monthly_data %>% count(ID,parameter, flag)
df <- merge(df, site_metadata, by = "ID", all = TRUE) 

max_min <- AQ_monthly_data[AQ_monthly_data$parameter == "pm25_p", ] %>%
  group_by(ID) %>%
  summarize(pm_max.pt = max(val, na.rm = T), pm_min.pt = min(val, na.rm = T), pm_avg.pt = mean(val, na.rm = T))

df <- merge(df, max_min, by = "ID", all = TRUE) 


#AQ_monthly_data %>% count(ID,parameter, flag)


#table(AQ_monthly_data$ID[])