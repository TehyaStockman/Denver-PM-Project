####Colocation 2time line

###This code has 3 parts
####First comparing raw QAQ data with CDPHE data
#####Second getting the Model Coefficients
##### Third applying the model results on raw data


if (!require(installr)) install.packages('installr')
library(installr)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(chron)) install.packages('chron')
library(chron)

if (!require(prodlim)) install.packages('prodlim')
library(prodlim)

if (!require(gtools)) install.packages('gtools')
library(gtools)

#if (!require(AirSensor)) install.packages('AirSensor')
#library(AirSensor)

if (!require(caret)) install.packages('caret')
library(caret)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(openair)) install.packages('openair')
library(openair)

if (!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)

if (!require(broom)) install.packages('broom')
library(broom)

#if (!require(weathercan)) install.packages('weathercan')
#library(weathercan)

if (!require(segmented)) install.packages('segmented')
library(segmented)


###linear 

install.packages("data.table")
install.packages("dplyer")
install.packages("readr")
install.packages("lubridate")
install.packages("fs")
install.packages("tidyverse")
install.packages("chron")
install.packages("stringr")
install.packages("openair")
install.packages("naniar")
install.packages("pracma")
install.packages("splitstackshape")
install.packages("ggplot2")

library(data.table)
library(dplyr)
library(tidyr)
library(fs)
library(tidyverse)
library(stringr)
library(chron)
library(openair)
library(naniar)
library(ggplot2)
library(pracma)
library(splitstackshape)
library(lubridate)
install.packages("ggpmisc")
library(ggpmisc)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
install.packages("broom")
library(broom)
install.packages('data.table')
library(data.table)

#SET THE WORKING DIRECTORY HERE
rm(list = ls())
workFol <- getwd()
setwd(workFol)

##setting up folders
## Setup Folder Locations
rawDatFol <- paste(workFol,"/Raw Data",sep="")
rawDatFol <- paste(workFol,"/Raw Data",sep="")
rawFigFol <- paste(workFol,"/Raw Figures",sep="")
rawTabFol <- paste(workFol,"/Raw Tables",sep="")
hourDatFol <- paste(workFol,"/Hourly Data",sep="")
hourFigFol <- paste(workFol,"/Hourly Figures",sep="")
hourTabFol <- paste(workFol,"/Hourly Tables",sep="")
DataStatFol<- paste(workFol,"/Data Stat",sep="")
CDPHEDatFol<-paste(workFol,"/CDPHE Raw DATA",sep="")
Folders <- data.frame(workFol,rawDatFol,rawFigFol,rawTabFol,hourDatFol,hourFigFol,hourTabFol,DataStatFol,
                      CDPHEDatFol)





##########################################################################################
#############################################Section One###################################

ProcLdat <- paste(workFol,"/Processed Data",sep="")

AQ_data_long <- load(paste(ProcLdat,"/Processed Data_QAQ_Col2.RData",sep=""))



names(AQ_data)[names(AQ_data) == "Date"] <- "date"
names(AQ_data)[names(AQ_data) == "sn"] <- "Sensor"
names(AQ_data)[names(AQ_data) == "rh"] <- "Humidity"
names(AQ_data)[names(AQ_data) == "temp"] <- "Temperature"
names(AQ_data)[names(AQ_data) == "pm25"] <- "PM2.5"
names(AQ_data)[names(AQ_data) == "pm1"] <- "PM1"
names(AQ_data)[names(AQ_data) == "pm10"] <- "PM10"



AQ_data$date<-force_tz(AQ_data$date, tzone = "MST")


AQ_data <- subset(AQ_data, select = c('date',"Sensor","PM2.5","Humidity","Temperature"))
AQ_data_long <- as.data.frame(AQ_data)


AQ_data_long <- timeAverage(AQ_data_long, avg.time = "hour", vars =c("PM2.5","Humidity","Temperature"),type="Sensor",Date = AQ_data_long$date)
AQ_data_long$date<-force_tz(AQ_data_long$date, tzone = "MST")




start_date_time<- as.POSIXct("2022-04-19 00:00:00", tz = "MST")
end_date_time <- as.POSIXct("2022-05-13 23:00:00", tz = "MST")


AQ_data_long<-subset(AQ_data_long, AQ_data_long$date >= start_date_time & AQ_data_long$date <= end_date_time)

######Before cleaning summary

Rawdata_summary <- data.frame(unclass(summary(AQ_data_long)),  # Convert summary to data frame
                              check.names = FALSE)

Rawdata_summary

output_dir <- paste(workFol,"/Col2_Model_Results/QAQ_Col2_Stat/",sep="")


fileName<- paste(output_dir, 'Col2_QAQ_Rawdata.csv',sep = '')

write.csv(Rawdata_summary,fileName)

p<-summaryPlot(AQ_data_long,xlab="time")

p

#output_dir <- paste(workFol,"/Col2_Model_Results/QAQ_Col2_Stat/",sep="")


#png(filename = paste(output_dir, paste0("Col2_QAQ_Rawd.png"), sep = ""))
#dev.off()




###########In this Section I clean the data #############

#m1<-max(AQ_data_long$PM10,na.rm = TRUE)#
#m1
m2<-max(AQ_data_long$PM2.5,na.rm = TRUE)#32.88
m2
#m3<-max(AQ_data_long$PM1,na.rm = TRUE)
#m3


####Filter
#a=0
a<-nrow(filter(AQ_data_long, PM2.5 >= 500))
a
#b=43
#b<-nrow(filter(AQ_data_long, PM10 >= 500))
#b
#c=0
#c<-nrow(filter(AQ_data_long, PM1 >= 500))
#c

#AQ_data_long$PM1 <- replace(AQ_data_long$PM1, AQ_data_long$PM1 > 500, NA)
#AQ_data_long$PM10 <- replace(AQ_data_long$PM10, AQ_data_long$PM10 > 500, NA)
AQ_data_long$PM2.5 <- replace(AQ_data_long$PM2.5, AQ_data_long$PM2.5> 500, NA)
AQ_data_long$Humidity <- replace(AQ_data_long$Humidity, AQ_data_long$Humidity > 80, NA)
AQ_data_long$Humidity <- replace(AQ_data_long$Humidity, AQ_data_long$Humidity < 10, NA)
AQ_data_long$Temperature <- replace(AQ_data_long$Temperature, AQ_data_long$Temperature < -39.5, NA)
AQ_data_long$Temperature <- replace(AQ_data_long$Temperature, AQ_data_long$Temperature > 89.5 , NA)

############Summary after cleaning#############


Cleandata_summary <- data.frame(unclass(summary(AQ_data_long)),  # Convert summary to data frame
                                check.names = FALSE)

Cleandata_summary

output_dir <- paste(workFol,"/Col2_Model_Results/QAQ_Col2_Stat/",sep="")


fileName<- paste(output_dir, 'Col2_QAQ_Cleandata.csv',sep = '')

write.csv(Cleandata_summary,fileName)

p<-summaryPlot(AQ_data_long)

p

##########Calculating the Dew point#################

#df_Weather_1h<-subset(Weather_1h, Weather_1h$date >= start_date_time & Weather_1h$date <= end_date_time)


a<-log(AQ_data_long$Humidity/100)
b<-((17.625*AQ_data_long$Temperature)/(243.04+AQ_data_long$Temperature))
c<-17.625-a

AQ_data_long$tdew<-243.04*((a+b)/(c-b))








### loading and cleaning CDPHE data


#####
datInCDPHE<-paste(workFol,"/CDPHE Raw DATA",sep="")

ATM_Col2_CDPHE_FEMh <- read.csv(paste(datInCDPHE,"/globeville0714.csv",sep=""))

ATM_Col2_CDPHE_FEMh$datetime<-as.character(ATM_Col2_CDPHE_FEMh$datetime)
ATM_Col2_CDPHE_FEMh$datetime<-mdy_hm(ATM_Col2_CDPHE_FEMh$datetime)


ATM_Col2_CDPHE_FEMh$datetime<-as.POSIXct(ATM_Col2_CDPHE_FEMh$datetime,tz="MST" ,tryFormats=c('%Y-%m-%d %T'))

names(ATM_Col2_CDPHE_FEMh)[names(ATM_Col2_CDPHE_FEMh) == "datetime"] <- "date"


#Globville_CDPHE_FEMh <- read.csv(paste(datInCDPHE,"/Globeville.csv",sep=""))

#ATM_Col2_CDPHE_FEMh$date <- as.POSIXct(ATM_Col2_CDPHE_FEMh$date,format="%Y-%m-%d %H:%M:%S")

### Filter for Col 3 dates and hourly average of PM2.5

ATM_Col2_CDPHE_FEMh$date<-ATM_Col2_CDPHE_FEMh$date+60*60




ATM_Col2_CDPHE_FEMh <-as.data.frame(ATM_Col2_CDPHE_FEMh)

#ATM_Col2_CDPHE_FEMh%>% filter(between(date, as.Date('2022-08-16'), as.Date('2022-09-16')))



Col2_CDPHE_h<-subset(ATM_Col2_CDPHE_FEMh, date >= as.Date("2022-04-19") & date <= as.Date("2022-05-14"))


#start_date_time<- as.POSIXct("2022-08-16 00:00:00")
#end_date_time <- as.POSIXct("2022-09-16 23:00:00")

#Col2_CDPHE_h<-subset(ATM_Col2_CDPHE_FEMh, ATM_Col2_CDPHE_FEMh$date >= start_date_time & ATM_Col2_CDPHE_FEMh$date <= end_date_time)

Col2_CDPHE_h<-na.omit(Col2_CDPHE_h)




##################CDPHE raw data summary#############


FEM_Rawdata_summary <- data.frame(unclass(summary(Col2_CDPHE_h)),  # Convert summary to data frame
                                  check.names = FALSE)

FEM_Rawdata_summary

output_dir <- paste(workFol,"/Col2_Model_Results/QAQ_Col2_Stat/",sep="")


fileName<- paste(output_dir, 'Col2_FEM_Rawdata.csv',sep = '')

write.csv(FEM_Rawdata_summary,fileName)

p<-summaryPlot(Col2_CDPHE_h)

p

#############CDPHE cleaning#########
#Col2_CDPHE_h$pm2.5 <- replace(Col2_CDPHE_h$pm2.5, Col2_CDPHE_h$pm2.5> 500, NA)
#Col2_CDPHE_h$rh <- replace(Col2_CDPHE_h$rh, Col2_CDPHE_h$rh > 80, NA)
#Col2_CDPHE_h$rh <- replace(Col2_CDPHE_h$rh, Col2_CDPHE_h$rh < 10, NA)
#Col2_CDPHE_h$Temperature <- replace(Col2_CDPHE_h$Temperature, Col2_CDPHE_h$Temperature < -39.5, NA)
#Col2_CDPHE_h$Temperature <- replace(Col2_CDPHE_h$Temperature, Col2_CDPHE_h$Temperature > 89.5 , NA)

##################CDPHE after cleaning data summary#############


#FEM_cleandata_summary <- data.frame(unclass(summary(Col2_CDPHE_h)),  # Convert summary to data frame
#                                 check.names = FALSE)

#FEM_cleandata_summary

#output_dir <- paste(workFol,"/Col2_Model_Results/QAQ_Col2_Stat/",sep="")


#fileName<- paste(output_dir, 'Col2_FEM_cleandata.csv',sep = '')

#write.csv(FEM_cleandata_summary,fileName)

#p<-summaryPlot(Col2_CDPHE_h)

#p






##########3

Col2_CDPHE_h <- subset(Col2_CDPHE_h, select = c("date", 'pm2.5'))

names(Col2_CDPHE_h)[names(Col2_CDPHE_h) == "pm2.5"] <- "CDPHEPM25"










#AQ_data_long <-AQ_data

#### make a list of sensor names


Sensor_name_List<-unique(AQ_data_long$Sensor)

Sensor_name_List

Lm_stat <- data.frame(matrix(ncol =0, nrow = 0))
Coef_Data <- data.frame(matrix(ncol =0, nrow = 0))

RSE_Data <- data.frame(matrix(ncol =0, nrow = 0))

#####Excluding QAQ208  becaucse it seem sto be damaged

Sensor_name_List<-Sensor_name_List[-5]
Sensor_name_List

for(i in Sensor_name_List ){
  set.seed(1234)
  
  temp_data<-filter(AQ_data_long, Sensor==i)
  
  temp_data<-subset(temp_data, select=c("date","Humidity","tdew","PM2.5"))
  variables<-c("Humidity","tdew","PM2.5")
  temp_data$date<-as.POSIXct(temp_data$date,format="%Y-%m-%d %H:%M:%S",tz="MST")
  
  
  ####Hourly averages 
  #temp_data <- timeAverage(temp_data, avg.time = "hour", vars = c("Humidity","tdew","PM2.5"),Date = temp_data$date)
  
  
  temp_data<-merge(temp_data,Col2_CDPHE_h,by="date")
  # temp_data<-na.omit(temp_data)
  temp_data_all<-temp_data[c(variables,"CDPHEPM25")]
  
  
  temp_data_all$Humidity<-as.numeric(temp_data_all$Humidity)
  temp_data_all$tdew<-as.numeric(temp_data_all$tdew)
  temp_data_all$PM2.5<-as.numeric(temp_data_all$PM2.5)
  temp_data_all$CDPHEPM25<-as.numeric(temp_data_all$CDPHEPM25)
  temp_data_all<-na.omit(temp_data_all)
  
  fit_ln <-train(CDPHEPM25 ~., data=(temp_data_all), method="lm",
                 trControl=trainControl(method="cv", verboseIter =T),
                 na.action = na.pass)
  
  
  coefficents<-coef(fit_ln$finalModel,unlist(fit_ln$bestTune))
  print(i)
  print(fit_ln)
  print(coefficents)
  rse<-summary(fit_ln)$sigma
  print(rse)
  
  Lm_stat<-rbind(Lm_stat,fit_ln$results)
  Coef_Data<-rbind(Coef_Data,coefficents)
  RSE_Data<-rbind(RSE_Data,rse)
  
  
  
}

Coef_results<-cbind(Coef_Data,Sensor_name_List)
names(Coef_results)[names(Coef_results) == "Sensor_name_List"] <- "Sensor"
Coef_results<-cbind(Coef_Data,Sensor_name_List)
names(Coef_results)[names(Coef_results) == "Sensor_name_List"] <- "Sensor"
colnames(Coef_results)[1] = "Intercept"
colnames(Coef_results)[2] = "Humidity"
colnames(Coef_results)[3] = "Tdew"
colnames(Coef_results)[4] = "PM2.5"


Lm_results<-cbind(Lm_stat,Sensor_name_List)
Lm_results<-Lm_results[-1]
names(Lm_results)[names(Lm_results) == "Sensor_name_List"] <- "Sensor"

RSE_results<-cbind(RSE_Data,Sensor_name_List)
names(RSE_results)[names(RSE_results) == "Sensor_name_List"] <- "Sensor"
colnames(RSE_results)[1] = "Sigma"


output_dir <- paste(workFol,"/Col2_Model_Results/CL2_QAQ_LMMODEL/",sep="")


fileName<- paste(output_dir, 'Col2_QAQ_Coef_RHTDEW.csv',sep = '')

write.csv(Coef_results,fileName)
save(Coef_results, file =paste(output_dir, 'Col2_QAQ_Coef_RHTDEW.RData',sep = ''))



fileName<- paste(output_dir, 'Col2_QAQ_LM_RHTDEW.csv',sep = '')

write.csv(Lm_results,fileName)
save(Lm_results,file =paste(output_dir, 'Col2_QAQ_LM_RHTDEW.RData',sep = ''))


fileName<- paste(output_dir, 'Col2_QAQ_RSE_RHTDEW.csv',sep = '')

write.csv(RSE_results,fileName)
save(RSE_results, file =paste(output_dir, 'Col2_QAQ_RSE_RHTDEW.RData',sep = ''))



###############Summary of Model stat########


Model_summary <- data.frame(unclass(summary(Coef_results)),  # Convert summary to data frame
                            check.names = FALSE)

Model_summary

output_dir <- paste(workFol,"/Col2_Model_Results/CL2_QAQ_LMMODEL/",sep="")


fileName<- paste(output_dir, 'Col2_QAQ_MODEL_Coef.csv',sep = '')

write.csv(Model_summary,fileName)


##########


Model_summary <- data.frame(unclass(summary(Lm_results)),  # Convert summary to data frame
                            check.names = FALSE)

Model_summary

output_dir <- paste(workFol,"/Col2_Model_Results/CL2_QAQ_LMMODEL/",sep="")


fileName<- paste(output_dir, 'Col2_QAQ_MODEL_LM.csv',sep = '')

write.csv(Model_summary,fileName)

###

Model_summary <- data.frame(unclass(summary(RSE_results)),  # Convert summary to data frame
                            check.names = FALSE)

Model_summary

output_dir <- paste(workFol,"/Col2_Model_Results/CL2_QAQ_LMMODEL/",sep="")


fileName<- paste(output_dir, 'Col2_QAQ_MODEL_RSE.csv',sep = '')

write.csv(Model_summary,fileName)



sd(Coef_results$Intercept)
sd(Coef_results$Humidity)
sd(Coef_results$Tdew)
sd(Coef_results$PM2.5)

sd(Lm_results$RMSE)

summary(Coef_results)

############## applying model results on Raw data#############
#






install.packages("data.table")
install.packages("dplyer")
install.packages("readr")
install.packages("lubridate")
install.packages("fs")
install.packages("tidyverse")
install.packages("chron")
install.packages("stringr")
install.packages("openair")
install.packages("naniar")
install.packages("pracma")
install.packages("splitstackshape")
install.packages("remotes")
install.packages("ggpubr")
install.packages('ggfortify')
install.packages("caTools")       
install.packages("randomForest")  
install.packages("ggplot2")
install.packages("shipunov")
install.packages("rlang")
install.packages("magrittr")
install.packages("caret")

library(data.table)
library(dplyr)
library(tidyr)
library(fs)
library(tidyverse)
library(stringr)
library(chron)
library(openair)
library(naniar)
library(ggplot2)
library(pracma)
library(splitstackshape)
library(lubridate)
library(ggpubr)
library(ggfortify)
library(caTools)
library(randomForest)
library(shipunov)
library(deming)
library(caret)
#getting the working directory



###mean values from colocation 3 analysis of ATM data

intercept<-5.697
RH<- -5.364e-03
Tdew<- 0.1895
PM_COR<-0.4582





#CHANGE THIS TO MATCH THE MATCH WHERE THE FILE IS GOING
output_dir <- paste(workFol,"/Col2_Model_Results/CL2_QAQ_Corrected/",sep="")


#### make a list of sensor names

Sensor_name_List<-unique(AQ_data_long$Sensor)

Sensor_name_List<-Sensor_name_List[-5]
Sensor_name_List



#____________________________ Looping Through Data _____________________________________
#Creating a dictionary of file names to look up that link to data tables
#data <- list()


aq_data <- data.frame(matrix(ncol = 0, nrow = 0))

for(i in Sensor_name_List ){
  
  
  temp_data<-filter(AQ_data_long, Sensor==i)
  
  temp_data<-subset(temp_data, select=c("date","Humidity","tdew","PM2.5","Sensor"))
  
  temp_data$date<-as.POSIXct(temp_data$date,format="%Y-%m-%d %H:%M:%S",tz="MST")
  
  
  
  ####Hourly averages 
  
  temp_data$PM25_P<-temp_data$PM2.5*PM_COR + temp_data$Humidity*RH +temp_data$tdew*Tdew+ intercept
  
  #print(temp_data)
  
  aq_data<-rbind(aq_data,temp_data)
  
}


aq_data$date <- as.character(as.character(aq_data$date))

aq_data$date<-ymd_hms(aq_data$date,quiet = FALSE, tz ="MST")

#AQ_data$Date <- as.numeric(as.character(AQ_data$Date))                         
aq_data$date <- as.POSIXct(aq_data$date, tz = "MST", tryFormats=c('%Y-%m-%d %T'))



fileName<- paste(output_dir, 'QAQ_COR_CL2.csv',sep = '')

write.csv(aq_data,fileName)
save(aq_data, file =paste(output_dir, 'QAQ_COR_CL2.RData',sep = ''))



summary(aq_data)
############################################################################Model values after correction###


Col2_CDPHE_h$Sensor<- c("FEM")
#Col2_CDPHE_h$date<- as.POSIXct(Col2_CDPHE_h$date, tz = "MST")
Col2_CDPHE_h$date<-force_tz(Col2_CDPHE_h$date, tzone = "MST")

names(Col2_CDPHE_h)[names(Col2_CDPHE_h) == "CDPHEPM25"] <- "PM2.5"



Col2_CDPHE_h <- Col2_CDPHE_h[, c("date", "Sensor", "PM2.5")]



aq_data<-aq_data[, c("date", "Sensor", "PM25_P")]

names(aq_data)[names(aq_data) == "PM25_P"] <- "PM2.5"


#aq_data$date<-as.POSIXct(aq_data$date, tz="MST")
aq_data$date<-force_tz(aq_data$date, tzone = "MST")


aq_data$PM2.5<-round(aq_data$PM2.5, digits = 2)



df_1h<-rbind(aq_data,Col2_CDPHE_h)

df_1h<-as.data.frame(df_1h)


df_1h_scatter<-reshape(df_1h, timevar = 'Sensor', idvar = 'date', id ="PM2.5", direction = 'wide')



#df_1h_scatter<-spread(df_1h,Sensor,PM2.5)

df_1h_scatter1<-df_1h_scatter

#df_1h_scatter1$date <-NULL
names(df_1h_scatter)[names(df_1h_scatter) == "PM2.5.FEM"] <- "FEM"


df_1h_scatter2<- gather(df_1h_scatter,Sensor,PM2.5,-date,-FEM)


df_1h_accuracy <- df_1h_scatter2 %>%
  mutate(sq.residual=(PM2.5 - FEM)^2)

#df_1h_accuracy <-na.omit(df_1h_accuracy)


# Slope, intercept and R2. Quickly calculated using R Package 'brooms' and their tidy and glance functions

model_params_1h <- df_1h_accuracy %>% nest(data = -Sensor) %>% 
  mutate(model = map(data, ~lm(PM2.5 ~ FEM, data = .)), tidied = map(model, tidy)) %>% unnest(tidied) 

model_stats_1h <- df_1h_accuracy %>% nest(data = -Sensor) %>% 
  mutate(model = map(data, ~lm(PM2.5 ~ FEM, data = .)), tidied = map(model, glance)) %>% unnest(tidied)

slopes_1h <- model_params_1h %>% filter(term=="FEM") 
slopes_1h <-subset(slopes_1h,select=c("Sensor","estimate")) %>% rename(.,slope_1h=estimate)


intercepts_1h <- model_params_1h %>% filter(term=="(Intercept)") 

intercepts_1h <-subset(intercepts_1h,select=c("Sensor","estimate"))%>% rename(.,intercept_1h=estimate)

r2_1h <- model_stats_1h

r2_1h<-subset(r2_1h,select=c("Sensor","r.squared"))%>% rename(.,r2_1h=r.squared)

# RMSE and NRMSE: Manual calculations. 

# Calculate RMSE and NRMSE
RMSE_1h <- df_1h_accuracy %>%
  group_by(Sensor) %>%
  summarize(RMSE_1h=sqrt(mean(sq.residual,na.rm=T)),
            NRMSE_1h=sqrt(mean(sq.residual,na.rm=T))/mean(df_1h_accuracy$FEM,na.rm=T)*100)

accuracy_1h <-  left_join(r2_1h, slopes_1h, by='Sensor') %>%
  left_join(., intercepts_1h, by='Sensor') %>%
  left_join(.,RMSE_1h,by='Sensor')




###save model results
output_dir <- paste(workFol,"/Model-Results_Zimmerman/",sep="")


fileName<- paste(output_dir, 'Col2_globeville0714_COR_QAQ2.csv',sep = '')

write.csv(accuracy_1h,fileName)
save(accuracy_1h, file =paste(output_dir, 'Col2_globeville0714_COR_QAQ2.RData',sep = ''))


summary(accuracy_1h)








####Now plot the heat map for CDPHE data vs corrected Data##########################3

####Now plot the heat map for CDPHE data vs corrected Data

### read corrected data and make it wide format

CL2_Cor<-aq_data


CL2_Cor <- subset(CL2_Cor, select = c("date", 'PM2.5', "Sensor"))

CL2_Cor$date<-force_tz(CL2_Cor$date, tzone = "MST")

CL2_Cor<-na.omit(CL2_Cor)
CL2_Cor$Sensor<-as.character(CL2_Cor$Sensor)
CL2_Cor<-as.data.frame(CL2_Cor)

CL2_Cor_wide<-reshape(CL2_Cor , timevar = 'Sensor', idvar = 'date', id = 'PM25_P', direction = 'wide')

for ( col in 1:ncol(CL2_Cor_wide)){
  colnames(CL2_Cor_wide)[col] <-  sub("PM25_P.", "", colnames(CL2_Cor_wide)[col])
}


### loading and cleaning CDPHE data

Col2_CDPHE_h$Sensor<-"FEM"


Col2_CDPHE_h$date<-force_tz(Col2_CDPHE_h$date, tzone = "MST")


Col2_CDPHE_h<-na.omit(Col2_CDPHE_h)



#####Merging the data sets
all_CL2<-merge(CL2_Cor_wide,Col2_CDPHE_h)

all_CL2$Sensor <-NULL


#all_CL2$date <- with_tz(all_CL2$date, "MST")


#CL2_h_matrix2 <- data.matrix(all_CL2)
all_CL2$date <-NULL
#all_CL2$ATM.01<-as.numeric(all_CL2$ATM.01)
#all_CL2<-na.omit(all_CL2)
head(all_CL2)
cormat <- cor(all_CL2,use="pairwise",method="pearson")
head(cormat)
cormat<-as.data.frame(cormat)
install.packages("ggcorrplot")
library("ggcorrplot")

output_dirc<- paste(workFol,"/Correlation Matrix Map/",sep="")

p<-ggcorrplot(cormat,method="square", ggtheme = ggplot2::theme(plot.title = element_text(size = 8)),
              title = "Corrected ATM vs CDPHE Colocation three
Correlation Matrix Pearson RH8020/Tdew",
              type = "full",
              lab = TRUE,
              show.legend = TRUE,
              legend.title = "Legend",
              show.dia = NULL,
              colors = c("blue", "white", "green"),
              outline.color = "gray",  lab_col = "black",
              lab_size =1.5, tl.srt=90, tl.cex =7)

p

ggsave(p, filename = paste(output_dirc, paste0("ATM_CL2_CDPHE_RHTDEW_Corrected",".jpeg"), sep = "/"))











df_1h<-merge(CL2_Cor,Col2_CDPHE_h,by="date")

df_1h$Sensor.y<-NULL

names(df_1h)[names(df_1h) == "CDPHEPM2.5"] <- "FEM"
names(df_1h)[names(df_1h) == "PM25_P"] <- "PM2.5"

names(df_1h)[names(df_1h) == "Sensor.x"] <- "Sensor"


df_1h_scatter<-df_1h


#df_1h_scatter2<-gather(df_1h_scatter2,Sensor,Humidity,-date,-FEM)
#(df_1h_scatter2)[names(df_1h_scatter2) == "date"] <- "Date"

#names(df_1h_scatter2)[names(df_1h_scatter2) == "FEM"] <- "FEMH"
#names(df_1h_scatter2)[names(df_1h_scatter2) == "Sensor"] <- "SensorH"

#df_1h_scatter<-cbind(df_1h_scatter1,df_1h_scatter2)

#df_1h_scatter<- subset(df_1h_scatter, select = c("date","FEM","Sensor","PM2.5","Humidity"))
output_dirc <- paste(workFol,"/Col2_Model_Results/CL2_QAQ_Corrected/",sep="")

p<-ggplot(df_1h_scatter,aes(x=PM2.5.y,y=PM2.5.x,group=Sensor,color=Sensor))+
  geom_point(alpha=0.7,size=1) +
  xlab(expression(CDPHEPM25~PM[2.5]~(µg/m^3))) +
  ylab(expression(Sensor~PM[2.5]~(µg/m^3))) +
  xlim(0,30) +
  ylim(0,30) +
  ggtitle("Colocation Two 1 h Average") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=10)) +
  geom_abline(slope=1,linetype="dashed") +
  coord_equal()+
  theme_bw()


p


filename<-"QAQ_CL2_Sensor_Corrected"

ggsave(paste(output_dirc,paste0(filename,".png")),plot=last_plot(),width=20,height=20,units="cm")


df_1h_scatter_Ex<-na.omit(df_1h_scatter)

output_dirc <- paste(workFol,"/Col2_Model_Results/CL2_QAQ_Corrected/",sep="")

p<-ggplot(df_1h_scatter_Ex,aes(x=PM2.5.y,y=PM2.5.x,group=Sensor,color=Sensor)) +
  geom_point(alpha=0.7,size=1) +
  xlab(expression(CDPHEPM25~PM[2.5]~(µg/m^3))) +
  ylab(expression(Sensor~PM[2.5]~(µg/m^3))) +
  xlim(0,30) +
  ylim(0,30) +
  ggtitle("Colocation Two 1 h Average") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=10)) +
  
  geom_abline(slope=1,linetype="dashed") +
  coord_equal()+
  theme_bw()


p

#library(viridis)
#p+scale_color_viridis(option = "D")


filename<-"QAQ_CL2_NA_Corrected"

ggsave(paste(output_dirc,paste0(filename,".png")),plot=last_plot(),width=20,height=20,units="cm")


####
