###Applying Corrections to Sensor Network

##Put all algs into a single folder for ones that will be used




##Make sure sensor data is all in a single folder
###Add smoke events to each of the sensor data files
##Using Data_By_Sensor_Wide
####Remove PurpleAir sensors
#Subset folder with datasets removed that will not use

#Loop through sensor data, add smoke events, and make sure everything in appropriate layout

sensor_data_files <- list.files(sensor_subset_dir)

smoke_events <- read.csv(paste(meta_data_dir, 'smokey_events.csv', sep = '/'))
smoke_events <- smoke_events[,-1]

i = 1
for (sensor_file in sensor_data_files) {
  sensor_data <- read.csv(paste(sensor_subset_dir, sensor_file, sep = '/'))  
  sitename <- sensor_data_files[i]
  
  if(!("val.pm25_r" %in% colnames(sensor_data)))
  {names(sensor_data)[names(sensor_data) == "val.pm25_p"] <- "val.pm25_r"}

  if("age_weeks" %in% colnames(sensor_data))
  {names(sensor_data)[names(sensor_data) == "age_weeks"] <- "age_weeks.x"}

  # #merge smokey data with ref data
  # all_data <- merge(x = sensor_data, y = smoke_events, by = 'date', all = TRUE)
  # 
  # all_data$flag2[all_data$flag == 'smoke'] <- 1
  # all_data$flag2[all_data$flag == 'smoke?'] <- 1
  # all_data$flag2[is.na(all_data$flag)] <-0
  # 
  # names(all_data)[names(all_data) == "flag"] <- "flag3"
  # names(all_data)[names(all_data) == "flag2"] <- "flag"
  # 
  # #make sure columns are all correctly labeled
  # if("val.temperature.x" %in% colnames(all_data))
  # {names(all_data)[names(all_data) == "val.temperature.x"] <- "val.temperature"}
  # 
  # all_data <- subset(all_data, select = -c(flag3))
  # #Remove NA values from the data set
  # all_data <- drop_na(all_data)
  # 
  # all_data <- distinct(all_data, .keep_all = TRUE)

  write.csv(sensor_data, paste(aq_data_dir, '/', sitename, sep = ''))
  i = i +1
}



#####Create predictions for each sensor
file_dir <- paste(aq_data_dir, "Data_By_Sensor_Wide", sep = '/')

corr_alg_files <- list.files(corr_alg_subset_dir)
AQ_files <- list.files(sensor_subset_dir)

aq_corr_alg_models <- list()

AQ_stuff <- data.frame(matrix(ncol = 0, nrow = 0))

for(file in AQ_files){
  sensor_data <- read.csv(paste(file_dir, file, sep = '/'))
  temp_data <- data.frame(matrix(ncol = 0, nrow = 1))
  
  temp_data$sensor_name <- file
  temp_data$max_age <- max(sensor_data$age_weeks)
  AQ_stuff <- rbind(AQ_stuff, temp_data)
}


##Loop through folder of algs and folder of sensor data

for(alg_file in corr_alg_files){
  
  for(file in AQ_files){
    
    #do not include any file from the list that match
    if(grepl(file_path_sans_ext(file), alg_file, fixed = TRUE))
    { next
    } else{
      temp_list<- list(file, alg_file)}
    
    aq_corr_alg_models <- list.append(aq_corr_alg_models, temp_list)
    
  }
  
}



#Call the evaluation function to create
alg_stats_of_site <- data.frame(matrix(ncol = 5, nrow = 0))
source(paste(create_corr_alg_dir, 'ml_models.R', sep = '/'))


all_corr_alg_models <- foreach(alg_model_index = aq_corr_alg_models, .combine='rbind') %dopar% {
  library(tools)
  library(openair)
  site_data_name <- alg_model_index[[1]]
  corr_alg_name <- alg_model_index[[2]]
  
  site_data <- read.csv(paste(sensor_subset_dir, site_data_name, sep = '/'))
  site_data <- site_data[-1]
  site_data$date <- as.POSIXct(site_data$date, tz="MST", format = '%Y-%m-%d %H:%M:%S')
  
  corr_alg <- readRDS(paste(corr_alg_subset_dir , corr_alg_name, sep = '/'))
  
  alg_site_name <- paste(file_path_sans_ext(site_data_name), 
                         file_path_sans_ext(corr_alg_name))
  
  #add variables to be put into evaluate_model function for naming purposes
  eval_site_name <- file_path_sans_ext(site_data_name)
  corr_site_name <- file_path_sans_ext(corr_alg_name)
  
  alg_stats_of_site <- evaluate_model(alg_site_name,
                                      site_data, corr_alg, site_data_name, corr_site_name)
  
  
  alg_stats_of_site
}



##Debug with normal for loop


for(alg_model_index in aq_corr_alg_models){
  library(openair)
  site_data_name <- alg_model_index[[1]]
  corr_alg_name <- alg_model_index[[2]]
  
  site_data <- read.csv(paste(sensor_subset_dir, site_data_name, sep = '/'))
  site_data <- site_data[-1]
  
  corr_alg <- readRDS(paste(corr_alg_subset_dir, corr_alg_name, sep = '/'))
  
  alg_site_name <- paste(file_path_sans_ext(site_data_name), 
                         file_path_sans_ext(corr_alg_name))
  
  #add variables to be put into evaluate_model function for naming purposes
  eval_site_name <- file_path_sans_ext(site_data_name)
  corr_site_name <- file_path_sans_ext(corr_alg_name)
  
  #input into evaluation model to create table of stats
  alg_stats <- evaluate_model(alg_site_name,
                              site_data, corr_alg, eval_site_name, corr_site_name)
  
  
  alg_stats_of_site <- rbind(alg_stats_of_site, alg_stats)
}

##Stats for each sensor
###avg, max, min, median, stdev

###Add rankings to the sensors in the network and combine data files together




###Choose One Correction, Apply to Network
##add "predicted" column to data
##separate out time of day and specific time frame for all sensors
##Average specific time of day over course of month
##Add in the distance to major roadway metrics


#####Create predictions for each sensor
AQ_predicted_files <- list.files(predicted_pm_dir)

aq_corr_alg_models <- list()

AQ_predicted_stats <- data.frame(matrix(ncol = 0, nrow = 1))

for(file in AQ_predicted_files){
  sensor_data <- read.csv(paste(predicted_pm_dir, file, sep = '/'))
  temp_data <- data.frame(matrix(ncol = 0, nrow = 1))
  temp_data$sensor_name <- file
  temp_data$ID <- sensor_data$ID[1]
  
  sensor_data$date <- as.POSIXct(sensor_data$date, tz="MST", format = '%Y-%m-%d %H:%M:%S')
  sensor_data <- subset(sensor_data, select = c("date", "ID","val.pm25_r",
                                                "pm25_fit", "hour", "weekend"))
  sensor_data <- unique(sensor_data)
  
  sensor_data <- sensor_data[sensor_data$date >= '2021-11-01 00:00:00' & sensor_data$date <= '2022-02-01 00:00:00',]
  temp_data$n_rows <- nrow(sensor_data)
  temp_data$avg_all <- mean(sensor_data$pm25_fit)
  temp_data$med_all <- median(sensor_data$pm25_fit)
  
  ##Data for morning on weekday
  sensor_data1 <- filter(sensor_data, between(hour, 7, 9))
  sensor_data1 <- filter(sensor_data1, weekend == 'weekday')
  temp_data$weekday_morning_avg <- mean(sensor_data1$pm25_fit)
  temp_data$weekday_morning_med <- median(sensor_data1$pm25_fit)
  
  ##Data for afternoon on weekday
  sensor_data2 <- filter(sensor_data, between(hour, 14, 16))
  sensor_data2 <- filter(sensor_data2, weekend == 'weekday')
  temp_data$weekday_afternoon_avg <- mean(sensor_data2$pm25_fit)
  temp_data$weekday_afternoon_med <- median(sensor_data2$pm25_fit)
  
  AQ_predicted_stats <- rbind(AQ_predicted_stats, temp_data)
}

sensor_data_spring <- read.csv("combine_AQ_pred_road_spring.csv")

sensor_nearest_road <- read.csv("sensor_nearest_road_dist.csv")
sensor_DOTI <- read.csv("sensor_nearest_road_dist_DOTI_subset.csv")
names(sensor_DOTI)[names(sensor_DOTI) == "ID.x"] <- "ID"

combine_AQ_pred_road <- merge(AQ_predicted_stats, sensor_nearest_road, by = 'ID')
combine_all <- merge(combine_AQ_pred_road, sensor_DOTI, by = 'ID')
combine_all_spring <- merge(sensor_data_spring, sensor_DOTI, by = 'ID')

write.csv(combine_all_spring, "combine_all_spring.csv")
combine_all_spring <- read.csv("combine_all_spring.csv")

combine_AQ_pred_road[combine_AQ_pred_road == 0] <- NA
combine_AQ_pred_road<-combine_AQ_pred_road[complete.cases(combine_AQ_pred_road),]

#ANOVA Analysis

oneway.morning <- aov(weekday_afternoon_avg ~ distances + TOT_DYFLOW, data = combine_all)
summary(oneway.morning)

oneway.neighborhood <- aov(avg_all ~ EMAT_SUBAR + Road.Name, data = combine_all)
summary(oneway.neighborhood)

oneway.facility <- aov(avg_all ~ ZONE, data = combine_all)
summary(oneway.facility)

oneway.afternoon <- aov(weekday_afternoon_avg ~ distances..m. + TOT_DYFLOW, data = combine_all)
summary(oneway.afternoon)

names(combine_AQ_pred_road)[names(combine_AQ_pred_road) == "distances..m."] <- "distance_road"

##May be a relationship between what road closest to and the weekday avg
one.way <- aov(weekday_morning_avg ~ Road.Name, data = combine_AQ_pred_road)
summary(one.way)

library(ggpmisc)
ggplot(combine_all_loc, aes(x=distances, y=weekday_morning_avg)) +
  geom_point(aes(color= Location)) +
  theme_bw()

ggplot(combine_AQ_pred_road, aes(x=distance_road, y=weekday_afternoon_avg)) +
  geom_point(aes(color= Road.Name)) +
  theme_bw()

temp_pm_data <- read.csv(paste(predicted_pm_dir, 'wide_Bruce Randolph (CS19).csv_LC_smoke.csv', sep = '/'))

write.csv(combine_all_loc, 'combine_all_fall.csv')

combine_all_loc$Location_EW[combine_all_loc$Location_EW == "North"] <- "East"

combine_all_loc <- read.csv('combine_all_fall.csv')
oneway.neighborhood <- aov(avg_all ~ Location_NS, data = combine_all_spring)
summary(oneway.neighborhood)
plot(oneway.neighborhood)

ggplot(combine_all_loc, aes(x=distances, y=avg_all)) +
  geom_point(aes(color= Road.Name)) +
  theme_bw()

combine_all_spring$near_road[combine_all_spring$X100m == 1] <- "Far"
combine_all_spring$near_road[combine_all_spring$X100m == 0] <- "Near"

combine_all_loc$near_road[combine_all_loc$X100m == 1] <- "Far"
combine_all_loc$near_road[combine_all_loc$X100m == 0] <- "Near"

ggplot(combine_all_spring, aes(x=near_road, y=avg_all)) +
  geom_boxplot() +
  theme_bw()

ggplot(combine_all_loc, aes(x=near_road, y=avg_all)) +
  geom_boxplot() +
  theme_bw()


ggplot(combine_all_loc, aes(x=distances, y=weekday_morning_avg)) +
  geom_point(aes(color= Location_NS)) +
  theme_bw()

#EMAT_SUBAR + 
##Filter hours
  ###Monthly average
  ###Morning 7am - 9am average
  ##Afternoon 2pm - 4pm average

##Filter weekend & weekday

##Create data frame linking averages and traffic data -- record ID so it can be linked

##Perform ANOVA Analysis

##CoD of all sensors





###Create Graph of various models for Columbian


