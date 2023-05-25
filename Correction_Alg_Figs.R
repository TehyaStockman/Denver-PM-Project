#Analyzing all corrections

library(ggplot2)
library(openair)
install.packages('installr')
library(installr)
updateR()

#read in alg_stats_of_site data table


#find lm and rft in string
#if statement to determine lm and rft
#separate string for everything before lm/rft and after

#ref_monitor_list <- c('La Casa State Site', 'National Jewish Health State Site', )

#save alg list of coefficients

write.csv(all_corr_alg_models, 'algorithm_stats.csv')

all_correction_stats <- read.csv('algorithm_stats.csv')
#remove .csv

##-----------------------------------------
##create box and whisker plots
#plotting pearson? Bias? CoD? -- each of the three sortings below with each of the types of plots
#pearson: want avg. closest to 1, Bias, want average closest to 0, CoD -- want average closes to 0

#sort by type of algorithm/variables
#sort by site

#Based on evaluation site
ggplot(all_corr_alg_models, aes(x =EVAL_SITE, y =COR)) + 
  geom_boxplot() +
  theme_bw()

ggplot(all_corr_alg_models, aes(x =EVAL_SITE, y =RMSE)) + 
  geom_boxplot() +
  theme_bw()

ggplot(all_corr_alg_models, aes(x =EVAL_SITE, y =BIAS)) + 
  geom_boxplot() +
  theme_bw()

#Based on correction algorithm created site
#Need to remove and separate after lm/rft
ggplot(all_corr_alg_models, aes(x =CORR_SITE, y =COR)) + 
  geom_boxplot() +
  theme_bw()

ggplot(all_corr_alg_models, aes(x =CORR_SITE, y =RMSE)) + 
  geom_boxplot() +
  theme_bw()

ggplot(all_corr_alg_models, aes(x =CORR_SITE, y =BIAS)) + 
  geom_boxplot() +
  theme_bw()

#sort by type of algorithm


##------------------------------------------
##Overarching stats



##------------------------------------------
##Heat Maps
#Pearson Correlation
#Bias
#Coefficient of Divergence




##------------------------------------------
#Scatter plots: pm25 sensor vs monitor
#Choose sensor with previous correction and then see what current corrections look like as well
#Showing bifurcation in data?



##------
#Time series
#Use same sensor/monitor pair to create time series -- time series includes ref monitor and sensor on same plot
#Create a few and choose which one to add

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 11, name = "RdYlBu")

install.packages('ggpmisc')
library(ggpmisc)

##SAMS
sams_data <- read.csv(paste(new_alg_data_dir, 'Swansea GRIMM.csv', sep = '/'))
sams_data$date <- as.POSIXct(sams_data$date, tz = 'MST')

timePlot(sams_data, pollutant = c('val.pm25_p.y', 'val.pm25_r'))

ggplot(sams_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_bw()

ggplot(sams_data, aes(x=date, y=val.pm25_r)) +
  geom_line(color = '#ABD9E9') +
  geom_line(aes(x = date, y= val.pm25_p.y), color = '#F46D43') +
  theme_bw()

##I-25 Denver
i25Den_data <- read.csv(paste(new_alg_data_dir, 'I-25 Denver State Site.csv', sep = '/'))
i25Den_data$date <- as.POSIXct(i25Den_data$date, tz = 'MST')

ggplot(i25Den_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_bw()

ggplot(i25Den_data, aes(x=date, y=val.pm25_r)) +
  geom_line(color = '#ABD9E9') +
  geom_line(aes(x = date, y= val.pm25_p.y), color = '#F46D43') +
  theme_bw()

ggplot(i25Den_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  geom_point() +
  theme_bw()

##CAMP
camp_data <- read.csv(paste(new_alg_data_dir, 'CAMP State Site.csv', sep = '/'))
camp_data$date <- as.POSIXct(camp_data$date, tz = 'MST')

ggplot(camp_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_bw()

ggplot(camp_data, aes(x=date, y=val.pm25_r)) +
  geom_line(color = '#ABD9E9') +
  geom_line(aes(x = date, y= val.pm25_p.y), color = '#F46D43') +
  theme_minimal()


#I25 Globeville

i25Glo_data <- read.csv(paste(new_alg_data_dir, 'I-25 Globeville State Site.csv', sep = '/'))
i25Glo_data$date <- as.POSIXct(i25Glo_data$date, tz = 'MST', tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                            "%Y/%m/%d %H:%M:%OS",
                                                                            "%Y-%m-%d %H:%M",
                                                                            "%Y/%m/%d %H:%M",
                                                                            "%Y-%m-%d",
                                                                            "%Y/%m/%d"))

ggplot(i25Glo_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_bw()

ggplot(i25Glo_data, aes(x=date, y=val.pm25_r)) +
  geom_line(color = '#ABD9E9') +
  geom_line(aes(x = date, y= val.pm25_p.y), color = '#F46D43') +
  theme_bw()

i25Glo_stats <- aqStats(i25Glo_data, pollutant = 'val.pm25_p.y')

#La Casa
i25Casa_data <- read.csv(paste(new_alg_data_dir, 'La Casa State Site.csv', sep = '/'))
i25Casa_data$date <- as.POSIXct(i25Casa_data$date, tz = 'MST')

ggplot(i25Casa_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_bw()

ggplot(i25Casa_data, aes(x=date, y=val.pm25_r)) +
  geom_line(color = '#ABD9E9') +
  geom_line(aes(x = date, y= val.pm25_p.y), color = '#F46D43') +
  theme_bw()

ggplot(i25Casa_data, aes(x=date, y=Ratio)) +
  geom_line(color = '#ABD9E9') +
  theme_bw()

Lacasa_stats <- aqStats(i25Casa_data, pollutant = 'val.pm25_p.y')

#NJH
NJH_data <- read.csv(paste(new_alg_data_dir, 'National Jewish Health State Site.csv', sep = '/'))
NJH_data$date <- as.POSIXct(NJH_data$date, tz = 'MST')

ggplot(NJH_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_bw()

ggplot(NJH_data, aes(x=date, y=val.pm25_r)) +
  geom_line(color = '#ABD9E9') +
  geom_line(aes(x = date, y= val.pm25_p.y), color = '#F46D43') +
  theme_bw()

ggplot(NJH_data, aes(x=date, y=Ratio)) +
  geom_line(color = '#ABD9E9') +
  theme_bw()

NJH_stats <- aqStats(NJH_data, pollutant = 'val.pm25_p.y')

##-------------------------------------------

