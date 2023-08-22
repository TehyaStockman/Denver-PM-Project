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

write.csv(all_corr_alg_models, 'algorithm_stats_780.csv')

all_correction_stats <- read.csv('algorithm_stats_updated2.csv')
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

##Evaluating sensors with corrections
ggplot(all_corr_alg_models, aes(x =EVAL_SITE, y =AVG)) + 
  geom_boxplot() +
  theme_bw()

rft_models_sensors <- read.csv("algorithm_stats_sensors_rft.csv")
ggplot(rft_models_sensors, aes(x =EVAL_SITE, y =AVG)) + 
  geom_boxplot() +
  theme_bw()

ggplot(rft_models_sensors, aes(x =EVAL_SITE, y =STDEV)) + 
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

sams_long <- subset(sams_data, select = c('date', 'val.pm25_p.y', 'val.pm25_r'))
sams_long <- melt(sams_long, id = 'date')

new_sams <- sams_data[sams_data$Ratio <= 0.2,]
sqrt(mean((sams_data$val.pm25_p.y - sams_data$val.pm25_r)^2))

sams_data$bias <- sams_data$val.pm25_p.y - sams_data$val.pm25_r
timePlot(sams_data, pollutant = c('val.pm25_p.y', 'val.pm25_r'))
cor(sams_data$val.pm25_p.y, sams_data$val.pm25_r)

ggplot(new_sams, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line(color = '#1B9E77') +
  stat_poly_eq(use_label(c('eq', 'R2'))) +
  geom_point(aes(color = Ratio)) +
  theme_bw()+
  xlab(bquote('SAMS Reference PM2.5' ~µg/m^3))+
  ylab(bquote('SAMS Sensor PM2.5' ~µg/m^3))
ggsave('Figures/sams_scatter.png')


ggplot(sams_long, aes(x=date, y=value)) +
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = 'Dark2')+
  ylab(bquote('SAMS PM2.5' ~µg/m^3))+
  theme_bw()
ggsave('Figures/sams_timeseries.png')

##I-25 Denver
i25Den_data <- read.csv(paste(new_alg_data_dir, 'I-25 Denver State Site.csv', sep = '/'))
i25Den_data2 <- filter(i25Den_data, flag == 1)
i25Den_data$date <- as.POSIXct(i25Den_data$date, tz = 'MST')

i25Den_long <- subset(i25Den_data, select = c('date', 'val.pm25_p.y', 'val.pm25_r'))
i25Den_long <- melt(i25Den_long, id = 'date')

new_den <- i25Den_data[i25Den_data$flag == 0,]

i25Den_data$bias <- i25Den_data$val.pm25_r - i25Den_data$val.pm25_p.y
mean(i25Den_data$bias)
sqrt(mean((i25Den_data$val.pm25_r - i25Den_data$val.pm25_p.y)^2))

ggplot(i25Den_data, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line(color = '#1B9E77') +
  stat_poly_eq(use_label(c('eq', 'R2'))) +
  geom_point(aes(color = age_weeks.x)) +
  theme_bw()+
  xlab(bquote('I-25 Denver Reference PM2.5' ~µg/m^3))+
  ylab(bquote('I-25 Denver Sensor PM2.5' ~µg/m^3))
ggsave('Figures/i25den_scatter.png')

ggplot(i25Den_long, aes(x=date, y=value)) +
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = 'Dark2')+
  ylab(bquote('I-25 Denver PM2.5' ~µg/m^3))+
  theme_bw()
ggsave('Figures/i25den_timeseries.png')


##CAMP
camp_data <- read.csv(paste(new_alg_data_dir, 'CAMP State Site.csv', sep = '/'))
camp_data <- filter(camp_data, flag == 1)
camp_data$date <- as.POSIXct(camp_data$date, tz = 'MST')
camp_data$bias <- camp_data$val.pm25_r -camp_data$val.pm25_p.y
mean(camp_data$bias)

new_camp <- camp_data[camp_data$date <= '2020-01-01 00:00:00',]
new_camp <- camp_data[camp_data$Ratio >= 0.8 & camp_data$date >= '2020-01-01 00:00:00',]
sqrt(mean((new_camp$val.pm25_p.y - new_camp$val.pm25_r)^2))
cor(camp_data$val.pm25_p.y,camp_data$val.pm25_r)
sqrt(mean((camp_data$val.pm25_p.y - camp_data$val.pm25_r)^2))

camp_long <- subset(camp_data, select = c('date', 'val.pm25_p.y', 'val.pm25_r'))
camp_long <- melt(camp_long, id = 'date')

ggplot(new_camp, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line(color = '#1B9E77') +
  stat_poly_eq(use_label(c('eq', 'R2'))) +
  geom_point(aes(color = Ratio)) +
  theme_bw()+
  xlab(bquote('CAMP Reference PM2.5' ~µg/m^3))+
  ylab(bquote('CAMP Sensor PM2.5' ~µg/m^3))
ggsave('Figures/camp_scatter.png')

ggplot(camp_long, aes(x=date, y=value)) +
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = 'Dark2')+
  ylab(bquote('CAMP PM2.5' ~µg/m^3))+
  theme_bw()
ggsave('Figures/camp_timeseries.png')

ggplot(camp_data, aes(x = date, y = bias)) +
  geom_line()+
  theme_bw()

i25Glo_data <- read.csv(paste(new_alg_data_dir, 'I-25 Globeville State Site.csv', sep = '/'))
i25Glo_data$date <- as.POSIXct(i25Glo_data$date, tz = 'MST', tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                            "%Y/%m/%d %H:%M:%OS",
                                                                            "%Y-%m-%d %H:%M",
                                                                            "%Y/%m/%d %H:%M",
                                                                            "%Y-%m-%d",
                                                                            "%Y/%m/%d"))
i25Glo_data$bias <- i25Glo_data$val.pm25_r -i25Glo_data$val.pm25_p.y
new_glo <-  i25Glo_data[i25Glo_data$flag == 1,] 
sqrt(mean((i25Glo_data$val.pm25_p.y - i25Glo_data$val.pm25_r)^2))

i25Glo_long <- subset(i25Glo_data, select = c('date', 'val.pm25_p.y', 'val.pm25_r'))
i25Glo_long <- melt(i25Glo_long, id = 'date')

ggplot(new_glo, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line(color = '#1B9E77') +
  stat_poly_eq(use_label(c('eq', 'R2'))) +
  geom_point(aes(color = age_weeks.x)) +
  theme_bw()+
  xlab(bquote('I-25 Globeville Reference PM2.5' ~µg/m^3))+
  ylab(bquote('I-25 Globeville Sensor PM2.5' ~µg/m^3))
ggsave('Figures/i25Glo_scatter.png')

ggplot(i25Glo_long, aes(x=date, y=value)) +
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = 'Dark2')+
  ylab(bquote('I-25 Globeville PM2.5' ~µg/m^3))+
  theme_bw()
ggsave('Figures/i25glo_timeseries.png')

i25Glo_stats <- aqStats(i25Glo_data, pollutant = 'val.pm25_p.y')

ggplot(i25Glo_data, aes(x = date, y = bias)) +
  geom_line()+
  theme_bw()

#La Casa
LaCasa_data <- read.csv(paste(new_alg_data_dir, 'La Casa State Site.csv', sep = '/'))
LaCasa_data$date <- as.POSIXct(LaCasa_data$date, tz = 'MST')

LaCasa_long <- subset(LaCasa_data, select = c('date', 'val.pm25_p.y', 'val.pm25_r'))
LaCasa_long <- melt(LaCasa_long, id = 'date')

new_casa <- LaCasa_data[LaCasa_data$Ratio <= 0.2 &LaCasa_data$Ratio > 0, ]

sqrt(mean((LaCasa_data$val.pm25_p.y - LaCasa_data$val.pm25_r)^2))
LaCasa_data$bias <- LaCasa_data$val.pm25_p.y - LaCasa_data$val.pm25_r 
  
ggplot(new_casa, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line(color = '#1B9E77') +
  stat_poly_eq(use_label(c('eq', 'R2'))) +
  geom_point(aes(color = Ratio)) +
  theme_bw()+
  xlab(bquote('La Casa Reference PM2.5' ~µg/m^3))+
  ylab(bquote('La Casa Sensor PM2.5' ~µg/m^3))
ggsave('Figures/Lacasa_scatter.png')

ggplot(LaCasa_long, aes(x=date, y=value)) +
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = 'Dark2')+
  ylab(bquote('La Casa PM2.5' ~µg/m^3))+
  theme_bw()
ggsave('Figures/Lacasa_timeseries.png')

ggplot(i25Casa_data, aes(x=date, y=Ratio)) +
  geom_line(color = '#ABD9E9') +
  theme_bw()

Lacasa_stats <- aqStats(i25Casa_data, pollutant = 'val.pm25_p.y')

#NJH
NJH_data <- read.csv(paste(new_alg_data_dir, 'National Jewish Health State Site.csv', sep = '/'))
NJH_data$date <- as.POSIXct(NJH_data$date, tz = 'MST')
NJH_data$bias <- NJH_data$val.pm25_p.y - NJH_data$val.pm25_r 
sqrt(mean((NJH_data$val.pm25_p.y - NJH_data$val.pm25_r)^2))

new_njh <- NJH_data[NJH_data$flag ==0,]

NJH_long <- subset(NJH_data, select = c('date', 'val.pm25_p.y', 'val.pm25_r'))
NJH_long <- melt(NJH_long, id = 'date')

ggplot(new_njh, aes(x=val.pm25_p.y, y=val.pm25_r)) +
  stat_poly_line(color = '#1B9E77') +
  stat_poly_eq(use_label(c('eq', 'R2'))) +
  geom_point(aes(color = age_weeks.x)) +
  theme_bw()+
  xlab(bquote('National Jewish Health Reference PM2.5' ~µg/m^3))+
  ylab(bquote('National Jewish Health Sensor PM2.5' ~µg/m^3))
ggsave('Figures/NJH_scatter.png')

ggplot(NJH_long, aes(x=date, y=value)) +
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = 'Dark2')+
  ylab(bquote('National Jewish Health PM2.5' ~µg/m^3))+
  theme_bw()
ggsave('Figures/NJH_timeseries.png')

ggplot(NJH_data, aes(x=date, y=Ratio)) +
  geom_line(color = '#ABD9E9') +
  theme_bw()

NJH_stats <- aqStats(NJH_data, pollutant = 'val.pm25_p.y')

##-------------------------------------------

