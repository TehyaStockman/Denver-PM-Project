### La Casa Clarity vs Lunar

###Comparing La Casa Monitors with each other

la_casa_clarity <- read.csv(paste(aq_data_dir, 'Data_By_Sensor_Wide', 'wide_La Casa Collo_A0051F54_1.1.csv', sep = '/'))
#la_casa_clarity <- read.csv(paste(aq_data_dir, 'Data_By_Sensor', 'La Casa Collo (Clarity).csv', sep = '/'))

la_casa_lunar <- read.csv(paste(aq_data_dir, 'Data_By_Sensor_Wide', 'wide_La Casa Collo (CS5).csv', sep = '/'))

la_casa_together <- merge(la_casa_clarity, la_casa_lunar, by = 'date')

la_casa_subset <- subset(la_casa_together, select = c(val.pm25_p.x, val.pm25_r))
library(ggpmisc)
library(ggplot2)

ggplot(la_casa_subset, aes(x=val.pm25_r, y=val.pm25_p.x)) +
  stat_poly_line(color = '#1B9E77') +
  stat_poly_eq(use_label(c('eq', 'R2'))) +
  geom_point() +
  theme_bw()+
  xlab(bquote('PM2.5 from Lunar Outpost Sensor' ~µg/m^3))+
  ylab(bquote('PM2.5 from Clarity Sensor' ~µg/m^3))
ggsave('Figures/camp_scatter.png')