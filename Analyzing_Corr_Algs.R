##Script for making plots and analyzing PM2.5 correction algorithms


library(openair)


#Make sure to initialize environment first
#Load in alg_stats_of_sites
#Load in Data_from_alg

#Stats for algorithms
alg_stats_of_sites <- read.csv("algorithm_stats.csv", header = TRUE)

#Bring in raw data used to create algorithms
alg_data_files <- list.files(alg_data_dir)


for(aq_file in alg_data_files){
  temp_data <- read.csv(paste(alg_data_dir, aq_file, sep = '/'), header = TRUE)
  temp_data$date <- as.POSIXct(temp_data$date, tz="MST", format = '%Y-%m-%d %H:%M')
  
  temp_data$val.pm25_p.y <- replace(temp_data$val.pm25_p.y, temp_data$val.pm25_p.y < -5, NA)
  temp_data <- na.omit(temp_data)
    
  timePlot(temp_data, pollutant = c('val.pm25_r', 'val.pm25_p.y'),
           y.relation = "free")
  print(aq_file)
  write.csv(temp_data, paste(alg_data_dir, aq_file, sep = '/'))
  
}

#making scatter plots by year
getwd()
years_list <- c(2019, 2020, 2021, 2022)
raw_stat_cnames <- c('ID', 'COR', 'RMSE', 'MAE', 'BIAS') 

raw_stats <- data.frame(matrix(ncol = 0, nrow = 1))
raw_stats_all_sites <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(raw_stats_all_sites) <- raw_stat_cnames


raw_stats <- data.frame(matrix(ncol = 0, nrow = 1))
raw_stats_all_sites <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(raw_stats_all_sites) <- raw_stat_cnames 


for(aq_file in alg_data_files){
  
  temp_data <- read.csv(paste(alg_data_dir, aq_file  , sep = '/'), header = TRUE)
  temp_data$date <- as.POSIXct(temp_data$date, tz="MST", format = '%Y-%m-%d %H:%M')
  print(aq_file)
  raw_stats <- data.frame(matrix(ncol = 0, nrow = 1))

  
  for(year_i in years_list){
    
    aq_site <- gsub('.csv', '', aq_file)
    filename <- paste(aq_site, year_i, '_scatterplot.png', sep = '')
    png(file = filename)
    
    
    temp_data_year <- selectByDate(temp_data, year = year_i)
    scatterPlot(temp_data_year, x = 'val.pm25_p.y', y = 'val.pm25_r')
    dev.off()

    
    # raw_stats$ID <- paste(aq_file, year_i, sep = ' ')
    # raw_stats$COR <- cor(temp_data_year$val.pm25_p.y, temp_data_year$val.pm25_r)
    # raw_stats$RMSE <- rmse(temp_data_year$val.pm25_p.y, temp_data_year$val.pm25_r)
    # raw_stats$MAE <- mae(temp_data_year$val.pm25_p.y, temp_data_year$val.pm25_r)
    # raw_stats$BIAS <- bias(temp_data_year$val.pm25_p.y, temp_data_year$val.pm25_r)
    # 
    # raw_stats_all_sites <- rbind(raw_stats_all_sites, raw_stats)
  }
  
}
write_excel_csv(raw_stats_all_sites, 'raw_stats_all_sites.xls')

#subsetting data by year

#Plot each set of data by year -- pair of raw data from sensor and FEM
#Trend plots of each
#Time series for each
  ##Bias/change over time?

#Using the alg stats of site table
#Box and whisker plots -- Correlation Coefficients
  ##By monitor
  ##By lm vs rft
  ##By variables

#Go through entire table and sort into the following (alg type, variables, site name) using a for loop
#make column of algorithm type

#make column of variables included

#make column of site name

#Create box and whisker plots by these categories above using for loop




