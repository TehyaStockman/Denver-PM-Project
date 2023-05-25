
install.packages("httr")
library(httr)
res <- GET("https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00204/data",authenticate("S0F9DIBVD4KI7RHTQI3J27W2", ""))
AQ_data<-content(res)
AQ_Data_1<-AQ_data$data
AQ_Data_1[1][[1]]$geo[1]
AQ_Data_1[1][[1]]$geo[2]
AQ_Data_1[1][[1]]$pm1
AQ_Data_1[3][[1]]$pm25
AQ_Data_1[1][[1]]$timestamp

names(AQ_data)
aq_data_frame <- data.frame(matrix(nrow = 0, ncol = 0))

for(i in 1:50){
  temp$Lat <- AQ_Data_1[i][[1]]$geo[1]
  temp$Long <- AQ_Data_1[i][[1]]$geo[2]
  temp$pm1 <- AQ_Data_1[i][[1]]$pm1
  temp$pm25 <- AQ_Data_1[i][[1]]$pm25
  temp$date <- AQ_Data_1[i][[1]]$timestamp
  
  aq_data_frame <- rbind(temp, aq_data_frame)
  
}




