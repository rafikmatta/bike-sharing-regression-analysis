library(readr)
library("MASS")
library(corrplot)

hour <- read_csv("C:/Users/rafik/Google Drive/Education/ds8002/ds8002project/Bike-Sharing-Dataset/hour.csv")

#pick interesting features of bike data
bike_share_data <- data.frame(hour$weathersit, 
                              hour$temp, 
                              hour$windspeed, 
                              hour$casual, 
                              hour$cnt)

#plot histos
hist(bike_share_data$hour.weathersit)
hist(bike_share_data$hour.temp)
hist(bike_share_data$hour.casual)
hist(bike_share_data$hour.cnt)
hist(bike_share_data$hour.windspeed)

#report stats
mean(bike_share_data$hour.weathersit)
median(bike_share_data$hour.weathersit)
var(bike_share_data$hour.weathersit)

mean(bike_share_data$hour.temp)
median(bike_share_data$hour.temp)
var(bike_share_data$hour.temp)

mean(bike_share_data$hour.casual)
median(bike_share_data$hour.casual)
var(bike_share_data$hour.casual)

mean(bike_share_data$hour.cnt)
median(bike_share_data$hour.cnt)
var(bike_share_data$hour.cnt)

mean(bike_share_data$hour.windspeed)
median(bike_share_data$hour.windspeed)
var(bike_share_data$hour.windspeed)

#correlation matrix
bike_share_matrix <- data.matrix(bike_share_data)
corr_matrix <- cor(bike_share_matrix)

corrplot(corr_matrix)

#prepare data
data_size = length(bike_share_data$hour.weathersit)
train_val = .8*data_size
test_val = train_val + 1

response_var_train <- bike_share_data$hour.cnt[1:train_val]
response_var_test <- bike_share_data$hour.cnt[test_val:data_size]

bike_share_data <- data.frame(rep(1,length(hour$weathersit)),hour$weathersit, hour$temp, hour$windspeed, hour$casual)
bike_share_matrix <- data.matrix(bike_share_data)

bike_share_matrix_train <- bike_share_matrix[1:train_val,]
bike_share_matrix_test <- bike_share_matrix[test_val:data_size,]

#there's a moderate correlation between temperature and casual users as well as total count of users
#there's a strong correlation between casual and total count of users
#assuming we can treat weather situation as an ordered variable the correlation between that and other's is weak

#MV Linear Reg


#train
weights = ginv(t(bike_share_matrix_train) %*% bike_share_matrix_train) %*% t(bike_share_matrix_train) %*% response_var_train

#test
response_test = vector()
for(i in 1:length(response_var_test))
{
  response_test[i] = weights[1] + bike_share_matrix_test[i,2]*weights[2] + bike_share_matrix_test[i,3]*weights[3] + bike_share_matrix_test[i,4]*weights[4] + bike_share_matrix_test[i,5]*weights[5]
}

response_test
response_test - response_var_test
