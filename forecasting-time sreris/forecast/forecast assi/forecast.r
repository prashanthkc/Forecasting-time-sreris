##############################Problem 1#########################
library(readxl)
airlines <- read_excel("C:\\Users\\hp\\Desktop\\forecast assi\\dataset\\Airlines Data\\Airlines Data.xlsx")
View(airlines) # Seasonality 12 months

# Pre Processing
# input t
airlines["t"] <- c(1:96)
View(airlines)

airlines["t_square"] <- airlines["t"] * airlines["t"]
airlines["log_passengers"] <- log(airlines["Passengers"])

# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

airlines1 <- cbind(airlines, X)
colnames(airlines1)

View(airlines1)
## Pre-processing completed

attach(airlines1)

# partitioning
train <- airlines1[1:76, ]
test <- airlines1[77:96, ]

########################### LINEAR MODEL

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))

rmse_linear <- sqrt(mean((test$Passengers - linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential

expo_model <- lm(log_passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm = T))
rmse_Quad

######################### Additive Seasonality

sea_add_model <- lm(Passengers ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata = test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Multiplicative Seasonality

multi_sea_model <- lm(log_passengers ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata = test, interval = 'predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers - exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

################### Additive Seasonality with Quadratic Trend

Add_sea_Quad_model <- lm(Passengers ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("linear_model", "expo_model", "Quad_model", "sea_add_model", "multi_sea_model","Add_sea_Quad_model"), c(rmse_linear, rmse_expo, rmse_Quad, rmse_sea_add, rmse_multi_sea,rmse_Add_sea_Quad))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

#Apply rmse_Add_sea_Quad model on full data since it has low rmse
Add_sea_Quad_model_final <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = airlines1)
summary(Add_sea_Quad_model_final)

####################### Predicting new data
library(readr)
test_data <- read_csv("C:\\Users\\hp\\Desktop\\forecast assi\\dataset\\Airlines Data\\new_data_Airlines Data.csv")
View(test_data)

# Pre Processing test data
# input t
test_data["t"] <- c(97:156)
View(test_data)

test_data["t_square"] <- test_data["t"] * test_data["t"]

# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

test_data1 <- cbind(test_data, X)
colnames(test_data1)

pred_new <- predict(Add_sea_Quad_model_final, newdata = test_data1, interval = 'predict')
pred_new <- as.data.frame(pred_new)
test_data1['predicted_passengers'] <- 0
test_data1['predicted_passengers'] <- pred_new$fit
plot(Add_sea_Quad_model_final)

#printing predicted passengers for next 5 years
test_data1[c('Month','predicted_passengers')]

# ACF plot
acf(Add_sea_Quad_model_final$residuals, lag.max = 12) # take all residual value of the model built & plot ACF plot

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1, 0, 0))
summary(A)
A$coef
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 12)

# predicting next 12 months errors using arima(order=c(1,0,0))
library(forecast)
errors_12 <- forecast(A, h = 12)
View(errors_12)

future_errors <- data.frame(errors_12)
future_errors <- future_errors$Point.Forecast
View(future_errors)

# predicted values for new data + future error values 
predicted_new_values <- pred_new$fit + future_errors
#so the predicted values including future errors are:
View(predicted_new_values)

#comparing predicted passengers for model with and without future errors
predicted_new_values <- as.data.frame(predicted_new_values)
colnames(predicted_new_values) <- 'predicted_passengers_with_future_error'
predicted_dataset <- cbind(test_data1[c('Month','predicted_passengers')], predicted_new_values)
View(predicted_dataset)
####################################problem 2###################################
library(readxl)
cocacola <- read_excel("C:\\Users\\hp\\Desktop\\forecast assi\\dataset\\Cocacola Data\\CocaCola_Sales_Rawdata.xlsx")
View(cocacola) # Seasonality 4 months

library(forecast)
#install.packages("fpp")
library(fpp)
#install.packages("smooth")
library(smooth) # for smoothing and MAPE
#install.packages("tseries")
library(tseries)

tssales <- ts(cocacola$Sales, frequency = 4, start = c(42))
View(tssales)

# dividing entire data into training and testing data 
train <- tssales[1:38]
test <- tssales[39:42]
# Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train <- ts(train, frequency = 4)
test <- ts(test, frequency = 4)

# Plotting time series data
plot(tssales)
# Visualization shows that it has level, trend, seasonality => Additive seasonality

#MOVING AVERAGE
ma_model1 <- sma(train)
ma_pred <- data.frame(predict(ma_model1, h = 4))
ma_pred

plot(forecast(ma_model1))
ma_mape <- MAPE(ma_pred$Point.Forecast, test)*100
ma_mape

#### USING HoltWinters function
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a <- HoltWinters(train, alpha = 0.2, beta = F, gamma = F)
hw_a
hwa_pred <- data.frame(predict(hw_a, n.ahead = 4))

# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a, h = 4))
hwa_mape <- MAPE(hwa_pred$fit, test)*100

# with alpha = 0.2, beta = 0.15
# Assuming time series data has level and trend parameter 
hw_ab <- HoltWinters(train, alpha = 0.2, beta = 0.15, gamma = F)
hw_ab
hwab_pred <- data.frame(predict(hw_ab, n.ahead = 4))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab, h = 4))
hwab_mape <- MAPE(hwab_pred$fit,test)*100

# with alpha = 0.2, beta = 0.15, gamma = 0.05 
# Assuming time series data has level,trend and seasonality 
hw_abg <- HoltWinters(train, alpha = 0.2, beta = 0.15, gamma = 0.05)
hw_abg
hwabg_pred <- data.frame(predict(hw_abg, n.ahead = 4))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg, h = 4))
hwabg_mape <- MAPE(hwabg_pred$fit, test)*100

# With out optimum values 
hw_na <- HoltWinters(train, beta = F, gamma = F)
hw_na
hwna_pred <- data.frame(predict(hw_na, n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape <- MAPE(hwna_pred$fit,test)*100

hw_nab <- HoltWinters(train, gamma = F)
hw_nab
hwnab_pred <- data.frame(predict(hw_nab, n.ahead = 4))
hwnab_pred
plot(forecast(hw_nab, h = 4))
hwnab_mape <- MAPE(hwnab_pred$fit, test)*100

hw_nabg <- HoltWinters(train)
hw_nabg
hwnabg_pred <- data.frame(predict(hw_nabg, n.ahead = 4))
hwnabg_pred
plot(forecast(hw_nabg, h = 4))
hwnabg_mape <- MAPE(hwnabg_pred$fit, test)*100

#display all MAPE values together for comparison
df_mape <- data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma

new_model <- HoltWinters(tssales)
new_model

plot(forecast(new_model, n.ahead = 4))

# Forecast values for the next 4 quarters
forecast_new <- data.frame(predict(new_model, n.ahead = 4))
forecast_new

##############USING ses,holt,hw functions
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 

ses_a<-ses(train,alpha = 0.2) # 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100

# with alpha = 0.2, beta = 0.1
holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100

# With out optimum values 
# simple exponential method
ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100

# Holts winter method 
holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100

# Holts winter Exponential method
hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100

#display all MAPE values together for comparison
df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 
new_model <- hw(tssales,alpha = NULL,beta = NULL,gamma = NULL)

plot(forecast(new_model,h=4))

# Forecasted values for the next 8 quarters (2 years)
forecast_new <- data.frame(predict(new_model,h=8))
#viewing next 2 years of Sales
View(forecast_new$Point.Forecast)

###################################Problem 3######################################
library(readr)
Plastic_data <- read_csv("C:\\Users\\hp\\Desktop\\forecast assi\\dataset\\PlasticSales Data\\PlasticSales.csv")
View(Plastic_data) # Seasonality 12 months

# Pre Processing
# input t
Plastic_data["t"] <- c(1:60)
View(Plastic_data)

Plastic_data["t_square"] <- Plastic_data["t"] * Plastic_data["t"]
Plastic_data["log_Sales"] <- log(Plastic_data["Sales"])

# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

Plastic_data1 <- cbind(Plastic_data, X)
colnames(Plastic_data1)

View(Plastic_data1)
## Pre-processing completed

attach(Plastic_data1)

# partitioning
train <- Plastic_data1[1:48, ]
test <- Plastic_data1[49:60, ]

########################### LINEAR MODEL

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))

rmse_linear <- sqrt(mean((test$Sales - linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential

expo_model <- lm(log_Sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic

Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm = T))
rmse_Quad

######################### Additive Seasonality

sea_add_model <- lm(Sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata = test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Multiplicative Seasonality

multi_sea_model <- lm(log_Sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata = test, interval = 'predict'))
rmse_multi_sea <- sqrt(mean((test$Sales - exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

################### Additive Seasonality with Quadratic Trend

Add_sea_Quad_model <- lm(Sales ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("linear_model", "expo_model", "Quad_model", "sea_add_model", "multi_sea_model","Add_sea_Quad_model"), c(rmse_linear, rmse_expo, rmse_Quad, rmse_sea_add, rmse_multi_sea,rmse_Add_sea_Quad))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

#Apply rmse_Add_sea_Quad model on full data since it has low rmse
Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Plastic_data1)
summary(Add_sea_Quad_model_final)

####################### Predicting new data
library(readr)
test_data <- read_csv("C:\\Users\\hp\\Desktop\\forecast assi\\dataset\\PlasticSales Data\\New_data_PlasticSales.csv")
View(test_data)

# Pre Processing test data
# input t
test_data["t"] <- c(61:72)
View(test_data)

test_data["t_square"] <- test_data["t"] * test_data["t"]

# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 12), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

test_data1 <- cbind(test_data, X)
colnames(test_data1)

pred_new <- predict(Add_sea_Quad_model_final, newdata = test_data1, interval = 'predict')
pred_new <- as.data.frame(pred_new)
test_data1['predicted_Sales'] <- 0
test_data1['predicted_Sales'] <- pred_new$fit
plot(Add_sea_Quad_model_final)

#printing predicted passengers for next year
test_data1[c('Month','predicted_Sales')]

# ACF plot
acf(Add_sea_Quad_model_final$residuals, lag.max = 12) # take all residual value of the model built & plot ACF plot

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1, 0, 0))
summary(A)
A$coef
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 12)

# predicting next 12 months errors using arima(order=c(1,0,0))
library(forecast)
errors_12 <- forecast(A, h = 12)
View(errors_12)

future_errors <- data.frame(errors_12)
future_errors <- future_errors$Point.Forecast
View(future_errors)

# predicted values for new data + future error values 
predicted_new_values <- pred_new$fit + future_errors
#so the predicted values including future errors are:
View(predicted_new_values)

#comparing predicted Sales for model with and without future errors
predicted_new_values <- as.data.frame(predicted_new_values)
colnames(predicted_new_values) <- 'predicted_Sales_with_future_error'
predicted_dataset <- cbind(test_data1[c('Month','predicted_Sales')], predicted_new_values)
View(predicted_dataset)

###########################################Problem 4##############################
library(readr)
Solarpower <- read_csv("C:\\Users\\hp\\Desktop\\forecast assi\\dataset\\SolarPower Data\\solarpower_cumuldaybyday2.csv")
View(Solarpower) # Seasonality 12 months

#converting cummulative power to non-commulative power
Solarpower$power <- diff(c(0,Solarpower$cum_power))

# Pre Processing
# input t
Solarpower["t"] <- c(1:2558)
View(Solarpower)

Solarpower["t_square"] <- Solarpower["t"] * Solarpower["t"]
Solarpower["log_Power"] <- log(Solarpower["power"])

#dropping cum_power column
Solarpower <- Solarpower[-c(2)]

# So creating 12 dummy variables
# creates a new my_months column, with format YYYY-MM, which can be later used for grouping
library(lubridate)
Solarpower$month <- month(as.POSIXlt(Solarpower$date, format="%d/%m/%Y"))

library(dummies)
X <- dummy(Solarpower$month)# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

#combining 2 df
Solarpower1 <- cbind(Solarpower, X)
colnames(Solarpower1)

View(Solarpower1)
## Pre-processing completed

attach(Solarpower1)

# partitioning
train <- Solarpower1[1:2046, ]
test <- Solarpower1[2047:2558, ]

########################### LINEAR MODEL

linear_model <- lm(power ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))

rmse_linear <- sqrt(mean((test$power - linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Quadratic

Quad_model <- lm(power ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$power-Quad_pred$fit)^2, na.rm = T))
rmse_Quad

######################### Additive Seasonality

sea_add_model <- lm(power ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata = test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$power - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

################### Additive Seasonality with Quadratic Trend

Add_sea_Quad_model <- lm(power ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = test))
rmse_Add_sea_Quad <- sqrt(mean((test$power - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("linear_model", "Quad_model", "sea_add_model","Add_sea_Quad_model"), c(rmse_linear, rmse_Quad, rmse_sea_add,rmse_Add_sea_Quad))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

#Apply Add_sea_Quad model on full data since it has low rmse
Add_sea_Quad_model_final <- lm(power ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Solarpower1)
summary(Add_sea_Quad_model_final)

####################### Predicting new data
#predictiong for next 5 years
library(readr)
test_data <- read_csv("C:\\Users\\hp\\Desktop\\forecast assi\\dataset\\SolarPower Data\\new_data_solarpower_cumuldaybyday2.csv")
View(test_data)

# Pre Processing test data
test_data <- test_data[2559:3732,1]
# input t
test_data["t"] <- c(2559:3732)
View(test_data)

test_data["t_square"] <- test_data["t"] * test_data["t"]

# So creating 12 dummy variables
test_data$month <- month(as.POSIXlt(test_data$date, format="%d-%m-%Y"))

library(dummies)
X <- dummy(test_data$month)# Creating dummies for 12 months
X <- X[,-c(13)]
colnames(X) <- month.abb # Assigning month namesView(X)

test_data1 <- cbind(test_data, X)
colnames(test_data1)

#dropping month data
test_data1 <- test_data1[,-c(4)]

pred_new <- predict(Add_sea_Quad_model_final, newdata = test_data1, interval = 'predict')
pred_new <- as.data.frame(pred_new)
test_data1['predicted_power'] <- 0
test_data1['predicted_power'] <- pred_new$fit
plot(Add_sea_Quad_model_final)

#printing predicted passengers for next 5 years
test_data1[c('date','predicted_power')]

# ACF plot
acf(Add_sea_Quad_model_final$residuals, lag.max = 12) # take all residual value of the model built & plot ACF plot

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1, 0, 0))
summary(A)
A$coef
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 12)

# predicting next 12 months errors using arima(order=c(1,0,0))
library(forecast)
errors_12 <- forecast(A, h = 12)
View(errors_12)

future_errors <- data.frame(errors_12)
future_errors <- future_errors$Point.Forecast
View(future_errors)

# predicted values for new data + future error values 
predicted_new_values <- pred_new$fit + future_errors
#so the predicted values including future errors are:
View(predicted_new_values)

#comparing predicted passengers for model with and without future errors
predicted_new_values <- as.data.frame(predicted_new_values)
colnames(predicted_new_values) <- 'predicted_power_with_future_error'
predicted_dataset <- cbind(test_data1[c('date','predicted_power')], predicted_new_values)
View(predicted_dataset)

#plotting time series data
pred_power <- ts(predicted_dataset$predicted_power , start = c(500))
plot(pred_power)


