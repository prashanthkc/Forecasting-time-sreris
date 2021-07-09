########################### PROBLEM 1 ##############################

library(readr)
airlines_data <- readxl::read_excel (file.choose()) # read the data
View(airlines_data) # Seasonality 12 months

# Pre Processing
# input t
airlines_data["t"] <- c(1:96)
View(airlines_data)

airlines_data["t_square"] <- airlines_data["t"] * airlines_data["t"]
airlines_data["log_Passengers"] <- log(airlines_data["Passengers"])


# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

airlines_dataPassengers <- cbind(airlines_data, X)
colnames(airlines_dataPassengers)

View(airlines_dataPassengers)
## Pre-processing completed

attach(airlines_dataPassengers)

# partitioning
train <- airlines_dataPassengers[1:70, ]
test <- airlines_dataPassengers[71:96, ]

########################### LINEAR MODEL #############################

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))

rmse_linear <- sqrt(mean((test$Passengers - linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential ############################

expo_model <- lm(log_Passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ###############################

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm = T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(Passengers ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata = test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add


######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Passengers ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata = test, interval = 'predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers - exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

################### Additive Seasonality with Quadratic Trend #################

Add_sea_Quad_model <- lm(Passengers ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear", "rmse_expo", "rmse_Quad", "rmse_sea_add", "rmse_Add_sea_Quad", "rmse_multi_sea"), c(rmse_linear, rmse_expo, rmse_Quad, rmse_sea_add, rmse_Add_sea_Quad, rmse_multi_sea))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

# Additive seasonality with Quadratic Trend has least RMSE value

write.csv(airlines_dataPassengers, file = "airlines_dataPassengers.csv", row.names = F)
getwd()

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = airlines_dataPassengers)
summary(Add_sea_Quad_model_final)

####################### Predicting new data #############################
library(xlsx)
test_data <- read.xlsx(file.choose(), 1)
View(test_data)
pred_new <- predict(Add_sea_Quad_model_final, newdata = test_data, interval = 'predict')
pred_new <- as.data.frame(pred_new)
pred_new$fit
plot(Add_sea_Quad_model_final)


############
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
?forecast
View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predicted_new_values <- pred_new$fit + future_errors

write.csv(predicted_new_values, file = "predicted_new_values.csv", row.names = F)
getwd()

############################### PROBLEM 2 ###################################

library(readr)
cocacola_data <- readxl::read_excel(file.choose()) # read the cocacola_data data
View(cocacola_data) # Seasonality 12 months

# Pre Processing
# input t
cocacola_data["t"] <- c(1:42)
View(cocacola_data)

cocacola_data["t_square"] <- cocacola_data["t"] * cocacola_data["t"]
cocacola_data["log_Sales"] <- log(cocacola_data["Sales"])


# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 42), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

cocacola_dataSales <- cbind(cocacola_data, X)
colnames(cocacola_dataSales)

View(cocacola_dataSales)
## Pre-processing completed

attach(cocacola_dataSales)

# partitioning
train <- cocacola_dataSales[1:30, ]
test <- cocacola_dataSales[31:42, ]

########################### LINEAR MODEL #############################

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))

rmse_linear <- sqrt(mean((test$Sales - linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential ############################

expo_model <- lm(log_Sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ###############################

Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm = T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(Sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata = test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add


######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata = test, interval = 'predict'))
rmse_multi_sea <- sqrt(mean((test$Sales - exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

################### Additive Seasonality with Quadratic Trend #################

Add_sea_Quad_model <- lm(Sales ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear", "rmse_expo", "rmse_Quad", "rmse_sea_add", "rmse_Add_sea_Quad", "rmse_multi_sea"), c(rmse_linear, rmse_expo, rmse_Quad, rmse_sea_add, rmse_Add_sea_Quad, rmse_multi_sea))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

# Additive seasonality with Quadratic Trend has least RMSE value

write.csv(cocacola_dataSales, file = "cocacola_dataSales.csv", row.names = F)
getwd()

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = cocacola_dataSales)
summary(Add_sea_Quad_model_final)

####################### Predicting new data #############################
library(xlsx)
test_data <- read.xlsx(file.choose(), 1)
View(test_data)
pred_new <- predict(Add_sea_Quad_model_final, newdata = test_data, interval = 'predict')
pred_new <- as.data.frame(pred_new)
pred_new$fit
plot(Add_sea_Quad_model_final)


############
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
?forecast
View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predicted_new_values <- pred_new$fit + future_errors

write.csv(predicted_new_values, file = "predicted_new_values.csv", row.names = F)
getwd()

################################## PROBLEM 3 ################################

library(readr)
library(readxl)
plastic_data <- read.csv(file.choose()) # read the plastic_data data
View(plastic_data) # Seasonality 12 months

# Pre Processing
# input t
plastic_data["t"] <- c(1:60)
View(plastic_data)

plastic_data["t_square"] <- plastic_data["t"] * plastic_data["t"]
plastic_data["log_Sales"] <- log(plastic_data["Sales"])


# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

plastic_dataSales <- cbind(plastic_data, X)
colnames(plastic_dataSales)

View(plastic_dataSales)
## Pre-processing completed

attach(plastic_dataSales)

# partitioning
train <- plastic_dataSales[1:40, ]
test <- plastic_dataSales[41:60, ]

########################### LINEAR MODEL #############################

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))

rmse_linear <- sqrt(mean((test$Sales - linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential ############################

expo_model <- lm(log_Sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ###############################

Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm = T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(Sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata = test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add


######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata = test, interval = 'predict'))
rmse_multi_sea <- sqrt(mean((test$Sales - exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

################### Additive Seasonality with Quadratic Trend #################

Add_sea_Quad_model <- lm(Sales ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear", "rmse_expo", "rmse_Quad", "rmse_sea_add", "rmse_Add_sea_Quad", "rmse_multi_sea"), c(rmse_linear, rmse_expo, rmse_Quad, rmse_sea_add, rmse_Add_sea_Quad, rmse_multi_sea))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

# Additive seasonality with Quadratic Trend has least RMSE value

write.csv(plastic_dataSales, file = "plastic_dataSales.csv", row.names = F)
getwd()

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = plastic_dataSales)
summary(Add_sea_Quad_model_final)

####################### Predicting new data #############################
library(xlsx)
test_data <- read.xlsx(file.choose(), 1)
View(test_data)
pred_new <- predict(Add_sea_Quad_model_final, newdata = test_data, interval = 'predict')
pred_new <- as.data.frame(pred_new)
pred_new$fit
plot(Add_sea_Quad_model_final)


############
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
?forecast
View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predicted_new_values <- pred_new$fit + future_errors

write.csv(predicted_new_values, file = "predicted_new_values.csv", row.names = F)
getwd()

################################### PROBLEM 4 #################################

library(readr)
solar_data<- read.csv(file.choose()) # read the solar_data data
View(solar_data) # Seasonality 12 months

# Pre Processing
# input t
solar_data["t"] <- c(1:2558)
View(solar_data)

solar_data["t_square"] <- solar_data["t"] * solar_data["t"]
solar_data["log_cum_power"] <- log(solar_data["cum_power"])


# So creating 12 dummy variables
X <- data.frame(outer(rep(month.abb,length = 2558), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names
View(X)

solar_datacum_power <- cbind(solar_data, X)
colnames(solar_datacum_power)

View(solar_datacum_power)
## Pre-processing completed

attach(solar_datacum_power)

# partitioning
train <- solar_datacum_power[1:1850, ]
test <- solar_datacum_power[1851:2558, ]

########################### LINEAR MODEL #############################

linear_model <- lm(cum_power ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = test))

rmse_linear <- sqrt(mean((test$cum_power - linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential ############################

expo_model <- lm(log_cum_power ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
rmse_expo <- sqrt(mean((test$cum_power - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ###############################

Quad_model <- lm(cum_power ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$cum_power-Quad_pred$fit)^2, na.rm = T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(cum_power ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata = test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$cum_power - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add


######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_cum_power ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata = test, interval = 'predict'))
rmse_multi_sea <- sqrt(mean((test$cum_power - exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

################### Additive Seasonality with Quadratic Trend #################

Add_sea_Quad_model <- lm(cum_power ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = test))
rmse_Add_sea_Quad <- sqrt(mean((test$cum_power - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear", "rmse_expo", "rmse_Quad", "rmse_sea_add", "rmse_Add_sea_Quad", "rmse_multi_sea"), c(rmse_linear, rmse_expo, rmse_Quad, rmse_sea_add, rmse_Add_sea_Quad, rmse_multi_sea))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

# Additive seasonality with Quadratic Trend has least RMSE value

write.csv(solar_datacum_power, file = "solar_datacum_power.csv", row.names = F)
getwd()

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(cum_power ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = solar_datacum_power)
summary(Add_sea_Quad_model_final)

####################### Predicting new data #############################
