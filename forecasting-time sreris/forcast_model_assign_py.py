# -*- coding: utf-8 -*-
"""
Created on Sat Apr 17 23:29:43 2021

@author: prashanth
"""

############################# PROBLEM 1 ################################

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.holtwinters import SimpleExpSmoothing # SES
from statsmodels.tsa.holtwinters import Holt # Holts Exponential Smoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing # 
# from datetime import datetime

airlines_data= pd.read_excel("F:/assignment/forecasting-time sreris/Datasets_Forecasting/Airlines Data.xlsx")

airlines_data.Passengers.plot() # time series plot 

# Centering moving average for the time series
airlines_data.Passengers.plot(label = "org")
for i in range(2, 9, 2):
    airlines_data["Passengers"].rolling(i).mean().plot(label = str(i))
plt.legend(loc = 3)
    
# Time series decomposition plot 
decompose_ts_add = seasonal_decompose(airlines_data.Passengers, model = "additive", period = 4)
decompose_ts_add.plot()
decompose_ts_mul = seasonal_decompose(airlines_data.Passengers, model = "multiplicative", period = 4)
decompose_ts_mul.plot()

# ACF plot on Original data sets 
import statsmodels.graphics.tsaplots as tsa_plots
tsa_plots.plot_acf(airlines_data.Passengers, lags = 4)
# tsa_plots.plot_pacf(airlines_data.Passengers, lags=4)

# splitting the data into Train and Test data
# Recent 4 time period values are Test data

Train = airlines_data.head(90)
Test = airlines_data.tail(6)

# to change the index value in pandas data frame 
# Test.set_index(np.arange(1,4),inplace=True)

# Creating a function to calculate the MAPE value for test data 
def MAPE(pred,org):
    temp = np.abs((pred-org)/org)*100
    return np.mean(temp)


# Simple Exponential Method
ses_model = SimpleExpSmoothing(Train["Passengers"]).fit()
pred_ses = ses_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_ses, Test.Passengers) 

# Holt method 
hw_model = Holt(Train["Passengers"]).fit()
pred_hw = hw_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hw, Test.Passengers) 

# Holts winter exponential smoothing with additive seasonality and additive trend
hwe_model_add_add = ExponentialSmoothing(Train["Passengers"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()
pred_hwe_add_add = hwe_model_add_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_add_add, Test.Passengers) 

# Holts winter exponential smoothing with multiplicative seasonality and additive trend
hwe_model_mul_add = ExponentialSmoothing(Train["Passengers"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()
pred_hwe_mul_add = hwe_model_mul_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_mul_add, Test.Passengers) 


# Final Model on 100% Data
hwe_model_add_add = ExponentialSmoothing(airlines_data["Passengers"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()

# Load the new data which includes the entry for future 4 values
new_data = pd.read_excel("F:/assignment/forecasting-time sreris/Datasets_Forecasting/airline_pridict_data.xlsx")

newdata_pred = hwe_model_add_add.predict(start = new_data.index[0], end = new_data.index[-1])
newdata_pred

####################################### PROBLEM 2 #############################

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.holtwinters import SimpleExpSmoothing # SES
from statsmodels.tsa.holtwinters import Holt # Holts Exponential Smoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing # 
# from datetime import datetime

cocacola = pd.read_excel("F:/assignment/forecasting-time sreris/Datasets_Forecasting/CocaCola_Sales_Rawdata.xlsx")

cocacola.Sales.plot() # time series plot 

# Centering moving average for the time series
cocacola.Sales.plot(label = "org")
for i in range(2, 9, 2):
    cocacola["Sales"].rolling(i).mean().plot(label = str(i))
plt.legend(loc = 3)
    
# Time series decomposition plot 
decompose_ts_add = seasonal_decompose(cocacola.Sales, model = "additive", period = 4)
decompose_ts_add.plot()
decompose_ts_mul = seasonal_decompose(cocacola.Sales, model = "multiplicative", period = 4)
decompose_ts_mul.plot()

# ACF plot on Original data sets 
import statsmodels.graphics.tsaplots as tsa_plots
tsa_plots.plot_acf(cocacola.Sales, lags = 4)
# tsa_plots.plot_pacf(cocacola.Sales, lags=4)

# splitting the data into Train and Test data
# Recent 4 time period values are Test data

Train = cocacola.head(38)
Test = cocacola.tail(4)

# to change the index value in pandas data frame 
# Test.set_index(np.arange(1,4),inplace=True)

# Creating a function to calculate the MAPE value for test data 
def MAPE(pred,org):
    temp = np.abs((pred-org)/org)*100
    return np.mean(temp)


# Simple Exponential Method
ses_model = SimpleExpSmoothing(Train["Sales"]).fit()
pred_ses = ses_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_ses, Test.Sales) 

# Holt method 
hw_model = Holt(Train["Sales"]).fit()
pred_hw = hw_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hw, Test.Sales) 

# Holts winter exponential smoothing with additive seasonality and additive trend
hwe_model_add_add = ExponentialSmoothing(Train["Sales"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()
pred_hwe_add_add = hwe_model_add_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_add_add, Test.Sales) 

# Holts winter exponential smoothing with multiplicative seasonality and additive trend
hwe_model_mul_add = ExponentialSmoothing(Train["Sales"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()
pred_hwe_mul_add = hwe_model_mul_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_mul_add, Test.Sales) 


# Final Model on 100% Data
hwe_model_add_add = ExponentialSmoothing(cocacola["Sales"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()

# Load the new data which includes the entry for future 4 values
new_data = pd.read_excel("F:/assignment/forecasting-time sreris/Datasets_Forecasting/CocaCola_pridict_data.xlsx")

newdata_pred = hwe_model_add_add.predict(start = new_data.index[0], end = new_data.index[-1])
newdata_pred


############################################ PROBLEM 3 ###############################

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.holtwinters import SimpleExpSmoothing # SES
from statsmodels.tsa.holtwinters import Holt # Holts Exponential Smoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing # 
# from datetime import datetime

plastic_data = pd.read_csv("F:/assignment/forecasting-time sreris/Datasets_Forecasting/PlasticSales.csv")

plastic_data.Sales.plot() # time series plot 

# Centering moving average for the time series
plastic_data.Sales.plot(label = "org")
for i in range(2, 9, 2):
    plastic_data["Sales"].rolling(i).mean().plot(label = str(i))
plt.legend(loc = 3)
    
# Time series decomposition plot 
decompose_ts_add = seasonal_decompose(plastic_data.Sales, model = "additive", period = 4)
decompose_ts_add.plot()
decompose_ts_mul = seasonal_decompose(plastic_data.Sales, model = "multiplicative", period = 4)
decompose_ts_mul.plot()

# ACF plot on Original data sets 
import statsmodels.graphics.tsaplots as tsa_plots
tsa_plots.plot_acf(plastic_data.Sales, lags = 4)
# tsa_plots.plot_pacf(plastic_data.Sales, lags=4)

# splitting the data into Train and Test data
# Recent 4 time period values are Test data

Train = plastic_data.head(50)
Test = plastic_data.tail(10)

# to change the index value in pandas data frame 
# Test.set_index(np.arange(1,4),inplace=True)

# Creating a function to calculate the MAPE value for test data 
def MAPE(pred,org):
    temp = np.abs((pred-org)/org)*100
    return np.mean(temp)


# Simple Exponential Method
ses_model = SimpleExpSmoothing(Train["Sales"]).fit()
pred_ses = ses_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_ses, Test.Sales) 

# Holt method 
hw_model = Holt(Train["Sales"]).fit()
pred_hw = hw_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hw, Test.Sales) 

# Holts winter exponential smoothing with additive seasonality and additive trend
hwe_model_add_add = ExponentialSmoothing(Train["Sales"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()
pred_hwe_add_add = hwe_model_add_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_add_add, Test.Sales) 

# Holts winter exponential smoothing with multiplicative seasonality and additive trend
hwe_model_mul_add = ExponentialSmoothing(Train["Sales"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()
pred_hwe_mul_add = hwe_model_mul_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_mul_add, Test.Sales) 


# Final Model on 100% Data
hwe_model_add_add = ExponentialSmoothing(plastic_data["Sales"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()

# Load the new data which includes the entry for future 4 values
new_data = pd.read_excel("F:/assignment/forecasting-time sreris/Datasets_Forecasting/PlasticSales_pridict_data.xlsx")

newdata_pred = hwe_model_add_add.predict(start = new_data.index[0], end = new_data.index[-1])
newdata_pred

####################################### PROBLEM 4 #############################

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.holtwinters import SimpleExpSmoothing # SES
from statsmodels.tsa.holtwinters import Holt # Holts Exponential Smoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing # 
# from datetime import datetime

solar_data= pd.read_csv("F:/assignment/forecasting-time sreris/Datasets_Forecasting/solarpower_cumuldaybyday2.csv")

solar_data.cum_power.plot() # time series plot 

# Centering moving average for the time series
solar_data.cum_power.plot(label = "org")
for i in range(2, 9, 2):
    solar_data["cum_power"].rolling(i).mean().plot(label = str(i))
plt.legend(loc = 3)
    
# Time series decomposition plot 
decompose_ts_add = seasonal_decompose(solar_data.cum_power, model = "additive", period = 4)
decompose_ts_add.plot()
decompose_ts_mul = seasonal_decompose(solar_data.cum_power, model = "multiplicative", period = 4)
decompose_ts_mul.plot()

# ACF plot on Original data sets 
import statsmodels.graphics.tsaplots as tsa_plots
tsa_plots.plot_acf(solar_data.cum_power, lags = 4)
# tsa_plots.plot_pacf(solar_data.cum_power, lags=4)

# splitting the data into Train and Test data
# Recent 4 time period values are Test data

Train = solar_data.head(2500)
Test = solar_data.tail(58)

# to change the index value in pandas data frame 
# Test.set_index(np.arange(1,4),inplace=True)

# Creating a function to calculate the MAPE value for test data 
def MAPE(pred,org):
    temp = np.abs((pred-org)/org)*100
    return np.mean(temp)


# Simple Exponential Method
ses_model = SimpleExpSmoothing(Train["cum_power"]).fit()
pred_ses = ses_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_ses, Test.cum_power) 

# Holt method 
hw_model = Holt(Train["cum_power"]).fit()
pred_hw = hw_model.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hw, Test.cum_power) 

# Holts winter exponential smoothing with additive seasonality and additive trend
hwe_model_add_add = ExponentialSmoothing(Train["cum_power"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()
pred_hwe_add_add = hwe_model_add_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_add_add, Test.cum_power) 

# Holts winter exponential smoothing with multiplicative seasonality and additive trend
hwe_model_mul_add = ExponentialSmoothing(Train["cum_power"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()
pred_hwe_mul_add = hwe_model_mul_add.predict(start = Test.index[0], end = Test.index[-1])
MAPE(pred_hwe_mul_add, Test.cum_power) 


# Final Model on 100% Data
hwe_model_add_add = ExponentialSmoothing(solar_data["cum_power"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()

# Load the new data which includes the entry for future 4 values
new_data = pd.read_excel("F:/data science & ML/forecasting-time series/Forecasting Datasets/Newdata_solar_data_cum_power.xlsx")

newdata_pred = hwe_model_add_add.predict(start = new_data.index[0], end = new_data.index[-1])
newdata_pred
