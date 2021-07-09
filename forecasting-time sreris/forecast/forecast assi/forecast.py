###################################problem 1#########################
import pandas as pd
airlines = pd.read_excel("C:/Users/hp/Desktop/forecast assi/Airlines Data.xlsx")

# Pre processing
import numpy as np
airlines['Month'] = airlines['Month'].dt.strftime('%Y-%m-%d')
airlines["t"] = np.arange(1,97)

airlines["t_square"] = airlines["t"]*airlines["t"]
airlines["log_Passengers"] = np.log(airlines["Passengers"])
airlines.columns

p = airlines["Month"][0]
p[5:7]

airlines['months']= 0

for i in range(96):
    p = airlines["Month"][i]
    airlines['months'][i]= p[5:7]

#replacing numeric with strings for month column
month = {1:'Jan',2:'Feb',3:'Mar',4:'Apr',5:'May',6:'Jun',7:'Jul',8:'Aug',9:'Sep',10:'Oct',11:'Nov',12:'Dec'}
airlines['months'].replace(month, inplace = True)

month_dummies = pd.DataFrame(pd.get_dummies(airlines['months']))
airlines1 = pd.concat([airlines, month_dummies], axis = 1)

# Visualization - Time plot
airlines1.Passengers.plot()

# Data Partition
Train = airlines1.head(76)
Test = airlines1.tail(20)

####################### L I N E A R ##########################
import statsmodels.formula.api as smf 

linear_model = smf.ols('Passengers ~ t', data=Train).fit()
pred_linear =  pd.Series(linear_model.predict(pd.DataFrame(Test['t'])))
rmse_linear = np.sqrt(np.mean((np.array(Test['Passengers']) - np.array(pred_linear))**2))
rmse_linear

##################### Exponential ##############################

Exp = smf.ols('log_Passengers ~ t', data = Train).fit()
pred_Exp = pd.Series(Exp.predict(pd.DataFrame(Test['t'])))
rmse_Exp = np.sqrt(np.mean((np.array(Test['Passengers']) - np.array(np.exp(pred_Exp)))**2))
rmse_Exp

#################### Quadratic ###############################

Quad = smf.ols('Passengers ~ t+t_square', data=Train).fit()
pred_Quad = pd.Series(Quad.predict(Test[["t","t_square"]]))
rmse_Quad = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(pred_Quad))**2))
rmse_Quad

################### Additive seasonality ########################

add_sea = smf.ols('Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov', data=Train).fit()
pred_add_sea = pd.Series(add_sea.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov']]))
rmse_add_sea = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(pred_add_sea))**2))
rmse_add_sea

################## Multiplicative Seasonality ##################

Mul_sea = smf.ols('log_Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mult_sea = pd.Series(Mul_sea.predict(Test))
rmse_Mult_sea = np.sqrt(np.mean((np.array(Test['Passengers']) - np.array(np.exp(pred_Mult_sea)))**2))
rmse_Mult_sea

################## Additive Seasonality Quadratic Trend ############################

add_sea_Quad = smf.ols('Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Train).fit()
pred_add_sea_quad = pd.Series(add_sea_Quad.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','t','t_square']]))
rmse_add_sea_quad = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(pred_add_sea_quad))**2))
rmse_add_sea_quad 

################## Multiplicative Seasonality Linear Trend  ###########

Mul_Add_sea = smf.ols('log_Passengers ~ t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mult_add_sea = pd.Series(Mul_Add_sea.predict(Test))
rmse_Mult_add_sea = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(np.exp(pred_Mult_add_sea)))**2))
rmse_Mult_add_sea 

################## Testing #######################################

data = {"MODEL":pd.Series(["rmse_linear","rmse_Exp","rmse_Quad","rmse_add_sea","rmse_add_sea_quad","rmse_Mult_sea","rmse_Mult_add_sea"]),"RMSE_Values":pd.Series([rmse_linear,rmse_Exp,rmse_Quad,rmse_add_sea,rmse_add_sea_quad,rmse_Mult_sea,rmse_Mult_add_sea])}
table_rmse=pd.DataFrame(data)
table_rmse

# 'rmse_Mult_add_sea' has the least value among the models prepared so far Predicting new values 
#predicting for next five years
model_full = smf.ols('log_Passengers ~ t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov', data=airlines1).fit()

#loading prediction data 
new_data_Airlines = pd.read_csv("C:/Users/hp/Desktop/forecast assi/new_data_Airlines Data.csv")
new_data_Airlines["t"] = np.arange(97,121)
new_data_Airlines["t_square"] = new_data_Airlines["t"]*new_data_Airlines["t"]

new_data_Airlines['months']= 0

for i in range(24):
    p = new_data_Airlines["Month"][i]
    new_data_Airlines['months'][i]= p[0:3]

month_dummies = pd.DataFrame(pd.get_dummies(new_data_Airlines['months']))
new_data_Airlines1 = pd.concat([new_data_Airlines, month_dummies], axis = 1)

pred_new  = pd.Series(model_full.predict(new_data_Airlines1))
#taking antilog of predicted values
pred_new = np.exp(pred_new)

new_data_Airlines1["forecasted_Passenger"] = pd.Series(pred_new)

#predicted Passengers for next 5 year will be
new_data_Airlines1[['Month','forecasted_Passenger']]
###################################################Problem 2##########################
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.holtwinters import SimpleExpSmoothing # SES
from statsmodels.tsa.holtwinters import Holt # Holts Exponential Smoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing # 

cocacola = pd.read_excel("C:/Users/hp/Desktop/forecast assi/CocaCola_Sales_Rawdata.xlsx")

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

# splitting the data into Train and Test data
# Recent 4 time period values are Test data
Train = cocacola.head(38)
Test = cocacola.tail(4)

# Creating a function to calculate the MAPE value for test data 
def MAPE(pred,org):
    temp = np.abs((pred-org)/org)*100
    return np.mean(temp)

# Simple Exponential Method
ses_model = SimpleExpSmoothing(Train["Sales"]).fit()
pred_ses = ses_model.predict(start = Test.index[0], end = Test.index[-1])
mape_SEM = MAPE(pred_ses, Test.Sales) 

# Holt method 
hw_model = Holt(Train["Sales"]).fit()
pred_hw = hw_model.predict(start = Test.index[0], end = Test.index[-1])
mape_HM = MAPE(pred_hw, Test.Sales) 

# Holts winter exponential smoothing with additive seasonality and additive trend
hwe_model_add_add = ExponentialSmoothing(Train["Sales"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()
pred_hwe_add_add = hwe_model_add_add.predict(start = Test.index[0], end = Test.index[-1])
mape_HWE_add = MAPE(pred_hwe_add_add, Test.Sales) 

# Holts winter exponential smoothing with multiplicative seasonality and additive trend
hwe_model_mul_add = ExponentialSmoothing(Train["Sales"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()
pred_hwe_mul_add = hwe_model_mul_add.predict(start = Test.index[0], end = Test.index[-1])
mape_HWE_mul = MAPE(pred_hwe_mul_add, Test.Sales) 

#getting Mean absolute percentage error for all model in a table for comparison
data = {"MODEL":pd.Series(["Simple Exponential Method","Holt method","HWE smoothing with add. Sn. & add. trend","HWE smoothing with Mul. Sn. & add. trend"]),"RMSE_Values":pd.Series([mape_SEM,mape_HM,mape_HWE_add,mape_HWE_mul])}
table_mape=pd.DataFrame(data)
table_mape

# Final Model on 100% Data for HWE smoothing with Mul. Sn. & add. trend
hwe_model_mul_add = ExponentialSmoothing(cocacola["Sales"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()

# Load the new data which includes the entry for future 4 values
new_data = pd.read_excel("C:/Users/hp/Desktop/forecast assi/New_data_CocaCola_Sales_Rawdata.xlsx")

newdata_pred = hwe_model_add_add.predict(start = new_data.index[0], end = new_data.index[-1])
newdata_pred

#storing predicted_sales in new_data
new_data['predicted_Sales'] = newdata_pred
#scroll down in new_data and check for predicted_sales for next 2 years

#################################problem 3####################################################
import pandas as pd
plastic = pd.read_csv("C:/Users/hp/Desktop/forecast assi/PlasticSales.csv")

# Pre processing
import numpy as np
plastic["t"] = np.arange(1,61)

plastic["t_square"] = plastic["t"]*plastic["t"]
plastic["log_Sales"] = np.log(plastic["Sales"])
plastic.columns

p = plastic["Month"][0]
p[0:3]

plastic['months']= 0

for i in range(60):
    p = plastic["Month"][i]
    plastic['months'][i]= p[0:3]

month_dummies = pd.DataFrame(pd.get_dummies(plastic['months']))
plastic1 = pd.concat([plastic, month_dummies], axis = 1)

# Visualization - Time plot
plastic1.Sales.plot()

# Data Partition
Train = plastic1.head(40)
Test = plastic1.tail(20)

####################### L I N E A R ##########################
import statsmodels.formula.api as smf 

linear_model = smf.ols('Sales ~ t', data=Train).fit()
pred_linear =  pd.Series(linear_model.predict(pd.DataFrame(Test['t'])))
rmse_linear = np.sqrt(np.mean((np.array(Test['Sales']) - np.array(pred_linear))**2))
rmse_linear

##################### Exponential ##############################

Exp = smf.ols('log_Sales ~ t', data = Train).fit()
pred_Exp = pd.Series(Exp.predict(pd.DataFrame(Test['t'])))
rmse_Exp = np.sqrt(np.mean((np.array(Test['Sales']) - np.array(np.exp(pred_Exp)))**2))
rmse_Exp

#################### Quadratic ###############################

Quad = smf.ols('Sales ~ t+t_square', data=Train).fit()
pred_Quad = pd.Series(Quad.predict(Test[["t","t_square"]]))
rmse_Quad = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_Quad))**2))
rmse_Quad

################### Additive seasonality ########################

add_sea = smf.ols('Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov', data=Train).fit()
pred_add_sea = pd.Series(add_sea.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov']]))
rmse_add_sea = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_add_sea))**2))
rmse_add_sea

################## Multiplicative Seasonality ##################

Mul_sea = smf.ols('log_Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mult_sea = pd.Series(Mul_sea.predict(Test))
rmse_Mult_sea = np.sqrt(np.mean((np.array(Test['Sales']) - np.array(np.exp(pred_Mult_sea)))**2))
rmse_Mult_sea

################## Additive Seasonality Quadratic Trend ############################

add_sea_Quad = smf.ols('Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Train).fit()
pred_add_sea_quad = pd.Series(add_sea_Quad.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','t','t_square']]))
rmse_add_sea_quad = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_add_sea_quad))**2))
rmse_add_sea_quad 

################## Multiplicative Seasonality Linear Trend  ###########

Mul_Add_sea = smf.ols('log_Sales ~ t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mult_add_sea = pd.Series(Mul_Add_sea.predict(Test))
rmse_Mult_add_sea = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(np.exp(pred_Mult_add_sea)))**2))
rmse_Mult_add_sea 

################## Testing #######################################

data = {"MODEL":pd.Series(["rmse_linear","rmse_Exp","rmse_Quad","rmse_add_sea","rmse_add_sea_quad","rmse_Mult_sea","rmse_Mult_add_sea"]),"RMSE_Values":pd.Series([rmse_linear,rmse_Exp,rmse_Quad,rmse_add_sea,rmse_add_sea_quad,rmse_Mult_sea,rmse_Mult_add_sea])}
table_rmse=pd.DataFrame(data)
table_rmse

# 'rmse_Mult_add_sea' has the least value among the models prepared so far Predicting new values 
#predicting for next five years
model_full = smf.ols('log_Sales ~ t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov', data=plastic1).fit()

#loading prediction data 
New_data_PlasticSales = pd.read_csv("C:/Users/hp/Desktop/forecast assi/New_data_PlasticSales.csv")
New_data_PlasticSales["t"] = np.arange(61,73)
New_data_PlasticSales["t_square"] = New_data_PlasticSales["t"]*New_data_PlasticSales["t"]

New_data_PlasticSales['months']= 0

for i in range(12):
    p = New_data_PlasticSales["Month"][i]
    New_data_PlasticSales['months'][i]= p[0:3]

month_dummies = pd.DataFrame(pd.get_dummies(New_data_PlasticSales['months']))
New_data_PlasticSales1 = pd.concat([New_data_PlasticSales, month_dummies], axis = 1)

pred_new  = pd.Series(model_full.predict(New_data_PlasticSales1))
#taking antilog of predicted values
pred_new = np.exp(pred_new)

New_data_PlasticSales1["forecasted_Sales"] = pd.Series(pred_new)

#predicted sales for next one year will be
New_data_PlasticSales1[['Month','forecasted_Sales']]

################################################problem 4####################################
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.holtwinters import SimpleExpSmoothing # SES
from statsmodels.tsa.holtwinters import Holt # Holts Exponential Smoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing # 

solarpower = pd.read_csv("C:/Users/hp/Desktop/forecast assi/solarpower_cumuldaybyday2.csv")

solarpower.cum_power.plot() # time series plot 

# Centering moving average for the time series
solarpower.cum_power.plot(label = "org")
for i in range(2, 9, 2):
    solarpower["cum_power"].rolling(i).mean().plot(label = str(i))
plt.legend(loc = 3)
    
# Time series decomposition plot 
decompose_ts_add = seasonal_decompose(solarpower.cum_power, model = "additive", period = 4)
decompose_ts_add.plot()
decompose_ts_mul = seasonal_decompose(solarpower.cum_power, model = "multiplicative", period = 4)
decompose_ts_mul.plot()

# ACF plot on Original data sets 
import statsmodels.graphics.tsaplots as tsa_plots
tsa_plots.plot_acf(solarpower.cum_power, lags = 4)

# splitting the data into Train and Test data
# Recent 4 time period values are Test data
Train = solarpower.head(2046)
Test = solarpower.tail(512)

# Creating a function to calculate the MAPE value for test data 
def MAPE(pred,org):
    temp = np.abs((pred-org)/org)*100
    return np.mean(temp)

# Simple Exponential Method
ses_model = SimpleExpSmoothing(Train["cum_power"]).fit()
pred_ses = ses_model.predict(start = Test.index[0], end = Test.index[-1])
mape_SEM = MAPE(pred_ses, Test.cum_power) 

# Holt method 
hw_model = Holt(Train["cum_power"]).fit()
pred_hw = hw_model.predict(start = Test.index[0], end = Test.index[-1])
mape_HM = MAPE(pred_hw, Test.cum_power) 

# Holts winter exponential smoothing with additive seasonality and additive trend
hwe_model_add_add = ExponentialSmoothing(Train["cum_power"], seasonal = "add", trend = "add", seasonal_periods = 4).fit()
pred_hwe_add_add = hwe_model_add_add.predict(start = Test.index[0], end = Test.index[-1])
mape_HWE_add = MAPE(pred_hwe_add_add, Test.cum_power) 

# Holts winter exponential smoothing with multiplicative seasonality and additive trend
hwe_model_mul_add = ExponentialSmoothing(Train["cum_power"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()
pred_hwe_mul_add = hwe_model_mul_add.predict(start = Test.index[0], end = Test.index[-1])
mape_HWE_mul = MAPE(pred_hwe_mul_add, Test.cum_power) 

#getting Mean absolute percentage error for all model in a table for comparison
data = {"MODEL":pd.Series(["Simple Exponential Method","Holt method","HWE smoothing with add. Sn. & add. trend","HWE smoothing with Mul. Sn. & add. trend"]),"RMSE_Values":pd.Series([mape_SEM,mape_HM,mape_HWE_add,mape_HWE_mul])}
table_mape=pd.DataFrame(data)
table_mape

# Final Model on 100% Data for HWE smoothing with Mul. Sn. & add. trend
hwe_model_mul_add = ExponentialSmoothing(solarpower["cum_power"], seasonal = "mul", trend = "add", seasonal_periods = 4).fit()

#predicting for next 2 years
new_data = pd.read_csv("C:/Users/hp/Desktop/forecast assi/new_data_solarpower_cumuldaybyday2.csv")

newdata_pred = hwe_model_add_add.predict(start = new_data.index[0], end = new_data.index[-1])
newdata_pred
#storing predicted_sales in new_data
new_data['predicted_cum_power'] = newdata_pred
#scroll down in new_data and check for predicted_sales for next 2 years

#plotting time series for predicted values
new_data.predicted_cum_power.plot()
#since there is a increase power consumption for next 2 years , it can be considered to switch into renewable source of energy.

##################################################END####################################