# load library
library(forecast)
library(tseries)

# 1. download a search word from google trends that you assume has a cyclic/seasonal component. 
# Inspect the file first to assure there a no missing values. Replace missing values with a 0 and save as csv.
# To read the data use the function read.csv().
<- read.csv()

# 2. transform the data into a dataset using the function ts() and the according cycle and start date

<- ts(, frequency = , start = )

# 3. inspect the data using the start() and end() function 
start()
end()

# 4. plot the data
plot.ts()

# 5. explain the course of the time series in own words and what whype of components you expect

# 6. run the automatic arima model using the function auto.arima() 
auto.arima() 

# Extra task: run the auto.arima() function with the parameters ic="aic" and trace = TRUE
# What is the output, and what values where chosen for q and p?

# 7. plot the random component using the function plot.ts()
plot.ts()

# 8. check the mean and autocorrelation of the random component
# Is it correct? Does the model fit the time series? 

# 9. use this model to calculate the forecast for a reasonable time frame of your data using the function forecast()
forecast()

# 10. plot the forecast using the function plot()
plot

# 11. use this model to calculate the forecast for an extremely long time (e.g. 100 years)
# Do you believe the forcast? 


