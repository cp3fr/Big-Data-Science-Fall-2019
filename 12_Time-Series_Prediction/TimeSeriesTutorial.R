# load library
library(forecast)
library(tseries)

# load data
data(AirPassengers)

# check what type of variable the data is
class(AirPassengers)

#Q: What if your data set is not yet a time series? 
datats <- ts(AirPassengers)
datats
datats <- ts(AirPassengers, frequency = 12)
datats
datats <- ts(AirPassengers, frequency = 12, start = c(1949, 1))
datats

# inspect beginning of data set
start(AirPassengers)
# inspect end of data set
end(AirPassengers)

# print data set to console
AirPassengers

# check frequency of time series
frequency(AirPassengers)

# plot data
plot(AirPassengers)

# Q: Explain the course of the time series in own words and what whype of components you expect

# decompose the time series
ddata <- decompose(AirPassengers, type = "multiplicative")

# Q: Inspect the variable in the workspace

# plot the individual components
plot(ddata$trend)
plot(ddata$seasonal)
plot(ddata$random)

# or better plot all at once
plot(ddata)

# the random component should be have zero
mean(ddata$random, na.rm=TRUE)
# --> it's not
# the random component should not look like it still contains cyclic/seasonal information
acf(ddata$random[!is.na(ddata$random)])
pacf(ddata$random[!is.na(ddata$random)])
# --> it still does

# run automatic arima model to estimate q and p parameters
AirPassengersModel <- auto.arima(AirPassengers)
plot.ts(AirPassengersModel$residuals)

# check the meand and autocorrelation of the random component of the new model
mean(AirPassengersModel$residuals, na.rm=TRUE)
acf(AirPassengersModel$residuals)
pacf(AirPassengersModel$residuals)
# Q: is it better? yes. 

# use this model to calculate the forecast for the next 10 years
AirPassengersForecast <- forecast(AirPassengersModel, level = c (95), h = 10*12)
plot(AirPassengersForecast)
