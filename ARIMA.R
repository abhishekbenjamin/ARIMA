library(forecast)
library(tseries)

class(GDP)

# Converting into Timeseries format

gdp_time <- ts(gdp$GDP, start = min(gdp$DATE), end = max(gdp$DATE), frequency = 4)

class(gdp_time)

plot(gdp_time)

# Checking for Auto-correlation & Stationarity 

## Auto-correlation Test

Acf(gdp_time) # Spikes crossing blue line indicates data is Not Stationary
Pacf(gdp_time) # This looks ok as not much spikes are crossing blue line

## Stationarity Test

adf.test(gdp_time) 

# Converting Non-stationary data into Stationary data

gdp_model <- auto.arima(gdp_time, ic = "aic", trace = T)

Acf(ts(gdp_model$residuals))
Pacf(ts(gdp_model$residuals))

# Predicting for next 10 years in 4 quarters

gdp_forecast <- forecast(gdp_model, level = c(95), h = 10*4)

gdp_forecast

plot(gdp_forecast)


# Validating Model Accuracy
## if model shows p-value less than 0.05 that means data has Auto-correlation

Box.test(gdp_forecast$residuals, lag = 5, type = "Ljung-Box")
Box.test(gdp_forecast$residuals, lag = 10, type = "Ljung-Box")
Box.test(gdp_forecast$residuals, lag = 15, type = "Ljung-Box")

