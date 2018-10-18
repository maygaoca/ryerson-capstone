
library('ggplot2')
library('forecast')
library('tseries')

#Step 1: Load, Visualize and Examine Data

sp500 <- read.csv("C:/Techs/Ryerson-DataScience/CMKE136-Capstone/data/SP500_092013-092018.csv", header = TRUE, stringsAsFactors = FALSE)

head(sp500)
sp500$Date <- as.Date(sp500$Date, format="%Y-%m-%d")
attach(sp500)
plot(Date, Close, main = "S&P 500 Stock Market Index", xlab = "Year", ylab = "Close Values")

#Step 2: Stationarize the time series

#step 2.1 Stationarity check - Dicky-Fuller test 
adf.test(Close, alternative = "stationary") 
#p-value > 0.5 so accept null hypothesis - non-stationary


#step 2.2 Differencing series to make it stationary, d=1
close_d1 <- diff(Close, differences = 1)
plot(close_d1)
adf.test(close_d1, alternative = "stationary")

#Step 3: Plot ACF/PACF charts and choose optimal parameters

#step 3.1 ACF to determine parameter q in ARIMA(p, d, q), q = 0
acf(close_d1, main = "ACF for Differenced Series")

#step 3.2 PACF to determine parameter p in ARIMA (p,d, q), p = 0
pacf(close_d1, main = "PACF for Differenced Series")

#Step 4: Build and fit ARIMA model
arima(close_d1, order=c(0,0,0))  #aic = 10685.41
arima(close_d1, order=c(1,0,0))  #aic = 10686.73
arima(close_d1, order=c(1,0,1))  #aic = 10679.06, lowest
arima(close_d1, order=c(0,0,1))  #aic = 10686.63
arima(close_d1, order=c(0,0,2))  #aic = 10683.67
arima(close_d1, order=c(2,0,0))  #aic = 10683.75

auto.arima(close_d1, seasonal = FALSE)

#Step 5: Predict and iterate with different time periods
fit <- arima(Close, order = c(1,1,1 ))
fcast <- forecast(fit, h=90)
plot(fcast)



