
library('ggplot2')
library('forecast')
library('tseries')

#Step 1: Load, Visualize and Examine Data

sp500 <- read.csv("C:/Techs/Ryerson-DataScience/CMKE136-Capstone/data/SP500_10012013-09302018.csv", header = TRUE, stringsAsFactors = FALSE)

head(sp500)
sp500$Date <- as.Date(sp500$Date, format="%Y-%m-%d")
attach(sp500)
plot(Date, Close, main = "S&P 500 Stock Market Index", xlab = "Year", ylab = "Close Values")
detach(sp500)
#Step 2: Stationarize the time series

#step 2.1 Create time series object and remove any potential outliers
ts_close <- tsclean(ts(sp500[, c('Close')], frequency = 365.25))
  
#step 2.2 Stationarity check - Dicky-Fuller test  
adf.test(ts_close, alternative = "stationary") 
#p-value > 0.5 so accept null hypothesis - non-stationary

#step 2.3 Differencing series to make it stationary, d=1
close_d1 <- diff(ts_close, differences = 1)
plot(close_d1)
adf.test(close_d1, alternative = "stationary")

#Step 3: Plot ACF/PACF charts and choose optimal parameters

#step 3.1 ACF to determine parameter q in ARIMA(p, d, q), q = 0
acf(close_d1, main = "ACF for Differenced Series")

#step 3.2 PACF to determine parameter p in ARIMA (p,d, q), p = 0
pacf(close_d1, main = "PACF for Differenced Series")

#Step 4: Build and fit ARIMA model
arima(close_d1, order=c(0,0,0))  #ARIMA(0,1,0) aic = 10685.41
arima(close_d1, order=c(1,0,0))  #ARIMA(1,1,0) aic = 10686.73
arima(close_d1, order=c(1,0,1))  #ARIMA(1,1,1) aic = 10679.06, lowest
arima(close_d1, order=c(0,0,1))  #ARIMA(0,1,1) aic = 10686.63
arima(close_d1, order=c(0,0,2))  #ARIMA(0,1,2) aic = 10683.67
arima(close_d1, order=c(2,0,0))  #ARIMA(2,1,0) aic = 10683.75

auto.arima(close_d1, seasonal = FALSE)

#Step 5: Diagnosis the model
fit <- auto.arima(ts_close)
tsdiag(fit)

#Step 6: Make Forecasts and Cross Validation
fcast <- forecast(fit, h=30)  
plot(fcast)

# 6.1 Actual 30days Index from Oct.1st, 2018
sp500_Oct <- read.csv("C:/Techs/Ryerson-DataScience/CMKE136-Capstone/data/SP500_10012018_11092018.csv", header = TRUE, stringsAsFactors = FALSE)

# 6.2 Calculate forecast accuracy by comparing with actual index closing values
accuracy(fcast, sp500_Oct$Close)

fit_5years <- arima(ts_close, order=c(1,1,1))

fcast_Oct <- forecast(fit_5years, h=30)

ts_5years_Oct <- (ts(c(sp500$Close, sp500_Oct$Close), frequency = 365.25))

plot(fcast_Oct, main=" ")
lines(ts_5years_Oct)

#Step 7: Compare ARIMA and SVM Models 

days <- 1:length(sp500$Date)
df_sp500 <- data.frame(days, sp500$Close)
colnames(df_sp500) <- c("Dayth", "Close")

# train an svm model, consider further tuning parameters for lower MSE
svm_mdl <- svm(Close ~ Dayth,data=df_sp500, type="eps-regression",kernel="radial",cost=10000, gamma=10)

#specify timesteps for forecast, for all series + 30 days ahead
total_days <- length(days) + 30
num_days <- 1:total_days

#compute forecast for all the days 
svm_fcast <- predict(svm_mdl, newdata=data.frame(Dayth=num_days))
plot(svm_fcast)
accuracy(svm_fcast, sp500_Oct$Close)



 



