library(TSA)

# data prep -------

# read data
LE <- read.csv("/Users/Traky/Desktop/4A03_Project/Lake_Erie.csv")

# take out the 1st column
LE <- LE[,-1]

# reorder column
LE <- LE[, c(2, 1)]

# convert to a ts object
LE_ts <- ts(
  LE,
  start = c(1921, 1),
  end = c(1970, 12),
  frequency = 12
)
LE_ts <- LE_ts[,-1]

# 80/20 training/testing split ------
train<-window(LE_ts,end=c(1960,12))
test<-window(LE_ts,start=c(1961,1))

# plotting -------

# plot the original series
plot(train,
     main = "Monthly Lake Erie Water Levels (1921 – 1960)",
     xlab = "",
     ylab = "Water Levels in Tens of Meters")

# plot the lagged-one differenced series
plot(diff(train),
     main = "Lagged-one Differenced Series (1921 – 1960)",
     xlab = "",
     ylab = "Water Levels in Tens of Meters")

diff_LE <- diff(train)

# stationarity test -------

# Dickey-Fuller Test
library(tseries, quietly = T)
adf.test(diff_LE) # stationary

# ACF & PCF ---------
library(PerformanceAnalytics)
chart.ACFplus(diff_LE,main = "Lagged-one Differenced Series (training)",maxlag = 60)

# seasonality ---------
library(TSA)
p <- periodogram(diff_LE)
seasonality <- p$freq[which.max(p$spec)] # 1/12
1 / seasonality # 12

# deterministic trend model ---------

# do the residuals show autocorrelation
fit<-lm(train~season(train)-1); fit
pred<-predict(fit)
chart.ACFplus(train-pred)

# SARIMA -------

# seasonal difference
lagged12_diff_LE<-diff(diff_LE,lag=12)

# plot
plot(window(lagged12_diff_LE,start=c(1956,1)),ylab="",main="First and Seasonal Difference of the Series (train)")
points(y=lagged12_diff_LE,x=time(lagged12_diff_LE),pch=as.vector(season(lagged12_diff_LE)))

# acf...
chart.ACFplus(lagged12_diff_LE, main = "First and Seasonal Difference of the Series (train)",maxlag = 60)

# Model Fitting ------------

# ARIMA(1,1,0)x(1,1,1)12
arima(train,order = c(1, 1, 0),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 552.16

# ARIMA(1,1,0)x(0,1,1)12 - chosen 1
arima(train,order = c(1, 1, 0),seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 551.74

# ARIMA(1,1,1)x(1,1,1)12
arima(train,order = c(1, 1, 1),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 553.87

# ARIMA(1,1,1)x(0,1,1)12
arima(train,order = c(1, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 553.48

# ARIMA(3,1,3)x(1,1,1)12 - chosen 2
arima(train,order = c(3, 1, 3),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 550.16
# warning - possible convergence problem

# ARIMA(3,1,3)x(0,1,1)12
arima(train,order = c(3, 1, 3),seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 550.56
# warning - possible convergence problem

# model selection ----------

# We proceed with these two models 

# ARIMA(1,1,0)x(0,1,1)12 
m1 <- arima(train,order = c(1, 1, 0),seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 645.71

# ARIMA(3,1,3)x(1,1,1)12
m2 <- arima(train,order = c(3, 1, 3),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 641.17

# Model Diagnostic -------
## WENDY TO FILL IN THE SECTION

# Prediction -------

# m1 - ARIMA(1,1,0)x(0,1,1)12 
# m2 - ARIMA(3,1,3)x(1,1,1)12

plot(m1,n.ahead = 120,n1=c(1950,1), type="l",main = "Forecast - ARIMA(1,1,0)x(0,1,1)12 ")
lines(test,col=12)

plot(m2,n.ahead = 120,n1=c(1950,1), type="l",main = "Forecast - ARIMA(3,1,3)x(1,1,1)12")
lines(test,col=12)

