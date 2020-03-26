library(TSA)
library(forecast)

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

# plotting -------

# plot the original series
plot(LE_ts,
     main = "Monthly Lake Erie Water Levels (1921 – 1970)",
     xlab = "",
     ylab = "Water Levels in Tens of Meters")

# plot the lagged-one differenced series
plot(diff(LE_ts),
     main = "Lagged-one Differenced Series",
     xlab = "",
     ylab = "Water Levels in Tens of Meters")

diff_LE <- diff(LE_ts)

# stationarity test -------

# Dickey-Fuller Test
library(tseries, quietly = T)
adf.test(diff_LE) # stationary

# ACF & PCF ---------
library(PerformanceAnalytics)
chart.ACFplus(diff_LE,main = "Lagged-one Differenced Series",maxlag = 60)

# seasonality ---------
library(TSA)
p <- periodogram(diff_LE)
seasonality <- p$freq[which.max(p$spec)] # 1/12
1 / seasonality # 12

# deterministic trend model ---------

# do the residuals show autocorrelation
fit<-lm(LE_ts~season(LE_ts)-1); fit
pred<-predict(fit)
chart.ACFplus(LE_ts-pred)

# perfect AR(1) !!!
# but we can't proceed with deterministic trend no more
# moving onto SARIMA

# SARIMA -------

# seasonal difference
lagged12_diff_LE<-diff(diff_LE,lag=12)

# plot
plot(window(lagged12_diff_LE,start=c(1966,1)),ylab="",main="First and Seasonal Difference of the Series")
points(y=lagged12_diff_LE,x=time(lagged12_diff_LE),pch=as.vector(season(lagged12_diff_LE)))

# is there still a seasonality
p <- periodogram(lagged12_diff_LE)
seasonality <- p$freq[which.max(p$spec)] # 0.12
1 / seasonality # 12

# acf...
chart.ACFplus(lagged12_diff_LE, main = "First and Seasonal Difference of the Series",maxlag = 60)

# Model Fitting ------------

# ARIMA(1,1,0)x(1,1,1)12
arima(LE_ts,order = c(1, 1, 0),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 647.5

# ARIMA(1,1,0)x(0,1,1)12
arima(LE_ts,order = c(1, 1, 0),seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 645.71

# ARIMA(1,1,1)x(1,1,1)12
arima(LE_ts,order = c(1, 1, 1),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 649.29

# ARIMA(1,1,1)x(0,1,1)12
arima(LE_ts,order = c(1, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 647.52

# ARIMA(3,1,3)x(1,1,1)12
arima(LE_ts,order = c(3, 1, 3),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 641.17

# ARIMA(3,1,3)x(0,1,1)12
arima(LE_ts,order = c(3, 1, 3),seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 641.34
# warning - possible convergence problem

# decomposition --------------

decomp_LE <- decompose(LE_ts)
plot(decomp_LE)

de_seasoned <- LE_ts - decomp_LE$seasonal
plot(de_seasoned) # need to detrend
plot(diff(de_seasoned)) # good
chart.ACFplus(diff(de_seasoned),maxlag = 80)

plot(decomp_LE$seasonal)
plot(diff(decomp_LE$seasonal,12))
chart.ACFplus(decomp_LE$seasonal)

arima(LE_ts,order = c(2, 1, 2),seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 641.12

# Random SARIMA Models -------

library(forecast)

# auto.arima on the original series
model_auto = auto.arima(LE_ts, stepwise = FALSE, approximation = FALSE) # take forever
summary(model_auto)
# OUTPUT: ARIMA(1,0,2)(2,1,0)[12]
# ar1     ma1     ma2     sar1     sar2
# 0.9308  0.2342  0.1307  -0.7049  -0.3307
# s.e. 0.0166  0.0434  0.0442   0.0397   0.0393
# sigma^2 estimated as 0.1996:  log likelihood=-362.41
# AIC=736.81   AICc=736.96   BIC=763.07

# ARIMA(1,0,0)(2,1,1)[12]
model_1 = arima(LE_ts,
                order = c(1, 0, 0),
                seasonal = list(order = c(2, 1, 1), period = 12))
model_1  #AIC 662.9

# ARIMA(1,0,1)(2,1,1)[12]
model_2 = arima(LE_ts,
                order = c(1, 0, 1),
                seasonal = list(order = c(2, 1, 1), period = 12))
model_2  #AIC 643.27

# ARIMA(1,0,2)(2,1,1)[12]
model_3 = arima(LE_ts,
                order = c(1, 0, 2),
                seasonal = list(order = c(2, 1, 1), period = 12))
model_3  #AIC 637.71

# ARIMA(1,0,2)(1,1,1)[12]
model_3.1 = arima(LE_ts,
                  order = c(1, 0, 2),
                  seasonal = list(order = c(1, 1, 1), period = 12))
model_3.1  # 637.17 (best model)

# ARIMA(1,0,2)(2,1,2)[12]
model_4 = arima(LE_ts,
                order = c(1, 0, 2),
                seasonal = list(order = c(2, 1, 2), period = 12))
model_4  #AIC 638.81

# ARIMA(1,0,2)(3,1,1)[12]
model_5 = arima(LE_ts,
                order = c(1, 0, 2),
                seasonal = list(order = c(3, 1, 1), period = 12))
model_5  #AIC 638.4

### WINNER: Model 3.1, ARIMA(1,0,2)(1,1,1)[12]
best_model <- model_3.1

# Model Diagnostic -------
## WENDY TO FILL IN THE SECTION

# Prediction -------

# 80/20 training/testing split ------
train<-window(diff(LE_ts),end=c(1960,12))
test<-window(diff(LE_ts),start=c(1961,1))

# 80% training, 20% testing
training = window(LE_ts,start=c(1921,1), end=c(1960,12))
test = window(LE_ts,start=c(1961,1), end=c(1970,12))

# re-run best model on training
best_model = arima(training,
                  order = c(1, 0, 2),
                  seasonal = list(order = c(1, 1, 1), period = 12))
best_model  # AIC: 544

pred <- predict(best_model, n.ahead=120)
plot(training, main = "Forecast")
lines(test,col=12) # actual
points(pred$pred,col=2) # predicted

# 95% CI
Upper_CI = pred$pred + 1.96*pred$se
Lower_CI = pred$pred - 1.96*pred$se
lines(Upper_CI,lty=2,col=8)
lines(Lower_CI,lty=2,col=8)
