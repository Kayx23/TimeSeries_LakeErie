# reading data -------
LE <- read.csv("/Users/Traky/Desktop/4A03_Project/Lake_Erie.csv")

# take out first column
LE <- LE[,-1]

# reorder column
LE <- LE[, c(2, 1)]

# NORMALITY? 
hist(LE$Level,xlab="Water Levels in Tens of Meters",main="")
qqnorm(LE$Level);qqline(LE$Level,col=2)

# Convert to a ts object
LE_ts <- ts(
  LE,
  start = c(1921, 1),
  end = c(1970, 12),
  frequency = 12
)
LE_ts <- LE_ts[,-1]

# plotting -------
plot(LE_ts,
     main = "Monthly Lake Erie Water Levels (1921 – 1970)",
     xlab = "",
     ylab = "Water Levels in Tens of Meters")

plot(diff(LE_ts),
     main = "Lagged-one Differenced Series",
     xlab = "",
     ylab = "Water Levels in Tens of Meters")
# already constant variance?

myModel <- diff(LE_ts)

# power transformation (no point)
# BoxCox.ar(LE_ts, lambda = seq(1.45, 1.6, 0.01))
# lambda = 1.55
# plot((LE_ts) ^ (lambda))
# plot(diff((LE_ts) ^ (lambda)), main = "power with lambda = 1.55")
# myModel <- diff((LE_ts) ^ (lambda))

# log is not appropriate 

# Dickey-Fuller Test  -------
library(tseries, quietly = T)
adf.test(myModel) # stationary

# ACF -------
acf_diff <- acf(myModel, main = "acf - myModel")
pcf_diff <- pacf(myModel, main = "pacf - myModel")
# has seasonality

# str(acf_myModel)
# x <- cbind(lag = acf_myModel$lag, autocorrelation = acf_myModel$acf)
# View(x)

# Detect Seasonality; Fourier Transformation -------
library(TSA)
p <- periodogram(myModel)
seasonality <- p$freq[which.max(p$spec)] # 1/12
1 / seasonality # 12

# Taking out seasonality -------
# acf(diff(diff(LE_ts,1),lag=12))
myModel_s12 <- diff(myModel, lag = 12, differences = 1)
plot(myModel_s12, main = "myModel_s12")

# ACF no seasonality --------
acf(myModel_s12, main = "acf - myModel_s12", lag.max = 48) # one spike (AR(12)????)
pacf(myModel_s12, main = "pacf - myModel_s12", lag.max = 48)
# spike, spike, spike, spike... AR(4) in the seasonal component??????
# both are geometric, meaning the non-seasonal component is a ARMA model

# differencing the seasonal component
plot(diff(myModel_s12, 1))

# Model Selection 1 -------

library(forecast)

# original--- BENCHMARK
model_auto = auto.arima(LE_ts, stepwise = FALSE, approximation = FALSE) # take forever
summary(model_auto)
# OUTPUT: ARIMA(1,0,2)(2,1,0)[12]
# ar1     ma1     ma2     sar1     sar2
# 0.9308  0.2342  0.1307  -0.7049  -0.3307
# s.e. 0.0166  0.0434  0.0442   0.0397   0.0393
# sigma^2 estimated as 0.1996:  log likelihood=-362.41
# AIC=736.81   AICc=736.96   BIC=763.07

# test our tentative models on "LE_ts"

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

# Model Selection 2 -------

# power transformed
model_auto_power = auto.arima(myModel, stepwise = FALSE, approximation = FALSE) # take forever
summary(model_auto_power)
# OUTPUT: ARIMA(3,0,0)(2,1,0)[12]
# ar1     ma1     ma2     sar1     sar2
# 0.2223  0.0498  -0.1082  -0.7107  -0.3463
# s.e. 0.0412  0.0421   0.0412   0.0390   0.0390
# sigma^2 estimated as 9.558:  log likelihood=-1496.48
# AIC=3004.95   AICc=3005.1   BIC=3031.2


# Model Diagnostic -------
plot(residuals(best_model))
qqnorm(residuals(best_model))
qqline(residuals(best_model), col = 2)

t<- acf(residuals(best_model))    # hmmm is this right.....
max(t$acf) #0.1372544




# Prediction -------

# 80% training, 20% testing
training = window(LE_ts,start=c(1921,1), end=c(1960,12))
test = window(LE_ts,start=c(1961,1), end=c(1970,12))

# re-run best model on training
best_model = arima(training,
                  order = c(1, 0, 2),
                  seasonal = list(order = c(1, 1, 1), period = 12))
best_model  # AIC: 544

# cautious: if we use the trainig from the start, the best model might have been different! 

pred <- predict(best_model, n.ahead=120)
plot(training, main = "Forecast")
lines(test,col=12) # actual
points(pred$pred,col=2) # predicted

# 95% CI
Upper_CI = pred$pred + 1.96*pred$se
Lower_CI = pred$pred - 1.96*pred$se
lines(Upper_CI,lty=2,col=8)
lines(Lower_CI,lty=2,col=8)



# SARIMA - https://online.stat.psu.edu/stat510/lesson/4/4.1
# https://kimberlyannschveder.wordpress.com/2018/07/07/lake-erie-level-data/
# https://towardsdatascience.com/a-time-series-analysis-of-lake-erie-from-1921-to-1970-using-a-sarima-model-b79698df4762
