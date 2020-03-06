
# reading data -------
LE <- read.csv("/Users/Traky/Desktop/4A03_Project/Lake_Erie.csv")

# take out first column
LE <- LE[, -1]

# reorder column
LE <- LE[, c(2, 1)]

# Convert to a ts object
LE_ts <- ts(
  LE,
  start = c(1921, 1),
  end = c(1970, 12),
  frequency = 12
)
LE_ts <- LE_ts[, -1]

# plotting -------
plot(LE_ts,
     main = "Monthly Lake Erie Levels (1921 – 1970)",
     xlab = "",
     ylab = "Lake Erie Water Levels in Tens of Meters")

plot(diff(LE_ts),
     main = "Diff - Monthly Lake Erie Levels (1921 – 1970)",
     xlab = "",
     ylab = "Lake Erie Water Levels in Tens of Meters")

# log transformation -------
plot(diff(log(LE_ts)), main = "difference log")

# power transformation -------
BoxCox.ar(LE_ts, lambda = seq(1.45, 1.6, 0.01))
lambda = 1.55
plot((LE_ts) ^ (lambda))
plot(diff((LE_ts) ^ (lambda)), main = "power with lambda = 1.55")
myModel <- diff((LE_ts) ^ (lambda))

# Dickey-Fuller Test  -------
library(tseries, quietly = T)
adf.test(myModel) # stationary

# ACF -------
acf_myModel <- acf(myModel, main = "acf - myModel")
pcf_myModel <- pacf(myModel, main = "pacf - myModel")  # AR(1) for non-seasonal component

# str(acf_myModel)
# x <- cbind(lag = acf_myModel$lag, autocorrelation = acf_myModel$acf)
# View(x)

# Fourier Transformation -------
library(TSA)
p <- periodogram(myModel)
seasonality <- p$freq[which.max(p$spec)] # 1/12
1/seasonality

# Taking out seasonality -------
myModel_s12 <- diff(myModel, 12)
plot(myModel_s12, main = "myModel_s12")
acf(myModel_s12, main = "acf - myModel_s12",lag.max = 48) # one spike
pacf(myModel_s12, main = "pacf - myModel_s12")
# is the spike indicating d=1 for the seasonal component? 

# differencing the seasonal component 
plot(diff(myModel_s12,1))

# Model Selection -------

# Auto select a model as a benchmark
library(forecast)

# original
model_auto = auto.arima(LE_ts, stepwise = FALSE, approximation = FALSE) # take forever
summary(model_auto)
# OUTPUT: ARIMA(1,0,2)(2,1,0)[12]
# ar1     ma1     ma2     sar1     sar2
# 0.9308  0.2342  0.1307  -0.7049  -0.3307
# s.e. 0.0166  0.0434  0.0442   0.0397   0.0393
# sigma^2 estimated as 0.1996:  log likelihood=-362.41
# AIC=736.81   AICc=736.96   BIC=763.07

# power transformed
model_auto_power = auto.arima(myModel, stepwise = FALSE, approximation = FALSE) # take forever
summary(model_auto_power)
# OUTPUT: ARIMA(3,0,0)(2,1,0)[12] 
# ar1     ma1     ma2     sar1     sar2
# 0.2223  0.0498  -0.1082  -0.7107  -0.3463
# s.e. 0.0412  0.0421   0.0412   0.0390   0.0390
# sigma^2 estimated as 9.558:  log likelihood=-1496.48
# AIC=3004.95   AICc=3005.1   BIC=3031.2

# Now, test our tentative models 

# SARIMA






# Model Diagnostic -------







# Prediction -------





# SARIMA - https://online.stat.psu.edu/stat510/lesson/4/4.1
# https://kimberlyannschveder.wordpress.com/2018/07/07/lake-erie-level-data/
# https://towardsdatascience.com/a-time-series-analysis-of-lake-erie-from-1921-to-1970-using-a-sarima-model-b79698df4762
