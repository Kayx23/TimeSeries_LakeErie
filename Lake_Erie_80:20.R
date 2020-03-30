library(TSA)

# Data prep -------

# read data
LE <- read.csv("/Users/Lake_Erie.csv")

# take out the 1st column
LE <- LE[, -1]

# reorder column
LE <- LE[, c(2, 1)]

# convert to a ts object
LE_ts <- ts(
  LE,
  start = c(1921, 1),
  end = c(1970, 12),
  frequency = 12
)
LE_ts <- LE_ts[, -1]

# 80/20 training/testing split ------
train <- window(LE_ts, end = c(1960, 12))
test <- window(LE_ts, start = c(1961, 1))

# Plotting -------

# plot the original series
plot(LE_ts,
     main = "Monthly Lake Erie Water Levels (1921-1960)",
     xlab = "",
     ylab = "Water Levels in Tens of Meters")
points(y = LE_ts,
       x = time(LE_ts),
       pch = as.vector(season(LE_ts)))

# plot the training series
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

# Dickey-Fuller Test -------
library(tseries, quietly = T)
adf.test(diff_LE) # stationary

# ACF & PCF ---------
library(PerformanceAnalytics)
chart.ACFplus(diff_LE, main = "Lagged-one Differenced Series (training)", maxlag = 60)

# Seasonality ---------
library(TSA)
p <- periodogram(diff_LE)
seasonality <- p$freq[which.max(p$spec)] # 1/12
1 / seasonality # 12

# Deterministic trend model ---------
fit <- lm(train ~ season(train) - 1)
fit
pred <- predict(fit)
chart.ACFplus(train - pred)

# SARIMA -------

# seasonal difference
lagged12_diff_LE <- diff(diff_LE, lag = 12)

# plot
plot(window(lagged12_diff_LE, start = c(1956, 1)),
     ylab = "",
     main = "First and Seasonal Difference of the Series (train)")
points(y = lagged12_diff_LE,
       x = time(lagged12_diff_LE),
       pch = as.vector(season(lagged12_diff_LE)))

# ACF&PACF
chart.ACFplus(lagged12_diff_LE, main = "First and Seasonal Difference of the Series (train)", maxlag = 60)

# Model Fitting ------------

# ARIMA(1,1,0)x(1,1,1)12
arima(train,
      order = c(1, 1, 0),
      seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 552.16

# ARIMA(1,1,0)x(0,1,1)12 - chosen 1
arima(train,
      order = c(1, 1, 0),
      seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 551.74

# ARIMA(1,1,1)x(1,1,1)12
arima(train,
      order = c(1, 1, 1),
      seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 553.87

# ARIMA(1,1,1)x(0,1,1)12
arima(train,
      order = c(1, 1, 1),
      seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 553.48

# ARIMA(3,1,3)x(1,1,1)12 - chosen 2
arima(train,
      order = c(3, 1, 3),
      seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 550.16

# ARIMA(3,1,3)x(0,1,1)12
arima(train,
      order = c(3, 1, 3),
      seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 550.56
# warning - possible convergence problem

# Model Selection ----------

# ARIMA(1,1,0)x(0,1,1)12
m1 <-
  arima(train,
        order = c(1, 1, 0),
        seasonal = list(order = c(0, 1, 1), period = 12)) # aic = 645.71

# ARIMA(3,1,3)x(1,1,1)12
m2 <-
  arima(train,
        order = c(3, 1, 3),
        seasonal = list(order = c(1, 1, 1), period = 12)) # aic = 641.17

# Model Diagnostic -------

# m1 - ARIMA(1,1,0)x(0,1,1)12

# calculating residuals
m1.res <- train - fitted(m1, train)

# plotting residuals
plot(m1.res, main = "Residuals of ARIMA(1,1,0)x(0,1,1)12")

# residuals autocorrelations
chart.ACFplus(m1.res, main = "Residuals of ARIMA(1,1,0)x(0,1,1)12", maxlag = 60)

par(mfrow = c(1, 2))

# residual histogram
hist(m1.res, main = "Residuals of ARIMA(1,1,0)x(0,1,1)12")

# reisidual QQ plot
qqnorm(m1.res, main = "Residuals of ARIMA(1,1,0)x(0,1,1)12")
qqline(m1.res, col = 2)

# m2 - ARIMA(3,1,3)x(1,1,1)12

# calculating residuals
m2.res <- train - fitted(m2, train)

par(mfrow = c(1, 1))

# plotting residuals
plot(m2.res, main = "Residuals of ARIMA(3,1,3)x(1,1,1)12")

# residuals autocorrelations
chart.ACFplus(m2.res, main = "Residuals of ARIMA(3,1,3)x(1,1,1)12", maxlag = 60)

par(mfrow = c(1, 2))

# residual histogram
hist(m2.res, main = "Residuals of ARIMA(3,1,3)x(1,1,1)12")

# reisidual QQ plot
qqnorm(m2.res, main = "Residuals of ARIMA(3,1,3)x(1,1,1)12")
qqline(m2.res, col = 2)

par(mfrow = c(1, 1))

# Prediction -------

# m1 - ARIMA(1,1,0)x(0,1,1)12
# m2 - ARIMA(3,1,3)x(1,1,1)12

m1.plot <- plot(
  m1,
  n.ahead = 120,
  n1 = c(1950, 1),
  type = "l",
  main = "Forecast - ARIMA(1,1,0)x(0,1,1)12 "
)
lines(test, col = 12)

m2.plot <- plot(
  m2,
  n.ahead = 120,
  n1 = c(1950, 1),
  type = "l",
  main = "Forecast - ARIMA(3,1,3)x(1,1,1)12"
)
lines(test, col = 12)

# Prediction Comparison Plot
m1pred <- predict(m1, n.ahead = 120)$pred
m2pred <- predict(m2, n.ahead = 120)$pred

plot(
  test,
  type = 'l',
  col = "blue",
  lwd = 2,
  main = "Comparison between Forecast Illustrations",
  ylab = "Water Level in Tens Meter"
)
lines(m1pred, col = "orange", lwd = 3)
lines(m2pred, col = "chocolate", lwd = 3)

legend(
  "bottomright",
  legend = c(
    "Testing dataset",
    "ARIMA(1,1,0)x(0,1,1)12",
    "ARIMA(3,1,3)x(1,1,1)12"
  ),
  col = c("blue", "orange", "chocolate"),
  lwd = c(2, 3, 3)
)

# checking constant mean each cycle in prediction

# m1

for (i in 1961:1970) {
  k <- mean(subset(m1.plot$pred, floor(time(m1.plot$pred)) == i))
  cat("The mean in year", i, "is", k, "\n")
}
# decreasing mean overtime

# m2

for (i in 1961:1970) {
  k <- mean(subset(m2.plot$pred, floor(time(m2.plot$pred)) == i))
  cat("The mean in year", i, "is", k, "\n")
}
# non-constant mean overtime
