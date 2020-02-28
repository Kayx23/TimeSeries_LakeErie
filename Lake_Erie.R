


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

# plotting
plot(LE_ts,
     main = "Monthly Lake Erie Levels (1921 – 1970)",
     xlab = "",
     ylab = "Lake Erie Water Levels in Tens of Meters")

plot(diff(LE_ts),
     main = "Diff - Monthly Lake Erie Levels (1921 – 1970)",
     xlab = "",
     ylab = "Lake Erie Water Levels in Tens of Meters")

# log transformation
plot(diff(log(LE_ts)))

# power transformation
BoxCox.ar(LE_ts, lambda = seq(1.45, 1.6, 0.01))

lambda = 1.55

plot((LE_ts) ^ (lambda))
plot(diff((LE_ts) ^ (lambda)))

## trying estimating parameters with both transformations

# https://kimberlyannschveder.wordpress.com/2018/07/07/lake-erie-level-data/
# https://towardsdatascience.com/a-time-series-analysis-of-lake-erie-from-1921-to-1970-using-a-sarima-model-b79698df4762



# Note that the order of moving average corresponds to seasonal frequency, which is
# usually a first step in classical time series decomposition.

# step 1 - find seasonality
library(TSA)

# Fourier Transformation
p <- periodogram(LE_ts)
p$freq[which.max(p$spec)] # 1/12 USELESS LOL

p_df <- data.frame(freq=p$freq, spec=p$spec)

# step 2 - find entered MA by taking out the seasonality 
trend_beer = ma(timeserie_beer, order = 4, centre = T)
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))



