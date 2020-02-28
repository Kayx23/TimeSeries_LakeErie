
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

# resources
# https://kimberlyannschveder.wordpress.com/2018/07/07/lake-erie-level-data/
# https://towardsdatascience.com/a-time-series-analysis-of-lake-erie-from-1921-to-1970-using-a-sarima-model-b79698df4762


