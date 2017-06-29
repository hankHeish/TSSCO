library(zoo)
library(xts)

#Step 1: Import Data
ID <- c("AAPL", "SPY", "QQQ", "GDX", "GLD")

GDX <- read.csv("C:/Users/J1060019/Desktop/dataFiles/GDX.csv", header = T)
GLD <- read.csv("C:/Users/J1060019/Desktop/dataFiles/GLD.csv", header = T)

GDX <- xts(GDX, order.by = as.Date(GDX$date))[, -1]
GLD <- xts(GLD, order.by = as.Date(GLD$date))[, -1]

#Define in-sample time range
start_IN <- "2014-01-01"
end_IN <- "2016-01-31"
range_IN <- paste(start_IN, "::", end_IN, sep = "")

GDX_IN <- GDX[, 4][range_IN]
GLD_IN <- GLD[, 4][range_IN]

#Define out-of-sample time range
start_OUT <- "2016-02-01"
end_OUT <- "2016-12-16"
range_OUT <- paste(start_OUT, "::", end_OUT)

GDX_OUT <- GDX[, 4][range_OUT]
GLD_OUT <- GLD[, 4][range_OUT]

#Calculate Spread
Spread_GDX <- diff(GDX)[-1]
Spread_GLD <- diff(GLD)[-1]

#Calculate Hedge Ratio
model <- lm(Spread_GDX ~ Spread_GLD - 1)

