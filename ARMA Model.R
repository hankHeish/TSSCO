#ARMA Model
library(xts)

CPI <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 4/024/CPI.csv", header = TRUE)
CPI <- xts(CPI[, -1], order.by = as.Date(CPI$time))

CPITrain <- CPI[1:(length(CPI) - 3), ]