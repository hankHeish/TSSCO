#Momentum
library(xts)
library(TTR)
library(quantmod)

Vanke <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 5/028/Vanke.csv", header = TRUE)
Vanke <- xts(Vanke[, -c(1, 2)], order.by = as.Date(Vanke$Date))

Close <- Vanke$Close
names(Close) <- "Vanke.Close"

lagClose <- lag(Close, 5)
names(lagClose) <- "lagClose"

VankeClose <- merge(Close, lagClose)
VankeClose <- na.omit(VankeClose)

momentum5 <- VankeClose$Vanke.Close - VankeClose$lagClose
names(momentum5) <- "Vanke.Momentum"

plot.zoo(merge(Close, momentum5), col = c("blue", "red"), main = "Vanke Momentum")

Momentum <- (Close - lagClose) / lagClose
Momentum <- na.omit(Momentum)
Momentum1 <- momentum(Close, n = 5, na.pad = TRUE)

Momentum1 <- merge(momentum5, Momentum1)

RocDis <- ROC(Close, n = 5, type = "discrete", na.pad = TRUE)
RocDis <- na.omit(RocDis)
names(RocDis) <- "Vanke.ROCDis"

Vanke2015 <- Vanke["2015"]
chartSeries(Vanke2015, theme = "white", name = "Vanke 2015", up.col = 'red', dn.col = 'green')
addTA(Cl(Vanke2015), on = 1, col = "black", type = "l")
addTA(Cl(Vanke2015), col = "black", type = "l")
addTA(momentum(Cl(Vanke2015), n = 35, na.pad = TRUE), col = 4, type = "l")

