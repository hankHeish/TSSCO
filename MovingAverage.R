#Moving Average 
library(quantmod)
library(TTR)
library(xts)

#==============================
#Simple Moving Average
#==============================
TsingTao <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 5/030/TsingTao.csv", header = TRUE)
TsingTao <- xts(TsingTao[, -c(1, 2)], order.by = as.Date(TsingTao$Date))
close <- TsingTao$Close
ndate <- length(close)

plot(close, main = "TsingTao Bear Close Price")

# SMA5 <- apply(embed(close, 5), 1, mean)
# 
# close <- close[-(1:4), ]
# close <- merge(close, SMA5)
# 
# par(bg = "white", las = 2, lend = 1)
# plot(close[, 1], ylim = c(35, 50), type = "l", main = "TsingTao Bear")
# points(close[, 2], col = 'red', pch = "+")
# legend("topright", legend = c("close", "SMA5"), col = c("black", "red"), 
#        pch = c(NA_integer_, "+"), lty = c(1, 0))

smaCal <- function(ts, k = 5)
{
    n <- length(ts)
    sma <- c()

    for (i in k:n)
        sma[i] <- mean(ts[(i-k+1):i])
    sma <- xts(sma, order.by = index(ts))
    names(sma) <- "sma"
    
    return (sma)
}

sam5 <- smaCal(close, 5)

#==============================
#Weighted Moving Average
#==============================
# b <- 1:5
# w <- b / sum(b)
# 
# wec <- w * close
# names(wec) <- "WeightedClose"
# 
# WMA5 <- rep(0, ndate)
# for (i in 5:ndate)
#     WMA5[i] <- sum(w*close[(i-4):i])
# 
# WMA5 <- xts(WMA5, order.by = index(close))
# names(WMA5) <- "WMA5"
# 
# WMA5c1 <- merge(close, WMA5)
# WMA5c1 <- WMA5c1[-(1:4), ]
# 
# plot(WMA5c1[, 1], main = "TsingTao Bear Weighted Moving Average")
# points(WMA5c1[, 2], col = "pink", pch = "w", cex = 0.7)
# legend("topleft", legend = c("close", "WMA5"), col = c("black", "pink"), pch = c(NA_integer_, "w"), lty = c(1, 0), cex = 0.7)

wmaCal <- function(ts, weight)
{
    n <- length(ts)
    k <- length(weight)
    wma <- c()
    
    for (t in k:n)
        wma[t] = sum(ts[(t-k+1):t] * weight)
    wma <- xts(wma, order.by = index(ts))
    names(wma) <- "wma"
    
    return (wma)
}

wma5 <- wmaCal(close, c(0.1, 0.15, 0.2, 0.25, 0.3))

#==============================
#Exponential Weighted Moving Average 
#==============================
# EWMA5 <- rep(0, ndate)
# 
# ewma0 <- mean(close[1:5])
# EWMA5[5] <- ewma0
# e <- 1 / 5
# closem <- coredata(close)
# 
# EWMA5[6] <- e * closem[6] + (1 - e) * ewma0
# 
# for (i in 7:ndate)
# {
#     b <- sort(0:(i-6), decreasing = TRUE)
#     w <- (1 - e)^b
#     EWMA5[i] <- sum(e * closem[6:i] * w) + ewma0 * e^(i-5)
# }
# 
# EWMA5c1 <- merge(close, EWMA5)
# EWMA5c1 <- EWMA5c1[-(1:4), ]
# 
# plot(EWMA5c1[, 1], main = "TsingTao Bear Closing Price")
# points(EWMA5c1[, 2], col = "green", pch = "E", cex = 0.5)
# legend("topleft", legend = c("close", "EWMA"), col = c("black", "green"), pch = c(NA_integer_, "E"), lty = c(1, 0), cex - 0.5)

ewmaCal <- function(ts, k = 5, a = 0.2)
{
    n <- length(ts)
    ewma <- c()
    ewma[k] <- mean(ts[1:k])
    
    for (t in (k+1):n)
        ewma[t] = a * ts[t] + (1 - a) * ewma[t - 1]
    ewma <- xts(ewma, order.by = index(ts))
    names(ewma) <- "ewma"
    
    return (ewma)
}

ewma <- ewmaCal(close, 5, 0.2)