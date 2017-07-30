# Statistical and Data Analysis for Financial Engineering
# Chapter 15 Cointergration
library(forecast)
library(tseries)
library(urca)
library(xts)

# Examole 15.1 Phillips-Ouliaris test on bond yields
Yield_Data <- read.table("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/treasury_yields.txt", header = T)
date <- as.Date(Yield_Data[, 1], format = "%m/%d/%y")
dat <- as.xts(Yield_Data[, 3:7], date)

res <- residuals(lm(dat[, 3] ~ dat[, 1] + dat[, 2] + dat[, 4] + dat[, 5]))

par(mfrow = c(2, 4))
plot(dat[, 1], type = "l", main = "3-months", minor.tick = FALSE)
plot(dat[, 2], type = "l", main = "6-months", minor.tick = FALSE)
plot(dat[, 3], type = "l", main = "1-year", minor.tick = FALSE)
plot(dat[, 4], type = "l", main = "2-years", minor.tick = FALSE)
plot(dat[, 5], type = "l", main = "3-years", minor.tick = FALSE)
plot(res, type = "l", main = "residual", minor.tick = FALSE)
acf(res, lag.max = 40, xlab = "lag", ylab = "ACF", main = "ACF of Residuals")

po.test(dat[, c(3, 1, 2, 4, 5)])
