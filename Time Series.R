#Time Series 
library(zoo)
library(xts)
library(TTR)
library(urca)
library(forecast)

# Index <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 4/022/TRD_Index.txt", header = TRUE, sep = "\t")
# SHIndex <- Index[Index$Indexcd == 1, ]
# 
# ClsIndex <- SHIndex$Clsindex
# ClsIndex <- xts(ClsIndex, order.by = as.Date(SHIndex$Trddt))
# names(ClsIndex) <- "ClsIndex"
# 
# plot(ClsIndex, main = "Close Index")
# #hist(ClsIndex, breaks = 50, col = 'lightblue', broder = FALSE, main = "Histogram of ClsIndex")
# 
# SHRet <- xts(SHIndex$Retindex, order.by = as.Date(SHIndex$Trddt))
# plot(SHRet, main = "Return Series")
# 
# #Autocorrelation Coeffieient and Partial Autocorrelation Coeffieient
# acf(SHRet, lag.max = 40)
# pacf(SHRet, lag.max = 40)
# acf(ClsIndex, lag.max = 40)
# 
# #uni root test
# ur.df(SHRet, type = 'none')
# summary(ur.df(SHRet, type = 'none'))
# 
# #white noise 
# set.seed(100)
# WhiteNoise <- rnorm(500, mean = 0, sd = 1)
# plot(WhiteNoise, type = 'l', main = "white noise", col = 'darkblue')
# 
# #White Noise Test(Ljung-Box test)
# Box.test(SHRet, lag = 12, type = 'Ljung-Box')

#========================================
#use ARMA to Predict CPI
#========================================
#ARMA Model(Chapter 24)
CPI <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 4/024/CPI.csv", header = TRUE)
CPI <- xts(CPI[, -1], order.by = as.Date(CPI$time))
plot(CPI, main = "CPI")

CPITrain <- CPI[1:(length(CPI) - 3), ]
CPITrain <- na.omit(CPITrain)

#test Stationary
summary(ur.df(CPITrain, type = 'drift'))
#test White Noise 
Box.test(CPITrain, type = 'Ljung-Box', fitdf = 0, lag = 12)

#estimate ARMA p, q
acf(CPITrain)
pacf(CPITrain)

mod1 <- arima(CPITrain, order = c(1, 0, 1), method = "ML")
mod2 <- arima(CPITrain, order = c(1, 0, 2), method = "ML")
mod3 <- arima(CPITrain, order = c(1, 0, 3), method = "ML")
mod4 <- arima(CPITrain, order = c(2, 0, 1), method = "ML")
mod5 <- arima(CPITrain, order = c(2, 0, 2), method = "ML")
mod6 <- arima(CPITrain, order = c(2, 0, 3), method = "ML")
mod7 <- arima(CPITrain, order = c(3, 0, 1), method = "ML")
mod8 <- arima(CPITrain, order = c(3, 0, 2), method = "ML")
mod9 <- arima(CPITrain, order = c(3, 0, 3), method = "ML")

confint(mod1)

#Diagnostic Plots for Time-Series Fits
tsdiag(mod1)
tsdiag(mod1, gof.lag = 20)
acf(CPITrain, 40)

#use ARMA to predict data
predict(mod1, n.ahead = 3)

#========================================
#use ARMA to predict SH Index(auto.arima)
#========================================
data <- read.table("C:/Users/J1060019/Desktop/R Quant/part 4/024/TRD_Index2.txt", header = TRUE)
SHIndex <- data[data$Indexcd == 1, ]
return <- xts(SHIndex$Retindex, order.by = as.Date(SHIndex$Trddt))
return <- return["/2013-04-30"]

#ARMA Model
SHmod <- auto.arima(return, stationary = TRUE, seasonal = FALSE, ic = 'aic')
summary(SHmod)

confint(SHmod)
tsdiag(SHmod)
