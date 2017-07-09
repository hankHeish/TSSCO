#Statistics and Data Analysis for Financial Engineering
#Chapter 12: Time Series Model: Bascis
library(forecast)

#12.3 Estimation Parameters of a Stationary Processes
data(Mishkin, package = "Ecdat")
y <- as.vector(Mishkin[, 1])
par(mfrow = c(1, 2))
acf(y)
acf(diff(y))

print(Box.test(y, lag = 10, type = "Ljung-Box"))
print(Box.test(diff(y), lag = 10, type = "Ljung-Box"))

#12.5 Estimation of AR(1) Processes
data(bmw, package = "evir")
print(Box.test(bmw, lag = 5, type = "Ljung-Box"))

fitAR1 <- arima(bmw, order = c(1, 0, 0))
print(fitAR1)

print(Box.test(residuals(fitAR1), lag = 5, type = "Ljung-Box", fitdf = 1))

#Inflation Rate
data(Mishkin, package = "Ecdat")
y <- as.vector(Mishkin[, 1])
fit <- arima(y, order = c(1, 0, 0))
print(Box.test(fit$residuals, type = "Ljung-Box", lag = 24, fitdf = 1))

#AR(p) Models
#print(Box.test(fit$residuals, type = "Ljung", lag = 24, fitdf = 2))
print(auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "aic"))
print(auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "bic"))

#12.7 Moving Average(MA) Processes
fitMA3 <- arima(diff(y), order = c(0, 0, 3))
print(fitMA3)

#12.9 ARIMA Processes
set.seed(4631)
y1 <- arima.sim(n = 500, list(ar = c(0.4)))
y2 <- cumsum(y1)
y3 <- cumsum(y2)

par(mfrow = c(3, 1))
plot(y1, type = "l", main = "I(0)")
plot(y2, type = "l", col = "darkred", main = "I(1)")
plot(y3, type = "l", col = "darkgreen", main = "I(2)")

#Fitting an ARIMA Model to CPI Data
CPI <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/CPI.dat.csv", header = T)
log_CPI <- log(CPI$CPI)

par(mfrow = c(3, 1))
plot(log_CPI, type = "l", main = "I(0)")
plot(diff(log_CPI), type = "l", col = "darkred", main = "I(1)")
plot(diff(log_CPI, differences = 2), type = "l", col = "darkgreen", main = "I(2)")

par(mfrow = c(2, 2))
acf(log_CPI)
acf(diff(log_CPI))
acf(diff(log_CPI, differences = 2))

fitARIMA <- arima(log_CPI, order = c(0, 2, 2))
acf(fitARIMA$residuals)

#Fitting an ARIMA Model to Industrial Production(IP) Data
IP <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/IP.dat.csv", header = T)
log_IP <- log(IP$IP)

par(mfrow = c(3, 1))
plot(log_IP, type = "l", main = "log_IP I(0)")
print(Box.test(log_IP, lag = 10, type = "Ljung-Box"))
plot(diff(log_IP), type = "l", col = "darkred", main = "log_IP I(1)")
print(Box.test(diff(log_IP), lag = 10, type = "Ljung-Box"))
plot(diff(log_IP, differences = 2), type = "l", col = "darkgreen", main = "log_IP I(2)")
print(Box.test(diff(log_IP, differences = 2), lag = 10, type = "Ljung-Box"))





