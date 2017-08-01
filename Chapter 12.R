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

# Figure 12.4
phi <- c(0.95, 0.75, 0.2, -0.9)

par(mfrow = c(2, 2))
for (i in 1:4)
{
    y <- phi[i]^(0:15)
    plot(0:15, y, xlab = "h", ylab = expression(rho(h)), ylim = c(-1, 1), type = "l")
    points(0:15, y, pch = 8)
    text(10, -0.85, eval(substitute(expression(paste(phi, " = ", j)), list(j = as.character(phi[i])))), cex = 1.1)
    abline(h = 0)
}

#12.5 Estimation of AR(1) Processes
# Figure 12.5 
set.seed(8716)
e <- rnorm(200)
x1 <- rnorm(200)
x2 <- rnorm(200)
x3 <- rnorm(200)
x4 <- rnorm(200)

for (i in 2:200)
{
    x1[i] <- 0.98 * x1[i - 1] + e[i]
    x2[i] <- -0.6 * x2[i - 1] + e[i]
    x3[i] <- 1 * x3[i - 1] + e[i]
    x4[i] <- 1.01 * x4[i - 1] + e[i]
}
par(mfrow = c(2, 2))
plot(x1, type = "l", xlab = "Time(t)", ylab = expression(Y[t]), 
     main = expression(paste(phi, " = 0.98")))
plot(x2, type = "l", xlab = "Time(t)", ylab = expression(Y[t]), 
     main = expression(paste(phi, " = -0.6")))
plot(x3, type = "l", xlab = "Time(t)", ylab = expression(Y[t]), 
     main = expression(paste(phi, " = 1")))
plot(x4, type = "l", xlab = "Time(t)", ylab = expression(Y[t]), 
     main = expression(paste(phi, " = 1.01")))

# Figure 12.6
library(xts)

data(bmw, package = "evir")
BMW <- xts(bmw, attr(bmw, "times"))

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
par(cex.axis=1.15, cex.lab=1.15, cex.main=1.15)
plot(BMW, main = "(a)", minor.ticks = FALSE)
acf(BMW, lag.max = 20, main = "(b)")
qqnorm(bmw, main = "(c)")
qqline(bmw)

# Example 12.4 Daily log returns for BMW stock- ACF plots and AR fit
data(bmw, package = "evir")
print(Box.test(bmw, lag = 5, type = "Ljung-Box"))

fitAR1 <- arima(bmw, order = c(1, 0, 0))
print(fitAR1)

print(Box.test(residuals(fitAR1), lag = 5, type = "Ljung-Box", fitdf = 1))

# Figure 12.7
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
par(cex.axis=1.15, cex.lab=1.15, cex.main=1.15)
plot(xts(residuals(fitAR1), attr(bmw, "times")), main = "(a)", minor.ticks = FALSE)
acf(residuals(fitAR1), lag.max = 20, main = "(b)")
qqnorm(residuals(fitAR1), main = "(c)")
qqline(residuals(fitAR1))

# Example 12.5 Inflation Rate - AR(1) fit and checking residuals
data(Mishkin, package = "Ecdat")
y <- as.vector(Mishkin[, 1])
fit <- arima(y, order = c(1, 0, 0))
print(Box.test(fit$residuals, type = "Ljung-Box", lag = 24, fitdf = 1))

par(mfrow = c(1, 2))
acf(y, lag.max = 30, xlab = "Lag", ylab = "ACF", main = "Inflation Rate")
acf(residuals(fit), lag.max = 30, xlab = "Lag", ylab = "ACF", main = "Residauls form AR(1)")

# 12.6 AR(p) Models
# Figure 12.9
par(mfrow = c(1, 1))
x1 <- as.vector(ARMAacf(ar = c(0.5, -0.3), lag.max = 10))
x2 <- as.vector(ARMAacf(ar = c(0.5, 0.15), lag.max = 10))
x3 <- as.vector(ARMAacf(ar = c(0.15, 0.8), lag.max = 10))

plot(0:10, x1, xlab = "lag", ylab = "ACF", main = "ACF of three AR(2) process", cex.axis = 1.5, 
     cex.lab = 1.5, cex = 2, cex.main = 1.5, pch = "*", type = "b", ylim = c(-0.5, 1))
lines(0:10, x2, cex.axis = 1.5, cex.lab = 1.5, cex = 2, pch = "o", type = "b")
lines(0:10, x3, cex.axis = 1.5, cex.lab = 1.5, cex = 2, pch = "o", type = "b")
abline(h = 0)

# Figure 12.10
data(Mishkin, package = "Ecdat")
y <- as.vector(Mishkin[, 1])
x <- diff(y)
logn <- log(length(x))

resultsdiff <- matrix(0, nrow = 20, ncol = 3)
for (i in 1:20)
{
    fit <- arima(x, order = c(i, 0 ,0))
    resultsdiff[i, 1] <- i
    resultsdiff[i, 2] <- fit$aic
    resultsdiff[i, 3] <- resultsdiff[i, 2] + (logn - 2) * i
}
plot(resultsdiff[, 1], resultsdiff[, 2], xlab = "p", ylab = "criterion", 
     cex.lab = 1.35, cex.axis = 1.35, main = "AIC and BIC for AR fits to change in inflation rate", 
     cex.main = 1.35, cex = 2, pch = "*", ylim = c(2440, 2560), type = "b")
points(resultsdiff[, 1], resultsdiff[, 3], pch = "o", cex = 2, type = "b")
legend(12, 2565, c("AIC", "BIC"), pch = c("*", "o"), cex = 2, lty = 0)

# Example 12.6 Changes in the Inflation Rate - AR(p) models
#print(Box.test(fit$residuals, type = "Ljung", lag = 24, fitdf = 2))
print(auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "aic"))
print(auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "bic"))

#12.7 Moving Average(MA) Processes
# Fugure 12.11 
data(Mishkin, package = "Ecdat")
y <- as.vector(Mishkin[, 1])

par(mfrow = c(2, 2))
plot(ts(y, start = 1950, frequency = 12), ylab = "Inflation Rate", xlab = "Year", 
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.3, main = "(a)")
acf(y, main = "(b)")
plot(ts(fit$residuals, start = c(1950, 2), frequency = 12), ylab = "Inflation Rate", xlab = "Year", 
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.3, main = "(c)")
acf(fit$residuals, main = "(d)")

# Figuer 12.12
data(Mishkin, package = "Ecdat")
y <- as.vector(Mishkin[, 1])
x <- diff(y)
logn <- log(length(x))

##### Fitting MA(q) Models #####
resultsdiff <- matrix(0, nrow = 9, ncol = 3)
for (i in 1:9)
{
    fit <- arima(y, order = c(0, 0, i))
    resultsdiff[i, 1] <- i
    resultsdiff[i, 2] <- fit$aic
    resultsdiff[i, 3] <- resultsdiff[i, 2] + (logn - 2) * i
}

par(mfrow = c(1, 1))
plot(resultsdiff[, 1], resultsdiff[, 2], xlab = "q", ylab = "criterion", 
     cex.lab = 1.35, cex.axis = 1.35, main = "AIC and BIC for MA fits to change in inflation rate", 
     cex.main = 1.35, cex = 2, pch = "*", ylim = c(2445, 2560), type = "b")
points(resultsdiff[, 1], resultsdiff[, 3], pch = "o", cex = 2, type = "b")
legend(12, 2565, c("AIC", "BIC"), pch = c("*", "o"), cex = 2, lty = 0)


# Example 12.8 Changes in the Inflation Rate - MA Models 
fitMA3 <- arima(diff(y), order = c(0, 0, 3))
print(fitMA3)
Box.test(fitMA3$residuals, lag = 5, type = "Ljung-Box", fitdf = 3)
Box.test(fitMA3$residuals, lag = 10, type = "Ljung-Box", fitdf = 3)
Box.test(fitMA3$residuals, lag = 15, type = "Ljung-Box", fitdf = 3)

fitMA2 <- arima(diff(y), order = c(0, 0, 2))
print(fitMA2)
Box.test(fitMA2$residuals, lag = 5, type = "Ljung-Box", fitdf = 3)
Box.test(fitMA2$residuals, lag = 10, type = "Ljung-Box", fitdf = 3)
Box.test(fitMA2$residuals, lag = 15, type = "Ljung-Box", fitdf = 3)

# Example 12.9 Changes in risk-free returns - ARMA model
library(forecast)
data(Capm, package = "Ecdat")

rf <- Capm$rf
diffrf <- diff(rf)
acf(diffrf)
arima(rf, order = c(2, 1, 0))

res <- matrix(0, nrow = 9, ncol = 4)
i <- 1
for (p in 0:2)
{
    for (q in 0:2)
    {
        res[i, 1] <- p
        res[i, 2] <- q
        fit <- arima(diffrf, order = c(p, 0, q))
        res[i, 4] <- AIC(fit, k = logn) + 1290
        res[i, 3] <- AIC(fit) + 1290
        
        i <- i + 1
    }
}
options(digits = 3)
colnames(res) <- c("p", "q", "AIC", "BIC")
res

# Figure 12.13
par(mfrow = c(2, 1))
acf(diffrf)

bestfit <- arima(diffrf, order = c(1, 0, 1))
acf(bestfit$residuals)

layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))
par(cex.axis = 1.15, cex.lab = 1.15, cex.main = 1.15)
acf(bestfit$residuals, lag.max = 20, main = "(a)")
qqnorm(bestfit$residuals, datax = T, main = "(b)")
qqline(bestfit$residuals, datax = T)
plot(ts(bestfit$residuals, start = c(1960, 2), frequency = 12), ylab = "Residual", main = "(c)")

# 12.9 ARIMA Processes
# Figure 12.14
set.seed(4631)
y1 <- arima.sim(n = 500, list(ar = c(0.4)))
y2 <- cumsum(y1)
y3 <- cumsum(y2)

par(mfrow = c(3, 1))
plot(y1, type = "l", main = "I(0)")
plot(y2, type = "l", col = "darkred", main = "I(1)")
plot(y3, type = "l", col = "darkgreen", main = "I(2)")

# Example 12.10 Fitting an ARIMA Model to CPI Data
CPI.data <- read.csv("C:/Users/J1060019/Desktop/datasets/CPI.dat.csv", header = T)
CPI <- as.matrix(CPI.data$CPI)[769:900, ]
CPI_diff1 <- as.matrix(diff(log(CPI), diff = 1))
CPI_diff2 <- as.matrix(diff(log(CPI), diff = 2))

fit_ma <- arima(CPI_diff2, order = c(0, 0, 2))
Box.test(fit_ma$residuals, lag = 20, type = "Ljung-Box", fitdf = 2)

par(mfrow = c(3, 1), cex.axis = 1.3, cex.lab = 1.1, cex.main = 1.35)
plot(ts(log(CPI), start = c(1977, 1), frequency = 12), 
     xlab = "year", ylab = "log(CPI)", type = "b", main = "(a)")
plot(ts(as.vector(CPI_diff1), start = c(1977, 2), frequency = 12), 
     xlab = "year", ylab = expression(paste(Delta, "log(CPI)")), type = "b", main = "(b)")
plot(ts(as.vector(CPI_diff2), start = c(1977, 3), frequency = 12), 
     xlab = "year", ylab = expression(paste(Delta^2, "log(CPI)")), type = "b", main = "(c)")

# Example 12.11 Fitting an ARIMA Model to Industrial Production(IP) Data
IP <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/IP.dat.csv", header = T)
log_IP <- log(IP$IP)

par(mfrow = c(3, 1))
plot(log_IP, type = "l", main = "log_IP I(0)")
print(Box.test(log_IP, lag = 10, type = "Ljung-Box"))
plot(diff(log_IP), type = "l", col = "darkred", main = "log_IP I(1)")
print(Box.test(diff(log_IP), lag = 10, type = "Ljung-Box"))
plot(diff(log_IP, differences = 2), type = "l", col = "darkgreen", main = "log_IP I(2)")
print(Box.test(diff(log_IP, differences = 2), lag = 10, type = "Ljung-Box"))