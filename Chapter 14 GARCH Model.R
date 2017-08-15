# Statistical and Data Analysis for Financial Engineering 
# Chapter 14 GARCH Models

# Figure 14.1
library(xts)
library(evir)

data(Mishkin, package = "Ecdat")
infl <- as.vector(Mishkin[, 1])

data(SP500, package = "Ecdat")
SPretrun <- SP500$r500

data(Garch, package = "Ecdat")

data(Capm, package = "Ecdat")
difflogrf <- diff(log(Capm$rf))

diffdm <- diff(Garch$dm)
diffbp <- diff(Garch$bp)
diffcd <- diff(Garch$cd)
diffdy <- diff(Garch$dy)

n1 <- length(SPreturn)
year_SP <- 1981 + (1:n1) * (1991.25 - 1981) / n1
d1 <- seq(as.Date("1981-01-01"), as.Date("1991-04-01"), by = 1)
date_SP <- d1[seq(1, length(d1), by = length(d1)/n1)]

n2 <- length(diffdm)
year_dm <- 1980 + (1:n2) * (1987.25 - 1960) / n2
d2 <- seq(as.Date("1981-01-01"), as.Date("1987-07-01"), by = 1)
date_dm <- d2[seq(1, length(d2), by = length(d2)/n2)]

n3 <- length(difflogrf)
year_rf <- 1960 + (1:n3) * (2003 - 1960) / n3
d3 <- seq(as.Date("1960-01-01"), as.Date("2003-01-01"), by = 1)
date_rf <- d3[seq(1, length(d3), by = length(d3)/n3)]

n4 <- length(infl)
year_infl <- 1950 + 1/12 + (1:n4) * (1991 - 1950 - 1/12) / n4
d4 <- seq(as.Date("1950-02-01"), as.Date("1991-01-01"), by = 1)
date_infl <- d4[seq(1, length(d4), by = length(d4)/n4)]

par(mfrow = c(2, 2))
plot(xts(abs(SPretrun), date_SP), main = "(a) S&P 500 daily return", xlab = "year",
     type = "l", cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, ylab = "|log return|", ylim = c(0, 0.08),
     minor.ticks = F, major.ticks = "year", major.fotmat = "%Y")
mod <- loess(abs(diffbp) ~ year_dm, span = 0.25)
lines(xts(predict(mod), date_dm), col = 2)

plot(xts(abs(diffbp), date_dm), main = "(b) BP/dollar Exchange Rate", xlab = "year", 
     type = "l", cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, ylab = "|change in rate|",
     minor.ticks = F, major.ticks = "year", major.fotmat = "%Y")
mod <- loess(abs(diffbp) ~ year_dm, span = 0.25)
lines(xts(predict(mod), date_dm), col = 2)

plot(xts(abs(difflogrf), date_rf), main = "(C) Risk-Free Interest Rate", xlab = "year", 
     type = "l", cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, ylab = "|change in log(rate)|", 
     minor.ticks = F, major.ticks = "year", major.fotmat = "%Y")
mod <- loess(abs(difflogrf) ~ year_rf, span = 0.25)
lines(xts(predict(mod), date_rf), col = 2)

plot(xts(abs(infl - mean(infl)), date_infl), main = "(C) Inflation Rate", xlab = "year", 
     type = "l", cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, ylab = "|rate - mean(rate)|", 
     minor.ticks = F, major.ticks = "year", major.fotmat = "%Y")
mod <- loess(abs(infl - mean(infl)) ~ year_infl, span = 0.3)
lines(xts(predict(mod), date_infl), col = 2)

# Example 14.1 A Simulated ARCH(1) process and AR(1) + ARCH(1) process
# Figure 14.2
n <- 10200
e <- rnorm(n)
a <- e
y <- e
sig2 <- e^2
omega <- 1
alpha <- 0.55
phi <- 0.8
mu <- 0.1

sig2[1] <-omega/(1 - alpha)

set.seed(7484)
for(t in 2:n)
{
    a[t] <- sqrt(sig2[t]) * e[t]               # ARCH(1)
    y[t] <- mu + phi * (y[t - 1] - mu) + a[t]  # AR(1) + ARCH(1)
    sig2[t + 1] <- omega + alpha * a[t]^2
}

par(mfrow = c(2, 4))
plot(e[10001:n], type = "l", xlab = "t", ylab = expression(epsolon), main = "(a) white noise")
plot(sqrt(sig2[10001:n]), type = "l", xlab = "t", ylab = expression(sigma[t]), 
     main = "(b) conditional std. dev.")
plot(a[10001:n], type = "l", xlab = "t", ylab = "a", main = "(c) ARCH")
plot(y[10001:n], type = "l", xlab = "t", ylab = "a", main = "(d) AR+ARCH")

acf(a[10001:n], main = "(e) ARCH")
acf(a[10001:n]^2, main = "(f) ARCH squared")
acf(y[10001:n], main = "(e) AR + ARCH")
acf(y[10001:n]^2, main = "(f) AR + ARCH squared")

# Figure 14.3 
n <- 10500
set.seed(2340)
e <- rnorm(n)
a <- e
y <- e
sig2 <- e^2
omega <- 1
alpha <- 0.08
beta <- 0.9
phi <- 0.8
mu <- 0.1

for(t in 2:n)
{
    a[t] <- sqrt(sig2[t])*e[t]
    y[t] <- mu + phi * (y[t - 1] - mu) + a[t]
    sig2[t + 1] <- omega + alpha * a[t - 1]^2 + beta * sig2[t - 1]
}
par(mfrow = c(2, 4))
plot(e[10001:n], type = "l", xlab = "t", ylab = expression(epsolon), main = "(a) white noise")
plot(sqrt(sig2[10001:n]), type = "l", xlab = "t", ylab = expression(sigma[t]), 
     main = "(b) conditional std. dev.")
plot(a[10001:n], type = "l", xlab = "t", ylab = "a", main = "(c) GARCH")
plot(y[10001:n], type = "l", xlab = "t", ylab = "a", main = "(d) AR+GARCH")

acf(a[10001:n], main = "(e) GARCH")
acf(a[10001:n]^2, main = "(f) GARCH squared")
acf(y[10001:n], main = "(g) AR + GARCH")
acf(y[10001:n]^2, main = "(h) AR + GARCH squared")

# Example 14.2 AR(1) + GARCH(1, 1) Model to Fit Daily BMW Stock Log Returns
library(xts)
library(rugarch)
library(MASS)

data(bmw, package = "evir")

arma.garch.norm <- ugarchspec(mean.model = list(armaOrder = c(1, 0)), 
                              variance.model = list(garchOrder = c(1, 1)))
bmw.garch.norm <- ugarchfit(data = bmw, spec = arma.garch.norm)

print(bmw.garch.norm)
plot(bmw.garch.norm, which = "all")

e <- residuals(bmw.garch.norm, standardize = TRUE)
fitdist(e, "t")

n <- length(e)
grid <- (1:n)/(1 + n)
par(mfrow = c(1, 1))
qqplot(sort(as.numeric(e)), qt(grid, df = 4), 
       main = "t-plot, df = 4", xlab = "Standardized Residual Quantiles",
       ylab = "t-quantiles")
abline(lm(qt(c(0.25, 0.75), df = 4) ~ quantile(e, c(0.25, 0.75))))

par(mfrow = c(3, 2))
for(i in c(1, 3, 10, 11, 8, 9))
    plot(bmw.garch.norm, which = i)

arma.garch.t <- ugarchspec(mean.model = list(armaOrder = c(1, 0)), 
                           variance.model = list(garchOrder = c(1, 1)),
                           distribution.model = "std")
bmw.garch.t <- ugarchfit(data = bmw, spec = arma.garch.t)
print(bmw.garch.t)
par(mfrow = c(3, 2))
for(i in c(1, 3, 10, 11, 8, 9))
    plot(bmw.garch.t, which = i)

# Figure 14.6
rho <- function(a, b){
    c(1, a*(1 - a*b - b^2)/(1 - 2*a*b - b^2) * (a + b)^(0:9))
}

target <- 0.5

a1 <- 0.1
b1 <- uniroot(f = function(b){rho(a1, b)[2] - target}, interval = c(0, 1 - a1))$root

a2 <- 0.3
b2 <- uniroot(f = function(b){rho(a2, b)[2] - target}, interval = c(0, 1 - a2))$root

a3 <- 0.5
b3 <- uniroot(f = function(b){rho(a3, b)[2] - target}, interval = c(0, 1 - a3))$root

plot(0:10, rho(a1, b1), type = "b", ylim = c(0, 1), lty = 1, lwd = 2, 
     ylab = expression(paste(rho[a^2], "(h)")), xlab = "h")
lines(0:10, rho(a2, b2), type = "b", lty = 2, lwd = 2)
lines(0:10, rho(a3, b3), type = "b", lty = 3, lwd = 2)
legend("topright", c(expression(paste(alpha, " = 0.1, ", beta, " = 0.894")), 
                     expression(paste(alpha, " = 0.3, ", beta, " = 0.604")), 
                     expression(paste(alpha, " = 0.5, ", beta, " = 0"))),
       lty = 1:3, lwd = 2)

# Figure 14.7 ACF of the Squared Residuals form an AR(1) Fit to the BMW Log Returns
data(bmw, package = "evir")
res <- residuals(arima(bmw, order = c(1, 0, 0)))

acf(res^2)

# Example 14.4 Regression Analysis with ARMA + GARCH Errors of the Nelson-Plosser Data
library(forecast)
library(tseries)

nelsonplosser <- read.csv("C:/Users/J1060019/Desktop/datasets/nelsonplosser.csv", header = T)
new_np <- na.omit(nelsonplosser)
n <- nrow(new_np)

fit.lm1 <- lm(diff(log(new_np$sp)) ~ diff(log(new_np$ip)) + diff(new_np$bnd))
summary(fit.lm1)
resid.lm1 <- resid(fit.lm1)

auto.arima(resid.lm1)

xregression <- cbind(diff(log(new_np$ip)), diff(new_np$bnd))
fit.arma <- arima(diff(log(new_np$sp)), xreg = xregression, order = c(0, 0, 1))
print(fit.arma)
resid.arma <- fit.arma$res

fit.arch <- garch(resid.arma, order = c(0, 1), trace = F)
summary(fit.arch)
resid.arch <- fit.arch$residuals
sigma.arch <- as.numeric(fit.arch$fitted.values[, 1])

par(mfrow = c(1, 2))
ts.plot(sigma.arch)
acf(na.omit(resid.arch))

nelploss.arch.std.res <- as.numeric(resid.arch/sigma.arch)[-1]

par(mfrow = c(2, 2))
acf(rstudent(fit.lm1), main = "(a) regression: residuals")
acf(rstudent(fit.lm1)^2, main = "(b)regression: squared residuals")
acf(nelploss.arch.std.res, main = "(c) MA/ARCH: residuals")
acf(nelploss.arch.std.res^2, main = "(d) MA/ARCH: squared ")

fit.lm3 <- lm(diff(log(sp)) ~ diff(log(ip)) + diff(bnd), weights = 1/sigma.arch^2)
summary(fit.lm3)

par(mfrow = c(1, 1))
plot(fitted(fit.lm1)[-1], fitted(fit.lm3))
abline(0, 1)

# Example 14.5 Forcasting BMW log Returns
library(xts)
library(rugarch)
library(xts)

data(bmw, package = "evir")
n <- length(bmw)
date <- attr(bmw, "time")

origin1 <- 4100
data[origin1]
origin2 <- 3880
data[origin2]
nahead = 1500

garch.norm <- ugarchspec(mean.model = list(armaOrder = c(1, 0)),
                         variance.model = list(garchOrder = c(1, 1)))

bmw.garch.norm.1 <- ugarchfit(data = bmw[1:origin1], spec = garch.norm)
pred1 <- ugarchforecast(bmw.garch.norm.1, data = bmw[1:origin1], n.ahead = nahead)
head(fitted(pred1))
head(sigma(pred1))

bmw.garch.norm.2 <- ugarchfit(data = bmw[1:origin2], spec = garch.norm)
pred2 <- ugarchforecast(bmw.garch.norm.2, data = bmw[1:origin2], n.ahead = nahead)
head(fitted(pred2))
head(sigma(pred2))

BMW <- xts(bmw, attr(bmw, "times"))

plot(BMW, type = "l", xlab = "year", ylab = "log return", ylim = c(-0.13, 0.21), 
     xlim = c(date[3393], date[5219]), main = "Forcasting BMW Returns")
lines(date[(origin2 + 1):(origin2 + nahead)], fitted(pred2) + 1.96*sigma(pred2), col = 5, lwd = 4)
lines(date[(origin2 + 1):(origin2 + nahead)], fitted(pred2) - 1.96*sigma(pred2), col = 5, lwd = 4)

lines(date[(origin1 + 1):(origin1 + nahead)], fitted(pred1) + 1.96*sigma(pred1), col = 2, lwd = 4)
lines(date[(origin1 + 1):(origin1 + nahead)], fitted(pred1) - 1.96*sigma(pred1), col = 2, lwd = 4)

legend("topleft", c("11-15-87", "9-18-88"), lty = c(1, 2), lwd = 4, col = c(5, 2))

# Figure 14.11
data(CRSPday, package = "Ecdat")
CRSPday <- ts(CRSPday, start = c(1989, 1), frequency = 253)
ibm <- CRSPday[, 5] * 100
crsp <- CRSPday[, 7] * 100
Y <- cbind(ibm, crsp)

par(mfrow = c(2, 1))
plot(Y[, 1], type = "l", xlab = "year", ylab = "return (%)", main = "(a)")
plot(Y[, 2], type = "l", xlab = "year", ylab = "return (%)", main = "(b)")

# Figure 14.12
layout(rbind(c(1, 2), c(3, 3)), widths = c(1, 1, 2), heights = c(1, 1))
acf(as.numeric(Y[, 1]), ylim = c(-0.1, 0.1), main = "(a)")
acf(as.numeric(Y[, 2]), ylim = c(-0.1, 0.1), main = "(b)")
ccf(as.numeric(Y[, 1]), as.numeric(Y[, 2]), type = c("correlation"), 
    ylab = "CCF", lag = 20)

cor(ibm, crsp)

# MV Box-Ljung test for bivariate returns 
source("SDAFE2.R")
mLjungBox(Y, 5)

# Fitting an VAR(1) Model(for simplicity)
fit.AR1 <- ar(Y, aic = FALSE, order.max = 1)

A <- fit.AR1$resid[-1, ]
mLjungBox(A, 5)

# Figure 14.13
par(mfrow = c(2, 2))
acf(A[, 1]^2, ylim = c(-0.05, 0.35), main = "(a)")
acf(A[, 2]^2, ylim = c(-0.05, 0.35), main = "(b)")
ccf(A[, 1]^2, A[, 2]^2, type = c("correlation"), main = "(c)", ylab = "CCF", lag = 20)
acf(A[, 1]*A[, 2], ylim = c(-0.05, 0.35), main = "(d)")

# Figure 14.14
source("SDAFE2.R")













