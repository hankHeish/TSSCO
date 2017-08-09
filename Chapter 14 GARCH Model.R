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

data(bmw, package = "evir")









