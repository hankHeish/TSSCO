#Statictics and Data Analysis for Financial Engineering 
#Chapter 13 Time Series Model: Further Topics 

# Figure 13.1
data(Hstarts, package = "Ecdat")
x <- ts(Hstarts[, 1], start = 1960, frequency = 4)

par(mfrow = c(1, 3))
plot(x, ylab = "log(starts)", type = "l", xlab = "year", main = "(a)")
acf(x, main = "(b)", xlab = "lag")
quart <- rep(1, 42) %x% (1:4)
boxplot(x ~ quart, xlab = "quarter", ylab = "log(starts)", main = "(c)")

# Figure 13.2
par(mfrow = c(3, 2))
plot(diff(x), xlab = "year", type = "l", main = "(a) nonseasonal differencing")
acf(diff(x), main = "(b) nonseasional differencing", xlab = "lag")

plot(diff(x, 4), type = "l", xlab = "year", main = "(c) seasional differencing")
acf(diff(x, 4), main = "(d) seasional differencing", xlab = "lag")

plot(diff(diff(x, 1), 4), type = "l", xlab = "year", 
     main = "(e) seasional & nonseasional differencing")
acf(diff(diff(x, 1), 4), main = "(f) seasional & nonseasional differencing")

# Example 13.1 ARIMA{(1, 1, 1)*(0, 1, 1)4} Model for Housing starts
data(Hstarts, package = "Ecdat")
x <- ts(Hstarts[, 1], start = 1960, frequency = 4)
fit1 <- arima(x, order =c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 4))
fit1

fit2 <- arima(x, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 4))
fit2

# Figure 13.3
pred <- predict(fit2, n.ahead = 16, newxreg = NULL, se.fit = TRUE)
year <- seq(1960.25, 2006, 0.25)
t1 <- 130:168
t2 <- 169:(169 + 15)

par(mfrow = c(1, 1))
plot(year[t1], x[t1], ylim = c(8.25, 10.3), xlim = c(1992.5, 2006), type = "b", lty = 2, 
     xlab = "year", ylab = "log(starts)", cex.axis = 1.5, cex.lab = 1.5)
points(year[t2], as.matrix(pred$pred), type = "b", pch = "*", lty = 3)
lines(year[t2], pred$pred - 2 * pred$se)
lines(year[t2], pred$pred + 2 * pred$se)
legend("topleft", c("data", "prediction", "lower CL", "upper CL"), 
       cex = 1.2, box.lty = 1, pch = c("o", "*", NA, NA), lty = c(2, 3, 1, 1))

# Figure 13.4
data("AirPassengers")
z <- as.ts(log(AirPassengers), stars = 1949, frequency = 12)

plot(z, type = "b", ylab = "passenger", cex.axis = 1.5, cex.lab = 1.5, lwd = 2)

# Example 13.2 Selecting a Transformation for the Housing Starts
library(forecast)
library(FitAR)

y <- exp(Hstarts[, 1])
par(mfrow = c(1, 1))
plot(y)

Hstart.arima <- arima(y, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 4))
BoxCox.Arima(Hstart.arima)

# Example 13.3 Residual Plot s for Weekly Interest Change
# Figure 13.6
library(car)
library(lmtest)

data <- read.table("C:/Users/J1060019/Desktop/datasets/WeekInt.txt", header = T)
# data <- read.table("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/WeekInt.txt", header = T)
cm10_dif <- diff(data$cm10)
aaa_dif <- diff(data$aaa)
cm30_dif <- diff(data$cm30)

fit <- lm(formula = aaa_dif ~ cm10_dif + cm30_dif)

par(mfrow = c(1, 1))
durbinWatsonTest(fit, max.lag = 1, rep = 1000)
durbinWatsonTest(fit, max.lag = 1, rep = 10000)
dwtest(fit, alternative = "two.sided", exact = F)
dwtest(fit, alternative = "two.sided", exact = T)

resid <- residuals(fit)

n <- length(resid)
tt <- (1:n) / (1 + n)

par(mfrow = c(2, 2))
qqnorm(resid, datax = T, main = "(a) Normal Plot",xlab = "theoretical quantiles", 
       ylab = "sample quantiles")
qqline(resid, datax = T)
qqplot(resid, (-0.00035 + 0.04058 * qt(tt, df = 3)), main = "(b) t-plot", 
       ylab = "theoretical quantiles")
abline(0, 1)
acf(resid, main = "(c)", xlab = "lag")
plot(fitted(fit), resid, main = "(d)", ylab = "Residuals", xlab = "fitted values")

auto.arima(resid, ic = "bic")
auto.arima(resid, ic = "aic")
length(resid)

# Example 13.4 Residual plots for weekly interest rate without differencing 
library(xts)
library(tseries)

data <- read.table("C:/Users/J1060019/Desktop/datasets/WeekInt.txt", header = T)
# data <- read.table("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/WeekInt.txt", header = T)
cm10_dif <- diff(data$cm10)
aaa_dif <- diff(data$aaa)
cm30_dif <- diff(data$cm30)

fit1 <- lm(aaa_dif ~ cm10_dif + cm30_dif)
fit2 <- lm(data$aaa ~ data$cm10 + data$cm30)

par(mfrow = c(1, 2))
dates <- as.Date(paste(data[, 2], data[, 1], data[, 3], sep = "/"), format = "%d/%m/%y")
plot(xts(residuals(fit2), order.by = dates), ylab = "Residuals", main = "(a)", type = "l")
acf(residuals(fit2), main = "(b) Residuals")

adf.test(residuals(fit2))
kpss.test(residuals(fit2))

# Example 13.5 Simulated Independent AR Process
set.seed(997711)
n <- 200

x <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
y <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
fit1 <- lm(y ~ x)
fit5 <- lm(diff(y) ~ diff(x))

x <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
y <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
fit2 <- lm(y ~ x)
fit6 <- lm(diff(y) ~ diff(x))

x <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
y <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
fit3 <- lm(y ~ x)
fit7 <- lm(diff(y) ~ diff(x))

x <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
y <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
fit4 <- lm(y ~ x)
fit8 <- lm(diff(y) ~ diff(x))

print(summary(fit1))
print(summary(fit2))
print(summary(fit3))
print(summary(fit4))
print(summary(fit5))
print(summary(fit6))
print(summary(fit7))
print(summary(fit8))

# Example 13.6 Regression with AR(1) errors
par(mfrow = c(1, 1))
phi <- c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)
x <- cbind(rep(1, 21), seq(-10, 10, 1))
se <- as.matrix(cbind(phi, rep(0, 7), rep(0, 7)))

for (i in 1:7)
{
    xx <- t(x) %*% x
    xxinv <- solve(xx)
    
    sig <- toeplitz(phi[i] ^ (0:20))
    
    cov <- xxinv %*% t(x) %*% sig %*% x %*% xxinv
    se[i, 2:3] <- t(sqrt(diag(cov)))
}

plot(se[, 1], se[4, 2] / se[, 2], type = "b", xlab = expression(phi), ylab = "SE ratio", 
     cex.axis = 1.5, cex.lab = 1.5, cex = 1.5, lwd = 2)
lines(se[, 1], se[4, 3] / se[, 3], type = "b", lty = "dashed", pch = "*", cex = 1.5, lwd = 2, col = "red")
legend("topright", c("intercept", "slope"), lty = c(1, 2), pch = c("o", "*"), 
       lwd = 2, cex = 1.5, col = c("black", "red"))

# Example 13.7 HC and HAC estimates for regression of weekly interest rate changes
library(sandwich)
library(car)

data <- read.table("C:/Users/J1060019/Desktop/datasets/WeekInt.txt", header = T)

cm10_dif <- diff(data$cm10)
aaa_dif <- diff(data$aaa)
cm30_dif <- diff(data$cm30)

fit <- lm(aaa_dif ~ cm10_dif + cm30_dif)
round(summary(fit$coef), 4)
options(digits = 2)

NeweyWest(fit, lag = 0, prewhite = F)
NeweyWest(fit, lag = 3, prewhite = F)

sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))
sqrt(diag(NeweyWest(fit, lag = 3, prewhite = F)))

coef(fit)/sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))
coef(fit)/sqrt(diag(NeweyWest(fit, lag = 3, prewhite = F)))

# Example 13.8 Demand for Ice Cream 
library(Ecdat)
data(Icecream)

fit_ic_lm <- lm(Icecream$cons ~ Icecream$income + Icecream$price + Icecream$temp)
summary(fit_ic_lm)

options(digits = 3)
durbinWatsonTest(fit_ic_lm)

fit_ic_ar <- arima(Icecream$cons, order = c(1, 0, 0), xreg = cbind(Icecream$income, Icecream$price, Icecream$temp))
print(fit_ic_ar, digits = 3)

fit_ic_ma <- arima(Icecream$cons, order = c(0, 0, 1), xreg = cbind(Icecream$income, Icecream$price, Icecream$temp))
print(fit_ic_ma, digits = 3)

par(mfrow = c(1, 3))
acf(fit_ic_lm$residuals, main = "linear model/indep. noise", xlab = "lag")
acf(fit_ic_ar$residuals, main = "linear model/AR(1) noise", xlab = "lag")
acf(fit_ic_ma$residuals, main = "linear model/MA(1) noise", xlab = "lag")

par(mfrow = c(2, 2))
plot(Icecream$cons, type = "b", main = "", xlab = "index")
plot(Icecream$temp, type = "b", main = "", xlab = "index")
plot(Icecream$income, type = "b", main = "", xlab = "index")
plot(Icecream$price, type = "b", main = "", xlab = "index")

# Example 13.9 Cross-correlation between changes in CPI (consumer price index) and IP (industrial production)
CPI.data <- read.csv("C:/Users/J1060019/Desktop/datasets/CPI.dat.csv", header = T)
IP.data <- read.csv("C:/Users/J1060019/Desktop/datasets/IP.dat.csv", header = T)

CPI <- as.matrix(CPI.data$CPI)[769:900]
IP <- as.matrix(IP.data$IP)[697:828]

cpi <- log(CPI)
ip <- log(IP)

cpi_diff1 <- diff(cpi)
ip_diff1 <- diff(ip)

par(mfrow = c(2, 2))
plot(ts(cpi, start = c(1977, 1), frequency = 12), xlab = "year", ylab = "log(CPI)", main = "(a)")
plot(ts(ip, start = c(1977, 1), frequency = 12), xlab = "year", ylab = "log(IP)", main = "(b)")
plot(ts(cpi_diff1, start = c(1977, 2), frequency = 12), xlab = "year", ylab = expression(paste(Delta, "log(CPI)")), main = "(a)")
plot(ts(ip_diff1, start = c(1977, 2), frequency = 12), xlab = "year", ylab = expression(paste(Delta, "log(IP)")), main = "(d)")

par(mfrow = c(1, 1), cex.axis = 1.35, cex.lab = 1.35, cex.main = 1.35)
ccf(cpi_diff1, ip_diff1, lwd = 3, ylab = "CCF", 
    main = expression(paste("corr{", Delat, "cpi(t), ", Delta, "ip(t - lag)}")))

# Figure 13.13
CPI_IP <- cbind(cpi_diff1, ip_diff1)
acf(CPI_IP)

source("SDAFE2.R")
mLjungBox(CPI_IP, lag = 10)

# Example 13.10 A Bivariate AR Model for cpi and ip
CPI_IP <- cbind(cpi_diff1, ip_diff1)
arFit <- ar(CPI_IP, order.max = 10)
options(digits = 2)
arFit$aic

arFit1 <- ar(CPI_IP, order.max = 1)
arFit1

colMeans(CPI_IP)

bPhi <- arFit1$ar[, , ]
bPhi2 <- bPhi %*% bPhi
bPhi3 <- bPhi2 %*% bPhi
bPhi4 <- bPhi3 %*% bPhi
bPhi5 <- bPhi4 %*% bPhi

epsilon.hat <- as.matrix(na.omit(arFit1$resid))
colnames(epsilon.hat) <- c("Resid Seeries 1", "Resid Series 2")

par(mfrow = c(1, 1), cex.axis = 1.15, cex.lab = 1.15, cex.main = 1.15)
acf(epsilon.hat)

# Example 13.11 Using a Bivariate AR(1) Model to Predict CPI and IP
x.n <- CPI_IP[131, ]
means <- colMeans(CPI_IP)
offset <- x.n - means
for (i in 1:10)
{
    offset <- bPhi %*% offset
    if (i == 1){
        forecasts <- offset
    }else{
        forecasts <- cbind(forecasts, means + offset)
    }
}
# plot(0:10, forecasts[2, ], type = "b", lty = 2, ylim = c(0, 0.006), pch = "*", cex = 3, 
#      xlab = "h", ylab = "forecast", cex.axis = 1.15, cex.lab = 1.15, lwd = 3)
plot(0:10,forecasts[2,],type="b",lty=2,ylim=c(0,.006),pch="*",cex=3,
     xlab="h",ylab="forecast",cex.axis=1.15,cex.lab=1.15,lwd=3)
lines(0:10, forecasts[1, ], type = "b", pch = "o", cex = 2, lwd = 3)
abline(h = means[1])
abline(h = means[2], lty = 2)
legend(2, 0.0015, c(expression(paste(Delta, "cpi")), expression(paste(Delta, "ip"))), 
       lty = c(1, 2), pch = c("o", "*"), cex = 1.6, lwd = 3, pt.cex = c(2, 3))

library(MASS)
arFit1 <- ar(CPI_IP, order.max = 1)

res <- arFit1$resid[7:126, ]
bPhi <- arFit1$ar[, , ]
bSigma <- arFit1$var.pred
means <- apply(CPI_IP, 2, mean)

set.seed(2015)
nither <- 50000
fore_CPI <- matrix(0, nrow = nither, ncol = 11)
fore_IP <- fore_CPI

for(k in 1:nither)
{
    forecasts <- t(CPI_IP[131, ])
    for (i in 1:11)
    {
        fore_CPI[k, i] <- forecasts[1]
        fore_IP[k, i] <- forecasts[2]
        fore_means <- means + bPhi %*% t(forecasts - means)
        forecasts <- t(mvnorm(n = 1, mu = fore_means, Sigma = bSigma))
    }
}





