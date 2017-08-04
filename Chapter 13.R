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

# Example 13.5 Simulated Independent AR Process
set.seed(997711)
n <- 200

x <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)
y <- arima.sim(list(order = c(1, 0, 0), ar = 0.99), n = n)

fit1 <- lm(y ~ x)
fit5 <- lm(diff(y) ~ diff(x))

print(summary(fit1))
par(mfrow = c(2, 2))
qqnorm(x)
qqnorm(fit1$residuals)
acf(fit1$residuals)
plot(fit1$residuals)
print(summary(fit5))