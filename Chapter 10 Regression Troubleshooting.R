# Statistics and Data Analysis for Financial Engineering 
# Chapter 10 Regression: Troubleshooting 

# Example 10.1. High-leverage points and residual outliers¡XSimulated data example
library(robust)
library(faraway)
set.seed(99)

x <- 1:11
x[11] <- 50
y <- 1 + x + rnorm(11)
y2 <- y
y2[11] <- y[11] - 45
x2 <- x
cexx <- c(rep(21, 10), 10)

par(mfrow = c(2, 2), lwd = 1)
plot(x, y, ylim = c(0, 60), cex = c(rep(1.25, 10), 1.5), pch = cexx, main = "(a)")
abline(lm(y ~ x), lwd = 2)
plot(x, y2, ylim = c(0, 60), cex = c(rep(1.25, 10), 1.5), ylab = "b", pch = cexx, main = "(b)")
abline(lm(y2 ~ x), lwd = 2)
plot(x2, y, ylim = c(0, 60), cex = c(rep(1.15, 10), 1.5), xlab = "x", pch = cexx, main = "(c)")
abline(lm(y ~ x2), lwd = 2)

# Example 10.2. Leverages in Example 10.1
par(mfrow = c(2, 2), lwd = 1, pch = 19)
plot(hatvalues(lm(y ~ x)), ylab = "leverage", main = "(a)", ylim = c(0, 1))
plot(hatvalues(lm(y2 ~ x)), ylab = "levergae", main = "(b)", ylim = c(0, 1))
plot(hatvalues(lm(y ~ x2)), ylab = "leverage", main = "(c)", ylim = c(0, 1))
plot(x2, hatvalues(lm(y ~ x2)), xlab = "x", ylab = "leverage", main = "(d)", ylim = c(0, 1))

# Example 10.3. Externally studentized and raw residuals in Example 10.1
par(mfrow = c(2, 3), lwd = 1, pch = 19)
plot(rstudent(lm(y ~ x)), ylab = "studentized residual", main = "Dataset (a)")
plot(rstudent(lm(y2 ~ x)), ylab = "studentized residual", main = "Dataset (b)")
plot(rstudent(lm(y ~ x2)), ylab = "studentized residual", main = "Dataset (c)")
plot(rstudent(lm(y ~ x)), ylab = "residual", main = "Dataset (a)")
plot(rstudent(lm(y2 ~ x)), ylab = "residual", main = "Dataset (b)")
plot(rstudent(lm(y ~ x2)), ylab = "residual", main = "Dataset (c)")

# Example 10.4. Cook¡¦s D for simulated data in Example 10.1
par(mfrow = c(2, 3), cex.axis = 1, cex.lab = 1, lwd = 1, pch = 19)
plot(sqrt(cooks.distance(lm(y ~ x))), ylab = "Square root Cook's D", cex = 1, main = "Dataset (a)", ylim = c(0, 11))
plot(sqrt(cooks.distance(lm(y2 ~ x))), ylab = "Square root Cook's D", cex = 1, main = "Dataset (b)", ylim = c(0, 11))
plot(sqrt(cooks.distance(lm(y ~ x2))), ylab = "Square root Cook's D", cex = 1, main = "Dataset (a)", ylim = c(0, 11))
halfnorm(sqrt(cooks.distance(lm(y ~ x))), ylab = "square root Cook's D", cex = 1, main = "Dataset (a)", xlim = c(0, 1.85))
halfnorm(sqrt(cooks.distance(lm(y2 ~ x))), ylab = "square root Cook's D", cex = 1, main = "Dataset (b)", xlim = c(0, 1.85))
halfnorm(sqrt(cooks.distance(lm(y ~ x2))), ylab = "square root Cook's D", cex = 1, main = "Dataset (c)", xlim = c(0, 1.85))

# Example 10.5. Weekly interest data with missing values recorded as zeros
data <- read.table(file = "C:/Users/J1060019/Desktop/datasets/WeekInt.txt", header = T)
cm10_diff <- diff(data$cm10)
aaa_dif <- diff(data$aaa)
cm30_diff <- diff(data$cm30)
ff_dif <- diff(data$ff)

fit <- lm(aaa_dif ~ cm10_diff + cm30_diff)
n <- length(cm30_diff)

par(mfrow = c(2, 2))
plot(hatvalues(fit), ylab = "Leverage", xlab = "Index", main = "(a)")
plot(2:n, rstudent(fit), ylab = "rstudent", xlab = "Index", main = "(b)")
plot(2:n, cooks.distance(fit), ylab = "Cook's D", xlab = "Index", main = "(c)")
plot(2:n, cooks.distance(fit), ylab = "Cook's D", xlim = c(368, 378), xlab = "Index", main = "(d)")

# Example 10.6. Detecting nonlinearity: A simulated data example
# Figur 10.6
n <- 80
set.seed("781235")
e <- matrix(runif(12 * n), nrow = n) %*% rep(1, 12)
e <- abs(e)^4
e <- e / mean(e)
x1 <- runif(n)
x1 <- sort(x1)
x2 <- rbeta(n, 6, 0.5)

y <- (8*x2 + x1 + 5*x1^3) + (4*x2 + x1 + 7*x1^3)

par(mfrow = c(1, 2))
plot(x1, y, xlab = expression(x[1]))
plot(x2, y, xlab = expression(x[2]))

# Figure 10.7
fit <- lm(y ~ x1 + x2)
rstudent <- rstudent(fit)

par(mfrow = c(1, 2))
qqnorm(rstudent, datax = T, main = "Normal QQ plot")
hist(rstudent, 12)

# Figure 10.8
par(mfrow = c(1, 3))
plot(x1, rstudent, main = "(a)", xlab = expression(x[1]))
fit2 <- loess(rstudent ~ x)
lines(x1, fit2$fitted, col = "red", lwd = 2)
plot(x2, rstudent, main = "(b)", xlab = expression(x[1]))
fit3 <- loess(rstudent ~ x2)
ordx2 <- order(x2)
lines(x2[ordx2], fit3$fitted[ordx2], col = "red", lwd = 2)
fitquad <- lm(y ~ poly(x1, 2) + x2)
rstudentquad <- rstudent(fitquad)
plot(fitquad$fitted, abs(rstudentquad), xlab = "fitted value", ylab = "abs(rstudent)", main = "(c)")
fit4 <- loess(abs(rstudentquad) ~ fitquad$fitted)
ord <- order(fitquad$fitted)
lines(fitquad$fitted[ord], fit4$fitted[ord], col = "red", lwd = 2)

# Figure 10.9
transy <- log(y)
fitquad2 <- lm(transy ~ poly(x1, 2) + x2)
rstudentquad2 <- rstudent(fitquad2)

par(mfrow = c(2, 2))
plot(x1, rstudentquad2, ylab = "rstudent", main = "(a)", xlab = expression(x[1]))
plot(x2, rstudentquad2, ylab = "rstudent", main = "(b)", xlab = expression(x[2]))
plot(fitquad$fitted, abs(rstudentquad2), xlab = "fitted values", ylab = "abs(rstudent)", main = "(c)")
fit5 <- loess(abs(rstudentquad2) ~ fitquad2$fitted)
ord <- order(fitquad2$fitted)
lines(fitquad2$fitted[ord], fit4$fitted[ord])
qqnorm(rstudentquad2, datax = T, main = "(d) normal plot")



