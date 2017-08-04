#Statistics and Data Analysis for Financial Engineering Chapter 3
BondValue <- function(c, T, r, par)
{
    # c: coupon payment
    # T: time to maturity 
    # r: vertor of yield to maturity
    # par: par value
    return (c / r + (par - c / r) * (1 + r)^(-2 * T))
}

price = 120
C = 40
T = 30
par = 1000
r = seq(0.02, 0.05, length = 300)

value = BondValue(C, T, r, par)
yield2M <- spline(value, r, xout = price)
yieldToM <- uniroot(function(r) r^2 - 0.5, c(0.7, 0.8))

plot(r, value, xlab = "yield to maturity", ylab = "price of bond", type = "l", 
     main = "par = 1000, coupon payment = 40, T = 30", lwd = 2)
abline(h = 1200)
abline(v = yield2M)

#Graphing Yield Curves
mk.maturity <- read.csv("C:/Users/J1060019/Desktop/datasets/mk.maturity.csv", header = TRUE)
mk.zero2 <- read.csv("C:/Users/J1060019/Desktop/datasets/mk.zero2.csv", header = TRUE)

plot(mk.maturity[, 1], mk.zero2[5, 2:56], type = "l", col = "darkred", xlab = "maturity", ylab = "yield")
lines(mk.maturity[, 1], mk.zero2[6, 2:56], lty = 2, type = "l", col = "darkgreen")
lines(mk.maturity[, 1], mk.zero2[7, 2:56], lty = 3, type = "l", col = "darkblue")
lines(mk.maturity[, 1], mk.zero2[8, 2:56], lty = 4, type = "l", col = "lightgreen")
legend("bottomright", c("1985-12-01", "1986-01-01", "1986-02-01", "1986-03-01"), lty = 1:4)

intForward <- mk.maturity[, 1] * mk.zero2[6, 2:56]
xout <- seq(0, 20, length = 200)
z1 <- spline(mk.maturity[, 1], intForward, xout = xout)
forward <- diff(z1$y) / diff(z1$x)
T_grid <- (xout[-1] + xout[-200]) / 2
plot(T_grid, forward, type = "l", lwd = 2, ylim = c(0.06, 0.11))