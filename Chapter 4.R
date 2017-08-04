#Statistics and Data Analysis for Financial Engineering 
#Chapter 4 Exploratory Data Analysis
library(faraway)
library(ggplot2)
library(fGarch)

data(SP500, package = "Ecdat")
SPreturn <- SP500$r500
n <- length(SPreturn)
year_SP <- 1981 + (1:n) * (1991.25 - 1981) / n

plot(year_SP, SPreturn, main = "S&P 500 daily return", xlab = "year", ylab = "log return", type = "l")

#Order Statistics, the Sample CDF, and Sample Quantiles
set.seed("991155")
edf_norm <- ecdf(rnorm(150))
plot(edf_norm, verticals = TRUE, do.p = FALSE, main = "EDF and CDF")

tt <- seq(from = -3, to = 3, by = 0.01)
lines(tt, pnorm(tt), lty = 2, lwd = 2, col = "darkred")
legend(1.5, 0.2, c("EDF", "CDF"), lty = c(1, 2), lwd = c(1.5, 2))

#Half-Normal Plot
data(Garch, package = "Ecdat")
diffdm <- diff(Garch$dm)

pdf("dm_halfnormal.pdf", width = 7, height = 6)
halfnorm(abs(diffdm), main = "changes in DM/dollar exchange rate", ylab = "Sorted data")
graphics.off()

#Quantile-Quantile Plot
qqplot(SPreturn, diffdm, xlab = "S&P return", ylab = "change in DM/dollar rate", main = "(a)")
xx <- quantile(SPreturn, c(0.25, 0.75))
yy <- quantile(diffdm, c(0.25, 0.75))
slope <- (yy[2] - yy[1]) / (xx[2] - xx[1])
inter <- yy[1] - slope * xx[1]
abline(inter, slope, lwd = 2)

#R Lib
EuStockMarkets <- read.csv("C:/Users/J1060019/Desktop/datasets/EuStockMarkets.csv", header = TRUE)
plot(EuStockMarkets)

logDAXR <- diff(log(EuStockMarkets$DAX))
logSMIR <- diff(log(EuStockMarkets$SMI))
logCACR <- diff(log(EuStockMarkets$CAC))
logFTSER <- diff(log(EuStockMarkets$FTSE))
logR <- cbind(logDAXR, logSMIR, logCACR, logFTSER)
pairs(data.frame(logR), pch = 21, col = 'darkblue', main = "Correlation Return")

par(mfrow = c(2, 2))
for (i in colnames(logR))
{
    qqnorm(logR[, i], datax = T, main = i)
    qqline(logR[, i], datax = T)
    print(shapiro.test(logR[, i]))
}

n <- dim(logR)[1]
q_grid <- (1:n) / (1 + n)
df_grid <- c(1, 4, 6, 10, 20, 30)
index.name <- dimnames(logR)[[2]]

for (i in 1:4)
{
    par(mfrow = c(3, 2))
    for (df in df_grid)
    {
        qqplot(logR[, i], qt(q_grid, df), main = paste(index.name[i], ", df = ", df))
        #qqplot(logR[, i], main = paste(index.name[i], ", df = ", df))
        abline(lm(qt(c(0.25, 0.75), df = df) ~ quantile(logR[, i], c(0.25, 0.75))))
    }
}

par(mfrow = c(1, 1))

x = seq(-0.1, 0.1, by = 0.01)
df = 5

mad_t <- mad(logR[, 1], constant = sqrt(df / (df - 2)) / qt(0.75, df))
plot(density(logR[, 1]), lwd = 2, ylim = c(0, 60))
lines(x, dstd(x, mean = mean(logR[, 1]), sd = mad_t, nu = df), lty = 5, lwd = 2, col = "red")
lines(x, dnorm(x, mean = mean(logR[, 1]), sd = sd(logR[, 1])), lty = 3, lwd = 4, col = "blue")
legend("topleft", c("KDE", paste("t: df = ", df), "normal"), lwd = c(2, 2, 4), lty = c(1, 5, 3), col = c("black", "red", "blue"))

#McDonald's Price and Returns 
data <- read.csv("C:/Users/J1060019/Desktop/datasets/MCD_PriceDaily.csv", header = TRUE)
adjPrice <- data[, 7]
plot(adjPrice, type = "l", lwd = 2)

LogRet <- diff(log(adjPrice))
hist(LogRet, breaks = 80, col = "darkgreen", main = "Return of McDonald's Price")
curve(dnorm(x, mean = mean(LogRet), sd = sd(LogRet)), add = TRUE, lty = 2, lwd = 2, col = "darkred")
