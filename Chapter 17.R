#Statistics and Data Analysis for Financial Engineering 
#Chapter 17: The Capital Asset Pricing Model
#Estimation of Beta and Testing the CAPM
data <- read.csv("C:/Users/J1060019/Desktop/datasets/capm2.csv", header = T)

n <- dim(data)[1]
EX_R_sp500 <- data$Close.sp500[2:n] / data$Close.sp500[1:n - 1] - 1 - data$Close.tbill[2:n] / (100 * 253)
EX_R_msft <- data$Close.msft[2:n] / data$Close.msft[1:n - 1] - 1 - data$Close.tbill[2:n] / (100 * 253)

fit <- lm(EX_R_msft ~ EX_R_sp500)
options(digits = 3)
print(summary(fit))
par(mfrow = c(2, 2))
plot(fit)

fit_NoInt <- lm(EX_R_msft ~ EX_R_sp500 - 1)
options(digits = 3)
print(summary(fit_NoInt))
par(mfrow = c(2, 2))
plot(fit_NoInt)

#R Lib
data <- read.csv("C:/Users/J1060019/Desktop/datasets/Stock_Bond_2004_to_2006.csv", header = T)
prices <- data[, c(5, 7, 9, 11, 13, 15, 17, 24)]
n <- dim(prices)[1]

data2 <- as.matrix(cbind(data[2:n, 3] / 365, 100 * (prices[1:n - 1, ] - 1)))
names(data2)[1] <- "treasury"
risk_free <- data2[, 1]
ExRet <- data2[, 2:9] - risk_free