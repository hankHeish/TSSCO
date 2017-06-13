#ARCH Model, GARCH Model
library(xts)
library(FinTS)
library(rugarch)
library(parallel)

data <- read.table("C:/Users/J1060019/Desktop/R Quant/part 4/025/TRD_Index2.txt", header = TRUE)
par(mfrow = c(2, 1))
SHIndex <- data[data$Indexcd == 1, ]

returns = xts(SHIndex$Retindex, order.by = as.Date(SHIndex$Trddt))
returns = returns['2009-01-05/2013-04-26']

par(mfrow = c(2, 1))
plot(returns^2, type = 'l', main = "Squared Daily Return of SH Index")
plot(abs(returns), type = 'l', main = "Absolute Daily Return")

#LB test for AutoCorrelation
Box.test(returns^2, lag = 12, type = 'Ljung-Box')

#LM test for ARCH
ArchTest(returns)

#GARCH Model
garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(2, 2)))
mod_garch <- ugarchfit(spec = garch11_spec, data = returns)
