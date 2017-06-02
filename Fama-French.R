#Fama-French Three Factor Model
library(quantmod)
library(xts)

stock <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 3/021/stock.txt", header = TRUE, sep = "\t")
HXBank <- stock[stock$Stkcd == 600015, ]
HXRet <- xts(HXBank$Dretwd, order.by = as.Date(HXBank$Trddt))
names(HXRet) <- "HXRet"

ThreeFactors <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 3/021/ThreeFactors.txt", header = TRUE, sep = "\t")
ThreeFactors <- xts(ThreeFactors[, -c(1, 2)], order.by = as.Date(ThreeFactors$TradingDate))

ThrFac <- ThreeFactors["2014-01-02/"]
ThrFac <- ThrFac[, -c(2, 4, 6)]

HXThreeFactor <- merge(HXRet, ThrFac, all = FALSE)

pairs(coredata(HXThreeFactor), pch = 20, col = 'blue', main = "pair plot of Return of HuaXiaBank and ThreeFactors")

regThrFac <- lm(HXRet~RiskPremium1+SMB1+HML1, data = HXThreeFactor)
