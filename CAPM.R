#CAPM
library(xts)

indexcd <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 3/020/TRD_Index.csv", header = TRUE)
mktcd <- indexcd[indexcd$Indexcd == 902, ]

mktret <- xts(mktcd$Retindex, order.by = as.Date(mktcd$Trddt))
mktret <- mktret["2014-01-02/2014/12/31"]
names(mktret) <- "mktret"

xin_an <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 3/020/xin_an.csv", header = TRUE)
xin_an <- xts(xin_an[, -c(1, 2)], order.by = as.Date(xin_an$Date))
xin_an <- xin_an[!xin_an[, 5] == 0]

xin_an_ret <- (xin_an[, 4] - lag(xin_an[, 4])) / lag(xin_an[, 4])[-1]
names(xin_an_ret) <- "return"

Ret <- na.omit(merge(mktret, xin_an_ret))
rf <- 1.036^(1/360) - 1
Eret <- Ret-rf

plot(coredata(Eret), col = 'darkgreen', pch = 20, main = "XinAn return and Market return")

summary(lm(Eret$return~Eret$mktret))