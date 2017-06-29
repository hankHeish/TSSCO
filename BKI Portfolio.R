library(xts)
library(zoo)

#read Stock historical price
#Stock <- read.csv("/Users/Heishminghan/Desktop/hist price.csv", header = TRUE, fileEncoding = 'big5')
Stock <- read.csv("C:/Users/J1060019/Desktop/hist price.csv", header = T)
Stock <- Stock[, -3]
names(Stock) <- c("Date", "Code", "Clo.Price")

code = c("2801", "2809", "2812", "2816", "2820", "2823", "2832", "2834", "2836", "2838", "2845"
         , "2849", "2850", "2851", "2852", "2855", "2856", "2867", "2880", "2881", "2882", "2883"
         , "2884", "2885", "2886", "2887", "2888", "2889", "2890", "2891", "2892", "5880", "6005")

StockList <- list()
for (i in 1:length(code))
{
    StockList[[code[i]]] = Stock[Stock$Code == code[i], ]
    StockList[[code[i]]] = xts(StockList[[code[i]]], order.by = as.Date(StockList[[code[i]]]$Date))
    StockList[[code[i]]] <- StockList[[code[i]]][, -c(1)]
    
    StockList[[code[i]]]$LagPrice <- lag(StockList[[code[i]]]$Clo.Price)
    StockList[[code[i]]] <- StockList[[code[i]]][-c(1), ]
    StockList[[code[i]]]$Return <- (as.numeric(StockList[[code[i]]]$Clo.Price) - as.numeric(StockList[[code[i]]]$LagPrice)) / as.numeric(StockList[[code[i]]]$LagPrice)
}

BKIRet <- matrix(0, length(StockList[[code[1]]]$Clo.Price), length(code))
colnames(BKIRet) <- code

for (i in 1:length(code))
    BKIRet[, i] <- StockList[[code[i]]]$Return

pairs(data.frame(BKIRet[, 19:29]), pch = 21, col = "darkgreen", main = "Correlations among 10 stocks return")

Corr <- cor(BKIRet)
