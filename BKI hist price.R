library(xts)
library(zoo)

#read Stock historical price
Stock <- read.csv("/Users/Heishminghan/Desktop/hist price.csv", header = TRUE, fileEncoding = 'big5')
Stock <- Stock[, -3]
code = c("2801", "2809", "2812", "2816", "2820", "2823", "2832", "2834", "2836", "2838", "2845"
         , "2849", "2850", "2851", "2852", "2855", "2856", "2867", "2880", "2881", "2882", "2883"
         , "2884", "2885", "2886", "2887", "2888", "2889", "2890", "2891", "2892", "5880", "6005")

StockList <- list()
for (i in 1:length(code))
{
    StockList[[code[i]]] = Stock[Stock$股票代號 == code[i], ]
    StockList[[code[i]]] = xts(StockList[[code[i]]], order.by = as.Date(StockList[[code[i]]]$日期))
    
#    StockList[[code[i]]] = c("Date", "Code", "ClosePrice")
}
