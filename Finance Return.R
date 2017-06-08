#Calculate Finance Index return 
library(xts)
library(zoo)

#Historical Index Price
FinIndex <- read.csv("C:/Users/J1060019/Desktop/HisIndex.csv", header = TRUE)
FinIndex <- FinIndex[FinIndex$Code == "TWA28", ]
FinIndex <- na.omit(FinIndex)

#Historical stock price
code = c("2801", "2809", "2812", "2816", "2820", "2823", "2832", "2834", "2836", "2838", "2845", "2849", 
        "2850", "2851", "2852", "2855", "2856", "2867", "2880", "2881", "2882", "2883", "2884", "2885", 
        "2886", "2887", "2888", "2889", "2890", "2891", "2892", "5880", "6005")
Stock <- read.csv("C:/Users/J1060019/Desktop/hist price.csv", header = TRUE)
Stock <- xts(Stock[, -c(1, 3)], order.by = as.Date(Stock$日期))

stock2801 <- Stock[Stock$股票代號 == code[1], ]
lagStock2801 <- lag(stock2801)
Ret2801 <- (stock2801 - lagStock2801)/lagStock2801
Ret2801 <- Ret2801[, -1]

stock2809 <- Stock[Stock$股票代號 == code[2], ]
lagStock2809 <- lag(stock2809)
Ret2809 <- (stock2809 - lagStock2809)/lagStock2809
Ret2809 <- Ret2809[, -1]

stock2812 <- Stock[Stock$股票代號 == code[3], ]
lagStock2812 <- lag(stock2812)
Ret2812 <- (stock2812 - lagStock2812)/lagStock2812
Ret2812 <- Ret2812[, -1]

stock2816 <- Stock[Stock$股票代號 == code[4], ]
lagStock2816 <- lag(stock2816)
Ret2816 <- (stock2816 - lagStock2816)/lagStock2816
Ret2816 <- Ret2816[, -1]

stock2820 <- Stock[Stock$股票代號 == code[5], ]
lagStock2820 <- lag(stock2820)
Ret2820 <- (stock2820 - lagStock2820)/lagStock2820
Ret2820 <- Ret2820[, -1]

#names(Ret) <- c(code[1], code[2], code[3], code[4], code[5])

Ret <- merge(Ret2801, Ret2809, Ret2812, Ret2816, Ret2820)[-1]
names(Ret) <- c("2801", "2809", "2812", "2816", "2820")

pairs(data.frame(Ret), pch = 20, col = 'darkblue', main = "Return Correlation")

#cat("Stock", code[1], sep = "") <- Stock[Stock$股票代號 == code[1], ]

# for (i in 1:length(code))
# {
#     as.data.frame(cat("stock_", code[i])) <- Stock[Stock$股票代號 == "code[i]", ]
# }
# as.data.frame(cat("stock", code[1])) <- c(0, 0, 0)
# as.data.frame(cat("stock", code[1])) <- Stock[Stock$股票代號 == code[1], ]

# FinIndex <- xts(FinIndex[, -c(1:3)], order.by = as.Date(FinIndex$Date))
# lagIndex <- lag(FinIndex)
# Ret <- (FinIndex - lagIndex) / lagIndex
# 
# FinIndex <- merge(FinIndex, lagIndex, Ret)
