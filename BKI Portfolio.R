library(xts)
library(quantmod)
library(TTR)
library(fPortfolio)

histPrice <- read.csv("C:/Users/J1060019/Desktop/hist price.csv", header = TRUE)
histPrice <- histPrice[, -3]

names(histPrice) <- c("Date", "Code", "Clo.Price")

code <- c("2801", "2809", "2812", "2816", "2820", "2823", "2832", "2834", "2836", "2838", "2845"
          , "2849", "2850", "2851", "2852", "2855", "2856", "2867", "2880", "2881", "2882", "2883"
          , "2884", "2885", "2886", "2887", "2888", "2889", "2890", "2891", "2892", "5880", "6005")

n <- dim(histPrice)[1]

for (i in 2:n)
{
    if (histPrice[i, 2] == lag(histPrice[i - 1, 2]))
        histPrice[i, 4] <- lag(histPrice[i - 1, 3])
}

histPrice[, 5] <- (histPrice[, 3] - histPrice[, 4]) / histPrice[, 4]
histPrice <- na.omit(histPrice)
names(histPrice) <- c("Date", "Code", "Clo.Price", "LagPrice", "DailyRet")
histPrice <- histPrice[, -c(3, 4)]

Ret2881 <- histPrice[histPrice$Code == "2881", ]
Ret2881 <- xts(Ret2881, order.by = as.Date(Ret2881$Date))[, -c(1, 2)]

for (i in 1:length(code))
{
    as.data.frame(paste("Ret", code[i], sep = "")) <- histPrice[histPrice$Code == code[i], ]
}

#https://stackoverflow.com/questions/43350988/target-of-assignment-expands-to-non-language-object