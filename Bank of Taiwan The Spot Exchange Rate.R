#R Crawler 

library(rvest)
library(tmcn)
library(stringr)

#Bank of Taiwan: The Spot Exchange Rate\
url <- read_html("http://rate.bot.com.tw/xrt?lang=zh-tw")

#Get Exchange Rate Name
ExRate.dollar <- url %>%
    html_nodes(".print_show") %>%
    html_text() %>%
    toUTF8()
ExRate.dollar <- str_replace_all(ExRate.dollar, "[\r\n]", "")
ExgRate.dollar <- data.frame(currency = ExRate.dollar)

#Get Current Exchange Rate(Bid, Ask)
ExRate.currnet <- url %>%
    html_nodes(".rate-content-cash") %>%
    html_text()
ExgRate.current <- data.frame(current = ExRate.currnet)

#Get Spot Exchange Rate(Bid, Ask)
ExRate.Spot <- url %>%
    html_nodes(".rate-content-sight") %>%
    html_text()
ExgRate.spot <- data.frame(Spot = ExRate.Spot)

ExgRate <- matrix(0, length(ExRate.dollar), 5)

n <- dim(ExgRate.dollar)[1]
for (i in 1:n)
{
   ExgRate[i, 1] <- as.character(ExgRate.dollar[i, 1])
    ExgRate[i, 2] <- as.character(ExgRate.current[i * 2 + 2, 1])
    ExgRate[i, 3] <- as.character(ExgRate.current[i * 2 + 3, 1])
    ExgRate[i, 4] <- as.character(ExgRate.spot[i * 2 + 2, 1])
    ExgRate[i, 5] <- as.character(ExgRate.spot[i * 2 + 3, 1])
}
colnames(ExgRate) <- c("currency", "Cash Bid", "Cash Ask", "Spot Bid", "Spot Ask")

#Bank of Taiwan: The Foreign Exchange Rate 
url <- read_html("http://rate.bot.com.tw/ir?Lang=zh-TW")

#Get Exchange Rate Nmae 
ExgRate.name <- url %>%
    html_nodes(".tabletTableCurrency-TextIndent0px") %>%
    html_text() %>%
    toUTF8()
ExgRate.name <- str_replace_all(ExgRate.name, "[\r\n]", "")
ExgRate.name <- data.frame(Name = ExgRate.name)

#Get Exchange Rate(Both Current Account and Time Deposite)
ExgRate.CurAcct <- url %>%
    html_nodes(".text-right") %>%
    html_text()
ExgRate.CurAcct <- data.frame(Curr = ExgRate.CurAcct)

n <- dim(ExgRate.name)[1]
len <- dim(ExgRate.CurAcct)[1]
ForDeposit <- matrix(0, n, 10)

for (i in 1:n)
{
    ForDeposit[i, 1] <- as.character(ExgRate.name[i, 1])
}
for (i in 1:len)
{
    if (i %% 9){
        ForDeposit[ceiling(i / 9), (i %% 9) + 1] <- as.character(ExgRate.CurAcct[i, 1])
    }else{
        ForDeposit[ceiling(i / 9), 9 + 1] <- as.character(ExgRate.CurAcct[i, 1])
    }
}
colnames(ForDeposit) <- c("currency", "current account", "7 days", "14 days", "21 days", "1 month", 
                          "3 months", "6 months", "9 months", "1year")


