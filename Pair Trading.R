#Pair Trading 
library(xts)
library(urca)
library(tseries)
library(PairTrading)
#library("PairTrading", lib.loc = "~/R/win-library/3.2")

stock <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 4/026/shstock.csv", header = TRUE)
stock <- xts(stock[-1], order.by = as.Date(stock[, 1]))

PLICC <- stock[, "Stk_601628"]
Sinopec <- stock[, "Stk_600028"]

formStart <- "2009-01-05"
formEnd <- "2010-05-31"
formPeriod <- paste(formStart, "::", formEnd, sep = "")

PLICCf <- PLICC[formPeriod]
Sinopecf <- Sinopec[formPeriod]

plot(PLICCf, ylim = c(0, 40), type = 'l')
lines(Sinopecf, col = "pink", lty = 6)
legend("topleft", legend = c("PLICC", "Sinopec"), col = c("darkblue", "pink"), lty = c(1, 6), cex = 0.7)

#price distance
SSD <- function(x, y)
{
    x <- log(x)
    y <- log(y)
    
    retx <- diff(x)[-1]
    rety <- diff(y)[-1]
    
    standardx <- cumprod(1 + retx)
    standardy <- cumprod(1 + rety)
    
    SSD <- sum((standardx - standardy)^2)
    
    return (SSD)
}

PSdistance <- SSD(PLICCf, Sinopecf)

#PLICC price
PLICClog <- log(PLICCf)
#uni root test
plogdf <- ur.df(coredata(PLICClog), type = 'none', lags = 1)

#PLICC return(log price difference)
retP <- diff(PLICClog)[-1]
#uni root test
retpdf <- ur.df(coredata(retP), type = 'none', lags = 1)

#Sinopec price
Sinolog <- log(Sinopecf)
#uni root test
Slogdf <- ur.df(coredata(Sinolog), type = 'none', lags = 1)

#Sinopec return(log price difference)
retS <- diff(Sinolog)[-1]
#uni root test
retSdf <- ur.df(coredata(retS), type = 'none', lags = 1)

plot.zoo(cbind(retP, retS), col = c("red", "black"), lty = c(2, 1), main = "log price diff")
legend("topleft", legend = c("PLICC", "Sinopec"), col = c("red", "black"), lty = c(2, 1), cex = 0.5)

#Cointegeration test
#OLS regression
regPS <- lm(PLICClog~Sinolog)

alpha <- coef(regPS)[1]
beta <- coef(regPS)[2]
spread <- PLICClog - alpha - beta * Sinolog
names(spread) <- "spread"

plot(spread, type = 'l', main = "Spread Series")

UniRoot <- ur.df(spread, type = 'none')

#Minimum Distance 
#Step 1. Setup trading condition
standardP <- cumprod(1 + retP)
standardS <- cumprod(1 + retS)

SSD_SP <- standardP - standardS
mean_SSD_SP <- as.numeric(mean(SSD_SP), na.rm = TRUE)
sd_SSD_SP <- as.numeric(sd(SSD_SP), na.rm = TRUE)

thredsholdUP <- mean_SSD_SP + 1.5 * sd_SSD_SP
thredsholdDOWN <- mean_SSD_SP - 1.5 * sd_SSD_SP

plot.zoo(SSD_SP, col = "darkred", ylim = c(-0.5, 0.06), main = "PLICc - Sinopec")
abline(h = mean_SSD_SP, col = "black", lty = 1, lwd = 1)
abline(h = c(thredsholdUP, thredsholdDOWN), col = "green", lty = 3, lwd = 2.5)

#Step 2. Setup trading period
tradeStart <- "2010-06-01"
tradeEnd <- "2010-12-31"
tradePeriod <- paste(tradeStart, "::", tradeEnd, sep = "")

PLICCT <- PLICC[tradePeriod]
SinopecT <- Sinopec[tradePeriod]

Calc_Spread <- function(x, y)
{
    x <- log(x)
    y <- log(y)

    retx <- diff(x)[-1]
    rety <- diff(y)[-1]
    
    standardX <- cumprod(1 + retx)
    standardY <- cumprod(1 + rety)

    spread <- standardX - standardY

    return (spread)
}

TradeSpread <- Calc_Spread(PLICCT, SinopecT)

plot(TradeSpread, ylim = c(-0.5, 0.15), main = "Trade Spread(Retrun)")
abline(h = mean_SSD_SP, col = "red", lty = 1, lwd = 1)
abline(h = c(thredsholdUP, thredsholdDOWN), col = "blue", lty = 4, lwd = 2)

#Cointegration Model
spreadf <- PLICClog - alpha - beta * Sinolog
mu <- mean(spreadf)
sd <- sd(spreadf)

CoSpreadT <- log(PLICCT) - alpha - beta * log(SinopecT)
names(CoSpreadT) <- "CoSpreadT"


plot(CoSpreadT, ylim = c(-0.1, 0.25), main = "Cointegration Spread(log Price)")
abline(h = mu, col = "red", lty = 1, lwd = 1)
abline(h = mu + 1.5 * sd, col = "blue", lty = 6, lwd = 2)
abline(h = mu - 1.5 * sd, col = "blue", lty = 6, lwd = 2)







