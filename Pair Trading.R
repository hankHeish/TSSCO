#Pair Trading 
library(xts)
library(urca)
library(tseries)
library(PairTrading)
library(PerformanceAnalytics)
library(quantmod)
library(TTR)

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

PLICC <- stock[, "Stk_601628"]["2009::2011"]
Sinopec <- stock[, "Stk_600028"]["2009::2011"]

prcdata <- merge(PLICC, Sinopec)

#EstimateParameters
PSreg <- EstimateParameters(prcdata, method = lm)

plot(PSreg$spread)

#EstimateParametersHistorically
BTreg <- EstimateParametersHistorically(prcdata, period = 250, method = lm)

#Test spread is stationary or not 
IsStationary(PSreg$spread, 0.5)

spread <- na.omit(BTreg$spread)
IsStationary(spread, 0.5)

signal <- Simple(spread, spread.entry = 0.06)

barplot(signal, col = "green", space = 0, border = "green", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
par(new = TRUE)
plot(spread)

#Pair Trading Return 
pairReturn <- Return(prcdata, lag(signal, 1), lag(na.omit(BTreg$hedge.ratio), 1))
names(pairReturn) <- "pairReturn"

charts.PerformanceSummary(pairReturn, main = "Pair Trade Return", geometric = FALSE)

#Backtesting Pair Return
Sinopec <- stock[, "Stk_600028"]
PLICC <- stock[, "Stk_601628"]

formStart <- "2009-01-30"
formEnd <- "2011-12-31"
formPeriod <- paste(formStart, "::", formEnd, sep = "")
Sinopecf <- Sinopec[formPeriod]
PLICCf <- PLICC[formPeriod]

log_PLICCf <- log(PLICCf)
#PLICC log price
summary(ur.df(log_PLICCf))
#PLICC return
summary(ur.df(diff(log_PLICCf)[-1]))

log_Sinof <- log(Sinopecf)
#Sinopec log price
summary(ur.df(log_Sinof))
#Sinopec return
summary(ur.df(diff(log_Sinof)[-1]))

#Test Cointegration
regrePS <- lm(log_PLICCf ~ log_Sinof)
summary(regrePS)

alpha <- coef(regrePS)[1]
beta <- coef(regrePS)[2]

spreadf <- log_PLICCf - alpha - beta * log_Sinof

UniRootf <- ur.df(spreadf, type = "none")
summary(UniRootf)

#Calculate Average mean and Standard Deviation
mu <- mean(spreadf)
sd <- sd(spreadf)

#Set trading period
PLICCT <- PLICC["2012-01-01/2012-06-30"]
SinopecT <- Sinopec["2012-01-01/2012-06-30"]

CoSpreadT <- log(PLICCT) - alpha - beta * log(SinopecT)
names(CoSpreadT) <- "CoSpreadT"

plot(CoSpreadT, ylim = c(-0.2, 0.2), main = "Spread Series")
abline(h = mu, col = "black", lwd = 1)
abline(h = c(mu + 0.2 * sd, mu - 0.2 * sd), col = "blue", lty = 6, lwd = 2)
abline(h = c(mu + 1.5 * sd, mu - 1.5 * sd), col = "green", lty = 2, lwd = 2.5)
abline(h = c(mu + 2.5 * sd, mu - 2.5 * sd), col = "red", lty = 3, lwd = 3)

#Simulation Trade
level <- c(mu - 2.5 * sd, mu - 1.5 * sd, mu - 0.2 * sd, 
           mu + 0.2 * sd, mu + 1.5 * sd, mu + 2.5 * sd)
interval <- function(x, level)
{
    prcLevel <- cut(x, breaks = c(-Inf, level, Inf))
    prcLevel <- as.numeric(prcLevel) - 4
}
prcLevel <- interval(CoSpreadT, level)

TradeSig <- function(prcLevel)
{
    n <- length(prcLevel)
    signal <- rep(0, n)
    
    for (i in (2:n))
    {
        if (prcLevel[i - 1] == 1 & prcLevel[i] == 2)
            signal[i] <- (-2)
        if (prcLevel[i - 1] == 1 & prcLevel[i] == 0)
            signal[i] <- 2
        if (prcLevel[i - 1] == 2 & prcLevel[i] == 3)
            signal[i] <- 3
        if (prcLevel[i - 1] == (-1) & prcLevel[i] == (-2))
            signal[i] <- 1
        if (prcLevel[i - 1] == (-1) & prcLevel[i] == 0)
            signal[i] <- (-1)
        if (prcLevel[i - 1] == (-2) & prcLevel[i] == (-3))
            signal[i] <- (-3)
    }
    
    return (signal)
}
signal <- TradeSig(prcLevel)

position <- c()
position[1] <- signal[1]
ns <- length(signal)

for (i in 2:ns)
{
    position[i] <- position[i - 1]
    
    if (signal[i] == 1)
        position[i] = 1
    if (signal[i] == (-2))
        position[i] = -1
    if (signal[i - 1] == 1 & signal[i] == (-1))
        position[i] = 0
    if (signal[i - 1] == (-1) & signal[i] == 2)
        position[i] = 0
    if (signal[i] == 3)
        break
    if (signal[i] == -3)
        break
}

position <- xts(position, order.by = index(CoSpreadT))

TradeSim <- function(PriceA, PriceB, Position)
{
    n <- length(Position)
    priceA <- as.numeric(PriceA)
    priceB <- as.numeric(PriceB)
    position <- as.numeric(Position)
    
    size <- 1000
    shareA <- size * position
    shareB <- c()
    shareB[1] <- (-beta) * shareA[1] * priceA[1] / priceB[1]
    cash <- c()
    cash[1] <- 2000
    
    for (i in 2:n)
    {
        shareB[i] <- shareB[i - 1]
        cash[i] <- cash[i - 1]
        
        if (position[i - 1] == 0 & position[i] == 1)
        {
            shareB[i] <- (-beta) * shareA[i] * priceA[i] / priceB[i]
            cash[i] <- cash[i - 1] - (shareA[i] * priceA[i] + shareB[i] * priceB[i])
        }
        if (position[i - 1] == 0 & position[i] == -1)
        {
            shareB[i] <- (-beta) * shareA[i] * priceA[i] / priceB[i]
            cash[i] <- cash[i - 1] + (shareA[i] * priceA[i] + shareB[i] * priceB[i])
        }
        if (position[i - 1] == 1 & position[i] == 0)
        {
            shareB[i] <- 0
            cash[i] <- cash[i - 1] + (shareA[i] * priceA[i] + shareB[i] * priceB[i])
        }
        if (position[i - 1] == -1 & position[i] == 0)
        {
            shareB[i] <- 0
            cash[i] <- cash[i - 1] - (shareA[i] * priceA[i] + shareB[i] * priceB[i])
        }
    }
    cash <- xts(cash, order.by = index(Position))
    shareA <- xts(shareA, order.by = index(Position))
    shareB <- xts(shareB, order.by = index(Position))
    asset <- cash + shareA * priceA + priceB * shareB
    account <- merge(Position, shareA, shareB, cash, asset)
    colnames(account) <- c("Position", "shareA", "shareB", "cash", "asset")
    
    return(account)
}

account<- TradeSim(PLICCT, SinopecT, position)

plot.zoo(account[, c(1, 4, 5), col = c("black", "blue", "red"), main = "Pair Trading Account"])

