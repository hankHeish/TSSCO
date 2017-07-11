library(zoo)
library(xts)

#Step 1: Import Data
ID <- c("AAPL", "SPY", "QQQ", "GDX", "GLD")

GDX <- read.csv("C:/Users/J1060019/Desktop/dataFiles/GDX.csv", header = T)
GLD <- read.csv("C:/Users/J1060019/Desktop/dataFiles/GLD.csv", header = T)

GDX <- xts(GDX, order.by = as.Date(GDX$date))[, -1]
GLD <- xts(GLD, order.by = as.Date(GLD$date))[, -1]

#Step 2. Define Time Range
#Define in-sample time range
start_IN <- "2014-01-01"
end_IN <- "2016-01-31"
range_IN <- paste(start_IN, "::", end_IN, sep = "")

GDX_IN <- GDX[, 4][range_IN]
GLD_IN <- GLD[, 4][range_IN]
names(GDX_IN) <- "Clo.Price"
names(GLD_IN) <- "Clo.Price"

#Define out-of-sample time range
start_OUT <- "2016-02-01"
end_OUT <- "2016-12-16"
range_OUT <- paste(start_OUT, "::", end_OUT, sep = "")

GDX_OUT <- GDX[, 4][range_OUT]
GLD_OUT <- GLD[, 4][range_OUT]

#Step 3. Calculate Hedge Ratio
#Calculate Spread
GDX_IN$LagPrice <- lag(GDX_IN$Clo.Price)
Spread_GDX <- as.numeric(GDX_IN$Clo.Price) - as.numeric(GDX_IN$LagPrice)
GLD_IN$LagPrice <- lag(GLD_IN$Clo.Price)
Spread_GLD <- as.numeric(GLD_IN$Clo.Price) - as.numeric(GLD_IN$LagPrice)

#Calculate Hedge Ratio
model <- lm(Spread_GDX ~ Spread_GLD)
print(summary(model)$coef)
print(summary(model)$r.square)
HedgeRatio <- as.numeric(model$coefficients[2])

#Step 4. Calculate Residual 
Residual <- Spread_GDX - HedgeRatio * Spread_GLD
meanRes <- mean(Residual, na.rm = TRUE)
stdRes <- sd(Residual, na.rm = TRUE)

upperThread <- meanRes + 1 * stdRes
lowerThread <- meanRes - 1 * stdRes

#Visualize Spread 
plot(Residual, type = "l", main = "GDX v.s. GLD spread(In-sample period)")
abline(h = meanRes, col = "red", lwd = 2)
abline(h = upperThread, col = "darkgreen", lwd = 1.5)
abline(h = lowerThread, col = "darkgreen", lwd = 1.5)

#Arbitrage
Residual <- na.omit(Residual)
Sell <- which(Residual >= upperThread)
Buy <- which(Residual <= lowerThread)

priceBuy <- c(rep(0, length(Residual)))
priceSell <- c(rep(0, length(Residual)))
spread <- as.numeric(Residual)
tradeQty <- 100
totalQty <- 0

for (i in 1:length(Residual))
{
    SpreadTemp <- Residual[i]
    
    if (SpreadTemp < lowerThread)
    {
        totalQty <- totalQty - tradeQty
        priceBuy[i] <- SpreadTemp
    }else if (SpreadTemp > upperThread)
    {
        totalQty <- totalQty + tradeQty
        priceSell[i] <- SpreadTemp
    }
}

dev.new()
plot(Residual, type = "l", main = "GDX v.s. GLD spread(In-sample period)")
abline(h = meanRes, col = "red", lwd = 2)
abline(h = upperThread, col = "darkblue", lwd = 1.5)
abline(h = lowerThread, col = "darkblue", lwd = 1.5)









