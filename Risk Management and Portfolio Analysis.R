library(fPortfolio)

data <- read.csv("C:/Users/J1060019/Desktop/dataFiles/returnsDaily24.csv", header = T)
datan <- as.timeSeries(data)

mySpec <- portfolioSpec()

setType(mySpec) <- c("MV", "CVaR")[1]

setSolver(mySpec) <- c("solveRquadprog", "solveRshortExact", "solveRglpk.CVAR")[1]

myCon <- c("LongOnly", "Short")[1]

setOptimize(mySpec) <- "minRisk"

load("C:/Users/J1060019/Desktop/dataFiles/myCOV.RData")
setEstimator(mySpec) <- "covEstimator"

tangencyPortfolio <- tangencyPortfolio(data = datan, spec = mySpec, constraints = myCon)
print(tangencyPortfolio)
GMVP <- minriskPortfolio(data = datan, spec = mySpec, constraints = myCon)
print(GMVP)

# id <- getWeights(GMVP) != 0
# getWeights(GMVP)[id]

ret_TangemcyP <- datan %*% as.numeric(getWeights(tangencyPortfolio))
rownames(ret_TangemcyP) <- rownames(datan)
colnames(ret_TangemcyP) <- "Tangency"
ret_TangemcyP <- as.timeSeries(ret_TangemcyP)

ret_GMVP <- datan %*% as.numeric(getWeights(GMVP))
rownames(ret_GMVP) <- rownames(datan)
colnames(ret_GMVP) <- "Tangency"
ret_GMVP <- as.timeSeries(ret_GMVP)

par(mfrow = c(2, 1))
plot(ret_TangemcyP, main = "Portfolio retruns of Tangency Portfolio", ylab = "", col = "blue")
plot(ret_GMVP, main = "Portfolio returns of GMVP Portfolio", ylab = "", col = "red")

#plot MV-Frontier Curve 
mySpec1 <- portfolioSpec()
setNFrontierPoints(mySpec1) <- 30
Frontier1 <- portfolioFrontier(datan, mySpec1)

#weight plots and other plots
par(mfrow = c(3, 1))
weightsPlot(Frontier1, mtext = FALSE)
text <- "Mean-Variance Portfolio - Long Only Constraints"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(Frontier1, mtext = F)
covRiskBudgetsPlot(Frontier1, mtext = F)

#Frontier Plot
par(mfrow = c(1, 1))
tailoredFrontierPlot(object = Frontier1, mText = "MV Portfolio - LongOnlyConstraints", risk = "Cov")
tailoredFrontierPlot(object = Frontier1, mText = "MV Portfolio - LongOnlyConstraints", risk = "CVaR")

#BackTesting
infile <- read.csv("C:/Users/J1060019/Desktop/dataFiles/.Close.csv", header = T)
myDate0 <- as.timeSeries(infile)
assetReturn <- returns(myDate0[, -51]) * 100

ID <- read.csv("C:/Users/J1060019/Desktop/dataFiles/file_twii50.csv", header = F)
ID <- ID$V2
colnames(assetReturn) <- ID

avg <- rowMeans(assetReturn)
newData <- cbind(avg, assetReturn)
colnames(newData) <- c("Lhs", colnames(assetReturn))

#Set Estimate Condition
portSpec <- portfolioSpec()
myConstraints <- "Long"

if (myConstraints == "Short")
{
    setSolver(portSpec) <- "solveRshortExact"
}else
{
    setSolver(portSpec) <- "solveRquadprog"
}

#Define Formula
Rhs <- paste(names(newData[, -1]), collapse = "+")
Formula <- paste("Lhs ~ ", Rhs, sep = "")

backtestSpec <- portfolioBacktest()
setSmootherLambda(backtestSpec) <- "3m"
setWindowsHorizon(backtestSpec) <- "12m"

#BackTesting
rawOutput <- portfolioBacktesting(formula = as.formula(Formula), data = newData, spec = portSpec, backtest = backtestSpec,
                                  constraints = myConstraints, trace = FALSE)
Weight <- round(100 * rawOutput$weights, 2)

smoothOutput <- portfolioSmoothing(object = rawOutput, trace = FALSE)
END <- time(tail(smoothOutput$portfolio, 2))
weightsDecision <- smoothOutput$smoothWeights[as.character(END), ]
round(t(cbind(weightsDecision)), 4)
smoothWeights <- round(100 * smoothOutput$smoothWeights, 2)
advice0 <- t(tail(smoothWeights, 1))
rownames(advice0) <- ID

backtestPlot(smoothOutput, which = "all", cex = 0.6, font = 1, family = "mono")