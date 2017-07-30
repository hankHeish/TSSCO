# Statistics and Data Analysis for Financial Engineering
# Chapter 18. Factor Models and Principal Compenents
library(magrittr)

# Example 18.2. Principal components analysis of yield curves
DataNoOmit <- read.table("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/treasury_yields.txt", header = T)
diffDataNoOmit <- diff(as.matrix(DataNoOmit[, 2:12]))

Data <- na.omit(DataNoOmit)
diffData <- diff(as.matrix(Data[, 2:12]))

n <- dim(diffData)[1]
options(digits = 5)
pca <- prcomp(diffData)
print(summary(pca))

par(mfrow = c(2, 2))
time <- c(1/12, 1/4, 1/2, 1, 2, 3, 5, 7, 10, 20, 30)
plot(time, as.vector(Data[1, 2:12]), ylim = c(0, 6), type = "b", lty = 1, lwd = 2, 
     ylab = "Yield", xlab = "T", main = "(a)")
lines(time, as.vector(Data[486, 2:12]), type = "b", lty = 2, lwd = 2, col = "red")
lines(time, as.vector(Data[n + 1, 2:12]), type = "b", lty = 2, lwd = 2, col = "blue")
legend("bottomright", c("07/31/01","07/02/07","10/31/08"), lty = c(1, 2, 3), lwd = 2, 
       cex = 0.2, col=c("black","red","blue"))

plot(pca, main = "(b)")

plot(time, pca$rotation[, 1], ylim = c(-0.8, 0.8), type = "b", lwd = 2, ylab = "PC", xlab = "T", main = "(c)")
lines(time, pca$rotation[, 2], lty = 2, type = "b", lwd = 2, col = "red")
lines(time, pca$rotation[, 3], lty = 3, type = "b", lwd = 2, col = "blue")
lines(0:30, 0*(0:30), lwd = 1)
legend("bottomright", c("PC1", "PC2", "PC3"), lty = c(1, 2, 3), lwd = 2, col = c("black", "red", "blue"))

plot(time, pca$rotation[, 1], ylim = c(-0.8, 0.8), type = "b", lwd = 2, ylab = "PC", xlab = "T",
     xlim = c(0, 3), main = "(d)")
lines(time, pca$rotation[, 2], lty = 2, type = "b", lwd = 2, col = "red")
lines(time, pca$rotation[, 3], lty = 3, type = "b", lwd = 2, col = "blue")
lines(0:30, 0*(0:30), lwd = 1)
legend("bottomright", c("PC1", "PC2", "PC3"), lty = c(1, 2, 3), lwd = 2, col = c("black", "red", "blue"))

# Fighure 18.2 
mu <- apply(Data[, 2:12], 2, mean)
par(mfrow = c(2, 2))
plot(time, mu, ylim = c(2.75, 4.65), type = "b", lwd = 4, xlab = "T", ylab = "Yield", 
     xlim = c(0, 7), main = "(a)", col = "black")
lines(time, mu + pca$rotation[, 1], lty = 5, type = "b", lwd = 2, col = "red")
lines(time, mu - pca$rotation[, 1], lty = 5, type = "b", lwd = 2, col = "blue")
legend("bottomright", c("mean", "mean + PC1", "mean - PC1"), lty = c(1, 5, 5), lwd = c(4, 2, 2),
       col = c("black", "red", "blue"))

plot(time, mu, ylim = c(2.75, 4.65), type = "b", lwd = 4, xlab = "T", ylab = "Yield", 
     xlim = c(0, 7), main = "(b)", col = "black")
lines(time, mu + pca$rotation[, 2], lty = 5, type = "b", lwd = 2, col = "red")
lines(time, mu - pca$rotation[, 2], lty = 5, type = "b", lwd = 2, col = "blue")
legend("bottomright", c("mean", "mean + PC2", "mean - PC2"), lty = c(1, 5, 5), lwd = c(4, 2, 2),
       col = c("black", "red", "blue"))

plot(time, mu, ylim = c(2.75, 4.65), type = "b", lwd = 4, xlab = "T", ylab = "Yield", 
     xlim = c(0, 7), main = "(c)", col = "black")
lines(time, mu + pca$rotation[, 3], lty = 5, type = "b", lwd = 2, col = "red")
lines(time, mu - pca$rotation[, 3], lty = 5, type = "b", lwd = 2, col = "blue")
legend("bottomright", c("mean", "mean + PC3", "mean - PC3"), lty = c(1, 5, 5), lwd = c(4, 2, 2),
       col = c("black", "red", "blue"))

par(lwd = 1)
plot(time, pca$rotation[, 4], ylim = c(-0.7, 0.7), type = "b", lwd = 3, ylab = "PC", xlab = "T", 
     xlim = c(0, 30), main = "d")
lines(time, pca$rotation[, 5], lty = 5, type = "b", lwd = 2, col = "red")
lines(0:30, 0*(0:30), lwd = 1)
legend("topright", c("PC4", "PC5"), lty = c(1, 5, 5), lwd = 2, col = c("black", "red"))

# Figure 18.3 
par(mfrow = c(1, 3))
for (i in 1:3){
    plot(pca$x[, i], main = paste0("PC", toString(i)), xlab = "day")
}

# Figure 18.4
acf(pca$x[, 1:3], ylab = "", xlab = "lag")

# Example 18.3 Principal components analysis of equity funds
EquityFund <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/equityFunds.csv", header = T)
pcaEq <- prcomp(EquityFund[, 2:9])
print(summary(pcaEq))

# Figure 18.5
par(mfrow = c(1, 2))
plot(pcaEq, main = "Scree plot")

plot(pcaEq$rotation[, 1], type = "b", ylab = "PC", lwd = 2, ylim = c(-1.5, 2), main = "(b)")
lines(pcaEq$rotation[, 2], type = "b", lwd = 2, col = "red")
lines(pcaEq$rotation[, 3], type = "b", lwd = 2, col = "blue")
lines(0:8, 0*(0:8), lwd = 1)
legend("top",c("PC1","PC2","PC3"),lty=c(1,2,3),lwd=2,cex=.65,col=c("black", "red", "blue"))
text(4.35,-1.25, "   EASTEU   LATAM   CHINA   INDIA   ENERGY   MINING   GOLD   WATER",cex=.38)

# Example 18.5 A macroeconomics factor model 
CPI.data <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/CPI.csv", header = T)
IP.data <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/IP.dat.csv", header = T)
berndtInvest <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/berndtInvest.csv", header = T)

berndt <- as.matrix(berndtInvest[, -1])

CPI <- CPI.data$CPI[775:900] %>% 
    log %>%
    diff %>%
    as.data.frame()
names(CPI) <- "CPI"

IP <- IP.data$IP[703:828] %>%
    log %>%
    diff %>%
    as.data.frame()
names(IP) <- "IP"

AR_fit <- ar(cbind(CPI, IP))
res <- AR_fit$resid[6:125, ]
lmfit <- lm(berndt[, 2:10] ~ res[, 1] + res[, 2])
slmfit <- summary(lmfit)
print(summary(lmfit))

rsq <- rep(0, 9)
for (i in 1:9){
    rsq[i] <- slmfit[[i]][[8]]
}
CPI_beta <- lmfit$coefficients[2, ]
IP_beta <- lmfit$coefficients[3, ]

par(mfrow = c(1, 3))
barplot(rsq, horiz = T, names = names(CPI_beta), main = "R Squared")
barplot(CPI_beta, horiz = T, main = "CPI beta")
barplot(IP_beta, horiz = T, main = "IP beta")

# Example 18.6 Fitting the Fama-French model to GE, IBM and Mobil
library(Ecdat)
library(robust)
data(CRSPmon)
FF_Data <- read.table("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/FamaFrench_mon_69_98.txt", header = T)

ge <- 100 * CRSPmon[, 1] - FF_Data$RF
ibm <- 100 * CRSPmon[, 2] - FF_Data$RF
mobil <- 100 * CRSPmon[, 3] - FF_Data$RF

fit <- lm(cbind(ge, ibm, mobil) ~ FF_Data$Mkt.RF + FF_Data$SMB + FF_Data$HML)
options(digits = 3)
print(summary(fit))

pairs(cbind(ge, ibm, mobil, FF_Data$Mkt.RF, FF_Data$SMB, FF_Data$HML), 
      col = c(rainbow(1), rainbow(2), rainbow(3), rainbow(4), rainbow(5), rainbow(6)), 
      panel = panel.smooth)
cor(fit$residuals)
pairs(cbind(ge, ibm, mobil), col = c(rainbow(1), rainbow(2), rainbow(3)), panel = panel.smooth)
covRob(data = fit$residuals, corr = T)

# Example 18.7 Estimating the Covariance Matrix of GE, IBM and Mobil Excess Return 
sigF <- cbind(FF_Data$Mkt.RF, FF_Data$SMB, FF_Data$HML) %>%
    var %>%
    as.matrix()

bbeta <- as.matrix(fit$coefficients)
bbeta <- t(bbeta[-1, ])

n <- dim(CRSPmon)[1]
sigeps <- (n - 1) / (n - 4) * as.matrix(var(as.matrix(fit$residuals))) 
sigeps <- diag(as.matrix(sigeps))
sigeps <- diag(sigeps, nrow = 3)

cov_equities <- bbeta %*% sigF %*% t(bbeta) + sigeps

options(digits = 3)
print(sigF)
print(bbeta)
print(sigeps)
print(cov_equities)
print(cov(cbind(ge, ibm, mobil)))

# Example 18.8 An Industry Cross-Sectional Factor Model
berndtInvest <- read.csv("/Users/Heishminghan/Desktop/Statistics and Data Analysis for Financial Engineering/berndtInvest.csv", header = T)
returns <- berndtInvest[, -c(1, 11, 18)]

ind_codes = as.factor(c(3,3,2,1,1,2,3,3,1,2,2,3,1,2,3))
codes = as.matrix(model.matrix(~ind_codes))
codes[,1] =  1 - codes[,2] - codes[,3]

betas = as.data.frame(codes)
colnames(betas) <- c("tech","oil","other")
rownames(betas) <- colnames(berndtInvest[, -c(1, 11, 18)])

factors <- matrix(0, nrow = 120, ncol = 3)
for (i in 1:120)
{
    return_data <- cbind(t(returns[i, ]), betas)
    colnames(return_data)[1] <- "return"
    lmfit <- lm(return ~ return_data[, 2] + return_data[, 3], data = return_data)
    factors[i, ] <- lmfit$coefficients
}

par(mfrow = c(1, 3))
plot(factors[, 1], type = "b", lty = "dotted", lwd = 2, 
     xlab = "month", ylab = "factor", main = "market")
plot(factors[, 2], type = "b", lty = "dotted", lwd = 2, 
     xlab = "month", ylab = "factor", main = "technology")
plot(factors[, 2], type = "b", lty = "dotted", lwd = 2, 
     xlab = "month", ylab = "factor", main = "oil")

print(cor(factors))
options(digits = 3)
cov(factors) %>%
    diag %>%
    sqrt %>%
    print
acf(factors[, 1:3], xlab = "lag", ylab = "")










