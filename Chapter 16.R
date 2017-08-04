#Statistics and Data Analysis for Financial Engineering 
#Chapter 16: Portfolio Selection

library(Ecdat)
library(quadprog)

#16.6 Risk-Efficiency Portfolios with N Risky Asstes
data(CRSPday)
R <- 100 * CRSPday[, 4:6]
mean_vect <- apply(R, 2, mean)
cov_vect <- cov(R)
sd_vect <- sqrt(diag(cov_vect))
corr_vect <- cor(R)

#Set the Constraints Matrix
Amat <- cbind(rep(1, 3), mean_vect, diag(1, nrow = 3))

#Target Portfolio mean
#muP <- seq(0.05, 0.14, length = 300)
muP <- seq(min(mean_vect) + 0.0001, max(mean_vect) - 0.0001, length = 300)

#For the Expect Portfolio Return
sdP <- muP
weights <- matrix(0, nrow = 300, ncol = 3)

for (i in 1:length(muP))
{
    bvec <- c(1, muP[i], rep(0, 3))
    result <- solve.QP(Dmat = 2 * cov_vect,
                       dvec = rep(0, 3),
                       Amat = Amat,
                       bvec = bvec, 
                       meq = 2)
    sdP[i] <- sqrt(result$value)
    weights[i, ] <- result$solution
}

par(mfrow = c(1, 1))
#pdf("quad_prog_plot.pdf", width = 6, height = 5)
#plot(sdP, muP, type = "l", xlim = c(0, 0.25), ylim = c(0, 0.15), lty = 3, main = "Portfolio Selection")
plot(sdP, muP, type = "l", lty = 3, main = "Portfolio Selection")

mufree = 1.3 / 252
points(0, mufree, cex = 4, pch = "*")
sharpe <- (muP - mufree) / sdP

#Find maximum sharpe's ratio
ind <- (sharpe == max(sharpe))
weights[ind, ]
#Show line of Optimal Portfolio
lines(c(0, 2), mufree + c(0, 2) * (muP[ind] - mufree) / sdP[ind], lwd = 4, lty = 1, col = "darkblue")
#Tangency Portfolio
points(sdP[ind], muP[ind], cex = 2, pch = "*")

#Find Minimum Variance Portfolio
ind2 <- (sdP == min(sdP))
points(sdP[ind2], muP[ind2], cex = 2, pch = "+")

ind3 <- (muP > muP[ind2])
#lines(sdP[ind3], muP[ind3], type = "l", xlim = c(0, 0.25), ylim = c(0, 0.3), lwd = 3, col = "red")
lines(sdP[ind3], muP[ind3], type = "l", lwd = 3, col = "darkred")

text(sd_vect[1], mean_vect[1], "GE", cex = 1.15)
text(sd_vect[2], mean_vect[2], "IBM", cex = 1.15)
text(sd_vect[3], mean_vect[3], "Mobile", cex = 1.15)

#graphics.off()

#Utility
data <- read.csv("C:/Users/J1060019/Desktop/datasets/Stock_Bond.csv", header = T)
price <- data[, c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)]

n <- dim(price)[1]
m <- dim(price)[2] - 1
return <- price[-1, ] / price[-n, ] - 1

mean_vect <- colMeans(return)
cov_mat <- cov(return)
nlambda <- 250
loglambda_vect <- seq(2, 8, length = nlambda)
w_matrix <- matrix(nrow = nlambda, ncol = 10)
mu_vect <- matrix(nrow = nlambda, ncol = 1)

sd_vect <- mu_vect
ExUtil_vect <- mu_vect
conv_vect <- mu_vect

for (i in 1:nlambda)
{
    lambda <- exp(loglambda_vect[i])
    opt <- solve.QP(Dmat = as.matrix(lambda^2 * cov_mat),
                    dvec = lambda * mean_vect,
                    Amat = as.matrix(rep(1, 10)),
                    bvec = 1, 
                    meq = 1)
    w <- opt$solution
    mu_vect[i] <- w %*% mean_vect
    sd_vect[i] <- sqrt(w %*% cov_mat %*% w)
    w_matrix[i, ] <- w
    ExUtil_vect[i] <- opt$value
}

par(mfrow = c(1, 3))
plot(loglambda_vect, mu_vect, type = "l", col = "darkred")
plot(loglambda_vect, sd_vect, type = "l", col = "darkblue")
plot(sd_vect, mu_vect, type = "l", col = "darkgreen", main = "Efficiency Frontier")

# #16.10 R Lab
# #Efficiency Equity Portfolio
data <- read.csv("C:/Users/J1060019/Desktop/datasets/Stock_Bond.csv", header = T)
prices <- cbind(data$GM_AC, data$F_AC, data$CAT_AC, data$UTX_AC, data$MRK_AC, data$IBM_AC)
n <- dim(prices)[1]

returns <- (prices[-1, ] / prices[-n, ] - 1) * 100
pairs(returns, pch = 20, col = "darkgreen", main = "Correlation among 6 Stock's Return")

mean_vect <- colMeans(returns)
cov_mat <- cov(returns)
sd_vect <- sqrt(diag(cov_mat))

Amat <- cbind(rep(1, 6), mean_vect, diag(1, nrow = 6), diag(-1, nrow = 6))
muP <- seq(0.05, 0.08, length = 600)
sdP <- muP
weight <- matrix(0, nrow = 600, ncol = 6)

for (i in 1:length(muP))
{
    bvec <- c(1, muP[i], rep(-0.1, 6), rep(-0.5, 6))
    result <- solve.QP(Dmat = 2 * cov_mat, 
                       dvec = rep(0, 6), 
                       Amat = Amat, 
                       bvec = bvec,
                       meq = 2)
    sdP[i] <- sqrt(result$value)
    weight[i, ] <- result$solution
}

par(mfrow = c(1, 1))
plot(sdP, muP, type = "l", lty = 1.5, main = "Portfolio Selection")

#mufree <- 1.3 / 252
mufree <- 3 / 365
points(0, mufree, cex = 2.5, pch = "*")

SharpeRatio <- (muP - mufree) / sdP
index_max <- (SharpeRatio == max(SharpeRatio))
print(weight[index_max, ])
lines(c(0, 4), mufree + c(0, 4) * (muP[index_max] - mufree) / sdP[index_max], lwd = 2, lty = 1, col = "darkblue")
points(sdP[index_max], muP[index_max], cex = 1, pch = "+")

index_min <- (sdP == min(sdP))
points(sdP[index_min], muP[index_min], cex = 1, pch = "#")

index_effi <- (muP > muP[index_min])
lines(sdP[index_effi], muP[index_effi], cex = 1.5, type = "l", xlim = c(0, 0.25), ylim = c(0, 0.3), col = "darkred", lwd = 2)

brand <- c("GM", "F", "CAT", "UTX", "MRK", "IBM")
for (i in 1:length(brand))
    text(sd_vect[i], mean_vect[i], brand[i], cex = 1.5)








