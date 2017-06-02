library(xts)
#library(xtsExtra)
library(quantmod)
library(TTR)
library(fPortfolio)
library(BLCOP)
# library(fAssets)
# library(MASS)
# library(quadprog)

sh_return <- read.csv("C:/Users/J1060019/Desktop/R Quant/part 3/019/ret.csv", header = TRUE)
sh_return <- xts(sh_return[, -1], order.by = as.Date(sh_return[, 1]))
sh_return <- na.omit(sh_return)
plot.zoo(sh_return, lty = 1:5, col = 1:5,  main = "Daily Return")

cumreturn <- cumprod(1 + sh_return)
plot.zoo(cumreturn, type = "l", lty = 1:5, ylim = c(0, 3.5), col = 1:5, main = "Cumulative Return")

#Correlation matrix 
pairs(data.frame(sh_return), pch = 20, col = 'darkgreen', main = "Correlation")

#Markowitz Model 
#minimize Risk 
min.var <- function(r_set, goal_return)
{
    n <- dim(r_set)[2]
    Q <- cov(r_set)
    
    r <- apply(r_set, MARGIN = 2, FUN = mean)
    L1 <- cbind(Q, rep(1, n), r)
    L2 <- rbind(c(rep(1, n), 0, 0), c(r, 0, 0))
    L <- rbind(L1, L2)
    b <- c(rep(0, n), 1, goal_return)
    
    solve.res <- solve(L, b)
    wt <- solve.res[1:n]
    
    return_mean <- r %*% wt
    return_variance <- wt %*% Q %*% wt
    
    return (c(return_mean, return_variance, wt))
}

step <- seq(-0.0002, 0.0012, by = 0.000002)
frontier_curve <- t(sapply(step, FUN = function(goal_return) min.var(sh_return, goal_return)))
plot(frontier_curve[, 2:1], pch = 20, main = "Frontier Curve", xlab = 'Variance', ylab = 'Goal Return')

#select train set and test set 
train_set <- sh_return["2009-01-01/2012-12-31"]
test_set <- sh_return["2013-01-01/2013-3-31"]

goal_return <- 0.001
portfolio <- min.var(train_set, goal_return)
portfolio_weight <- portfolio[3:length(portfolio)]

test_return <- test_set %*% portfolio_weight
test_return <- xts(test_return, order.by = index(test_set))
test_cum_return <- cumprod(1 + test_return)
plot.xts(test_cum_return)

L <- 100
sim_weight <- t(sapply(1:L, FUN = function(i)
    {
        weight <- runif(5, min = 0, max = 1)
        weight <- weight / sum(weight)

        return (weight)
    }))

sim_return <- sapply(1:L, FUN = function(i)
    {
        weight <- sim_weight[i, ]
        sim_ret <- test_set %*% weight

        return (sim_ret)
    })

sim_return <- xts(sim_return, order.by = index(test_set))
sim_cum_return <- cumprod(1 + sim_return)

plot.zoo(merge(sim_cum_return, test_cum_return), col = c(rep('lightgreen', 100), 'darkblue'), screens = 1)

#Black-Litterman Model
prior_mean <- colMeans(sh_return)
prior_cov_matrix <- cov(sh_return)

pick_matrix <- matrix(0, nrow = 2, ncol = 5)
colnames(pick_matrix) <- colnames(sh_return)

pick_matrix[1, 1:4]  <- 1
pick_matrix[2, c(1, 2, 5)] <- c(0.5, 0.5, -1)
q <- c(0.04, 0.022)
confidence <- c(85, 90)

views <- BLViews(pick_matrix, q, confidence, assetNames = colnames(sh_return))
posterior <- posteriorEst(views, tau = 1, prior_mean, prior_cov_matrix)

densityPlots(posterior, assetsSel = 'X600018')

optimal_portfolios <- optimalPortfolios.fPort(posterior, optimizer = "tangencyPortfolio")

weightsPie(optimal_portfolios$priorOptimPortfolio, main = "proir")
weightsPie(optimal_portfolios$posteriorOptimPortfolio, main = "post")
