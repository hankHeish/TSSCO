#Statistics and Data Analysis for Financial Engineering Chapter 5
#Fitting a t-distribution to changes in risk-free returns
library(fGarch)
library(fitdistrplus)

data(Capm, package = "Ecdat")
x <- diff(Capm$rf)
print(fitdistr(x, "t"))

n <- length(x)
start <- c(mean(x), std(x), 5)

loglike_t <- function(beta) sum(-dt((x - beta[1]) / beta[2], 
                                beta[3], log = TRUE) + log(beta[2]))

