#Statistics and Data Analysis for Finacial Engineering Chapter 2
data <- read.csv("C:/Users/J1060019/Desktop/datasets/Stock_Bond.csv", header = TRUE)

names(data)
attach(data)

par(mfrow = c(1, 2))
plot(GM_AC, type = "l", col = "darkgreen")
plot(F_AC, type = "l", col = "darkred")

n <- dim(data)[1]
GMReturn <- GM_AC[-1] / GM_AC[-n] - 1
FReturn <- F_AC[-1] / F_AC[-n] - 1
par(mfrow = c(1, 1))
plot(GMReturn, FReturn)

niter <- 1e5
below <- rep(0, niter)
set.seed(2009)

#Simulation
for (i in 1:niter)
{
    r <- rnorm(45, mean = 0.05 / 252, sd = 0.23 / sqrt(252))
    
    logPrice <- log(1e6) + cumsum(r)
    minlogPrice <- min(logPrice)
    
    below[i] <- as.numeric(minlogPrice < log(950000))
}
mean(below)

#Simulating a Geometric Random Walk
set.seed(2012)
n = 252
par(mfrow = c(3, 3))

for (i in 1:9)
{
    logr <- rnorm(n, 0.05 / 252, 0.2 / sqrt(252))
    
    price <- c(120, 120 * exp(cumsum(logr)))
    plot(price, type = "l", col = "darkred")
}