x <- seq(0.5, 2, length.out = 100)

BoxCox <- function(y, lambda)
{
    (y ^ lambda - 1) / lambda
}
lambda <- seq(-2, 3, 0.5)

plot(0, 0, type = "n", xlim = c(0.5, 2), ylim = c(-2, 2.5), main = "Box-Cox Transformation")

for (i in 1:length(lambda))
{
    points(x, BoxCox(x, lambda[i]), type = "l", col = i)
    points(2, BoxCox(2, lambda[i]), col = i, pch = i)
}

legend(0.7, 2.5, legend = as.character(rev(lambda)), lty = 1,
       pch = length(lambda):1, col = length(lambda):1)

x <- seq(-4, 4, 0.1)
plot(x, dnorm(x), type = "l", main = "N(0, 1)")
curve(dnorm(x), from = -4, to = 4)

x <- 0:50
plot(x, dbinom(x, size = 50, prob = 0.33), type = "h")

#Use Normal Distribution to Approximate Binomial Distribution
par(mfrow = c(1, 2))
n <- 20
p <- 0.4
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

x <- 0:20
plot(x, dbinom(x, n, p), type = "h", lwd = 2,
     xlab = "x", ylab = "P(X = x)", main = "B(20, 0.4)")
z <- seq(0, n, 0.1)
lines(z, dnorm(z, mu, sigma), col = "darkred", lwd = 2)
abline(h = 0, lwd = 2, col = "grey")

#Bernoulli
par(mfrow = c(1, 1))

sample.size <- seq(from = 1, to = 800, by = 5)
m <- length(sample.size)
xbar <- numeric(m)

for (i in 1:m)
{
    xbar[i] <- mean(rbinom(sample.size[i], 1, 0.5))
}
plot(sample.size, xbar, xlab = "Number of observations, n", ylab = "sample mean", main = "Law of Large Number", 
     type = "l", col = "darkred", lwd = 0.5)
abline(h = 0.5, col = "blue")

#Unoform Distribution
Umin <- 5
Umax <- 80
n.sample <- 20
n.repeated <- 500

RandomSample <- matrix(0, n.sample, n.repeated)
for (i in 1:n.repeated)
{
    rnumber <- runif(n.sample, Umin, Umax)
    RandomSample[, i] <- as.matrix(rnumber)
}

par(mfrow = c(2, 2))
for (i in 1:4)
{
    title <- paste(i, "-th sampling", sep = "")
    hist(RandomSample[, i], ylab = "f(x)", xlab = "random uniform", pro = T, main = title)
}

par(mfrow = c(1, 1))
SampleMean <- apply(RandomSample, 2, mean)
hist(SampleMean, ylab = "f(x)", xlab = "sample mean", pro = T, main = "n = 20")

CLT.unif <- function(Umin, Umax, n.sample, n.repeated)
{
    RandomSampling <- matrix(0, n.sample, n.repeated)
    for (i in 1:n.repeated)
    {
        rnumber <- runif(n.sample, Umin, Umax)
        RandomSampling[, i] <- rnumber
    }
    SampleMean <- apply(RandomSampling, 2, mean)
    par(mfrow = c(1, 2))
    
    title <- paste("n = ", n.sample, sep = "")
    hist(SampleMean, breaks = 30, ylab = "f(x)", xlab = "sample mean", pro = T, main = title)
    qqnorm(SampleMean)
    qqline(SampleMean)
}

#We want a DAUGHTER 

no.child <- function(repeated)
{
    ans <- matrix(0, 1, repeated)
    
    for (i in 1:repeated)
    {
        haveDgt <- 0
        child <- 0
        
        while (haveDgt == 0 & child <= 3)
        {
            #pick <- runif(1, 0, 99)
            pick <- sample(0:99, 1)
            
            if (pick <= 48){
                haveDgt <- 1
                child <- child + 1
            }else if (pick <= 99){
                child <- child + 1
            }
        }
        if (haveDgt)
            ans[1, i] <- 1
    }
    
    return (apply(ans, 1, mean))
}







