calc_mean = function(x)
{
    return (0.08 * x + 0.15 * (1 - x))
}

mean = sapply(seq(0, 1, by = 0.02), calc_mean)
sd_mat = matrix(nrow = 5, ncol = 51)

for (i in 1:5)
{
    calc_sd = function(x)
    {
        sd = sqrt(x^2*0.12^2 + (1-x)^2*0.25^2 + 2*x*(1-x)*(-1.5+0.5*i)*0.12*0.25)
        return (sd)
    }
    sd_mat[i, ] = sapply(seq(0, 1, by = 0.02), calc_sd)
}

plot(mean, sd_mat[1, ], type = 'l', ylab = 'standard deviation')
lines(mean, sd_mat[2, ], type = 'l', col = 2)
lines(mean, sd_mat[3, ], type = 'l', col = 3)
lines(mean, sd_mat[4, ], type = 'l', col = 4)
lines(mean, sd_mat[5, ], type = 'l', col = 5)
legend('topleft', col = c(1:5), legend = c('1', '2', '3', '4', '5'), lty = 1, cex = 0.5)
