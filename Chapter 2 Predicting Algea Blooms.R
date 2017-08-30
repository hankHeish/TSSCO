# Data Mining with R: Learning with Case Studies
# Chapter 2: Predicting Algea Blooms

library(DMwR)
head(algae)

# 2.3 Loading the Data into R
algae <- read.table(file = "/Users/Heishminghan/Desktop/Analysis.txt", 
                    head = F, 
                    dec = '.', 
                    col.names = c('season', 'size', 'speed', 'mxPH', 'mnO2', 'C1', 'NO3', 'NH4', 
                                  'oPO4', 'PO4', 'Chla', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7'),
                    na.strings = c('XXXXXXX'))

# 2.4 Data Visualization and Summarization
summary(algae)

hist(algae$mxPH, prob = T)

library(car)
par(mfrow = c(1, 2))
hist(algae$mxPH, prob = T, xlab = '', main = "Hstogram of Maximum PH Value", ylim = c(0, 1))
lines(density(algae$mxPH, na.rm = T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH, main = "Normal QQ Plot Maximum pH")

par(mfrow = c(1, 1))

boxplot(algae$oPO4, ylab = "Orthophosphate(oOp4)")
rug(jitter(algae$oPO4), side = 2)
abline(h = mean(algae$oPO4, na.rm = T), lty = 2)

# plot(algae$NH4, xlab = "")
# abline(h = mean(algae$NH4, na.rm = T), lty = 1)
# abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2)
# abline(h = median(algae$NH4, na.rm = T), lty = 3)
# identify(algae$NH4)

plot(algae$NH4, xlab = "")
click.lines <- identify(algae$NH4)
algae[click.lines, ]

algae[algae$NH4 > 19000, ]

library(lattice)
bwplot(size ~ a1, data = algae, ylab = "River Size", xlab = "Algal A1")

library(Hmisc)
bwplot(size ~ a1, data = algae, panel = panel.bpplot, probs = seq(0.01, 0.49, by = 0.01),
       datadensity = TRUE, ylab = "River Side", xlab = "Algal A1")

minO2 <- equal.count(na.omit(algae$mnO2), number = 4, overlap = 1/5)
stripplot(season ~ a3|minO2, data = algae[!is.na(algae$mnO2), ])

# 2.5 Unknown Vlues
# 2.5.1 Removing the Observations with Unknown Values
library(DMwR)
data(algae)

algae[!complete.cases(algae), ]

nrow(algae[!complete.cases(algae), ])

algae <- na.omit(algae)
algae <- algae[-c(62, 199), ]

apply(algae, 1, function(x) sum(is.na(x)))

data(algae)
manyNAs(algae, 0.2)

algae <- algae[-manyNAs(algae, 0.2), ]

# 2.5.2 Filling in the Unknowns with the Most Frequent Values
algae[48, "mxPH"] <- mean(algae$mxPH, na.rm = T)
algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm = T)

data("algae")
algae <- algae[-manyNAs(algae), ]
algae <- centralImputation(algae)







