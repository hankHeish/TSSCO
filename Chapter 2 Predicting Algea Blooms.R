# Data Mining with R: Learning with Case Studies
# Chapter 2: Predicting Algea Blooms

library(DMwR)
head(algae)

# 2.3 Loading the Data into R
algae <- read.table(file = "C:/Users/J1060019/Desktop/datasets/Analysis.txt", 
                    head = F, 
                    dec = '.', 
                    col.names = c('season', 'size', 'speed', 'mxPH', 'mnO2', 'C1', 'NO3', 'NH4', 
                                  'oPO4', 'PO4', 'Chla', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7'),
                    na.strings = c('XXXXXXX'))
# file = "/Users/Heishminghan/Desktop/Analysis.txt"

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

# plot(algae$NH4, xlab = "")
# click.lines <- identify(algae$NH4)
# algae[click.lines, ]

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

# 2.5.3 Filling in the Unknown Values by Exploring Correlations 
cor(algae[, 4:18], use = "complete.obs")

symnum(cor(algae[, 4:18], use = "complete.obs"))

data(algae)
algae <- algae[-manyNAs(algae), ]
fit <- lm(PO4 ~ oPO4, data = algae)

alpha <- fit$coefficients[1]
beta <- fit$coefficients[2]

algae[28, 'PO4'] <- alpha + beta *algae[28, 'oPO4']

data(algae)
algae <- algae[-manyNAs(algae), ]

fillPO4 <- function(oP){
    if (is.na(oP)){
        return (NA)
    }else{
        return (alpha + beta*oP)
    }
}
algae[is.na(algae$PO4), 'PO4'] <- sapply(algae[is.na(algae$PO4), 'oPO4'], fillPO4)

histogram(~ mxPH|season, data = algae)

algae$season <- factor(algae$season, levels = c('spring', 'summer', 'autumn', 'winter'))
histogram(~ mxPH | size*speed, data = algae)
stripplot(size ~ mxPH|speed, data = algae, jitter = T)

# 2.5.4 Filling in the Unknown Values by Exploring Similarities between Cases
data(algae)
algae <- algae[-manyNAs(algae), ]
algae <- knnImputation(algae, k = 10)
algae <- knnImputation(algae, k = 10, meth = 'median')


# 2.6 Obtaining Prediction Models
# 2.6.1 Multiple Linear Regression
data(algae)
algae <- algae[-manyNAs(algae), ]
clean.algae <- knnImputation(algae, k = 10)

lm.a1 <- lm(a1 ~ ., data = clean.algae[, 1:12])
summary(lm.a1)
anova(lm.a1)

lm2.a1 <- lm(a1 ~ . - season, data = clean.algae[, 1:12])
summary(lm2.a1)
anova(lm.a1, lm2.a1)

final.lm <- step(lm.a1)
summary(final.lm)

# 2.6.2 Regression Trees
library(rpart)
library(tree)
data(algae)
algae <- algae[-manyNAs(algae), ]

rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])

rt.a1
prettyTree(rt.a1)
printcp(rt.a1)

rt2.a1 <- prune(rt.a1, cp = 0.08)
rt2.a1

set.seed(1234)
rt.a1 <- rpartXse(a1 ~ ., data = algae[, 1:12])

first.tree <- rpart(a1 ~ ., data = algae[, 1:12])
snip.rpart(first.tree, c(4, 7))

prettyTree(first.tree)
snip.rpart(first.tree)

# 2.7 Model Evaluation and Selection
lm.predictions.a1 <- predict(final.lm, clean.algae)
rt.predictions.a1 <- predict(rt.a1, algae)

mae.a1.lm <- mean(abs(lm.predictions.a1 - algae[, 'a1']))
mae.a1.rt <- mean(abs(rt.predictions.a1 - algae[, 'a1']))

mse.a1.lm <- mean((lm.predictions.a1 - algae[, 'a1'])^2)
mse.a1.rt <- mean((rt.predictions.a1 - algae[, 'a1'])^2)

nmse.a1.lm <- mean((lm.predictions.a1 - algae[, 'a1'])^2) / mean((mean(algae[, 'a1']) - algae[, 'a1'])^2)
nmse.a1.rt <- mean((rt.predictions.a1 - algae[, 'a1'])^2) / mean((mean(algae[, 'a1']) - algae[, 'a1'])^2)







