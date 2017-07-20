#EB Relative Strength (Closing Price)
#20170712

library(RODBC)
library(xts)
library(ggplot2)
library(PerformanceAnalytics)

myCon <- odbcConnect(dsn = 'cmoney_41', uid = 'hank', pwd = 'hank')

#Select Trading Date
StartDate <- "2010-01-01"
EndDate <- "2017-07-11"
range <- paste(StartDate, "::", EndDate, sep = "")

FutCode <- c("23", "28")
FUTName <- c("TE", "TF")
StockCode <- c("2330", "2882")
StockName <- c("TSMC", "Cathay")

#Select Trading Date
MySQL <- "select Tradingdate"
MySQL <- paste(MySQL, " from [DBMain].[dbo].[Tradingdate]", sep = "")
MySQL <- paste(MySQL, " where Tradingdate between '", sep = "")
MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
MySQL <- paste(MySQL, EndDate, "'", sep = "")

TrdDate <- sqlQuery(myCon, MySQL)

for (index in 1:length(FutCode))
{
    #Get Futures Price
    MySQL <- "select 日期, 股票代號, 收盤價, 開盤價"
    MySQL <- paste(MySQL, " from [Cmoney].[dbo].[日收盤表排行]", sep = "")
    MySQL <- paste(MySQL, " where 日期 between '", sep = "")
    MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
    MySQL <- paste(MySQL, EndDate, "'", sep = "")
    MySQL <- paste(MySQL, " and 股票代號 = 'TWB", FutCode[index], "'",sep = "")
    MySQL <- paste(MySQL, " order by 日期", sep = "")
    FUT <- sqlQuery(myCon, MySQL)
    names(FUT) <- c("TxDate", "Code", "CloPrice", "OpenPrice")
    
    #Get Divisor
    MySQL <- "select top 1 年月日, 指數基值"
    MySQL <- paste(MySQL, " from [MarketData].[dbo].[TEJ指數成分股股數]", sep = "")
    MySQL <- paste(MySQL, "where 公司代碼 like 'M", FutCode[index], "%'", sep = "")
    MySQL <- paste(MySQL, " and 年月日 = '", EndDate, "'", sep = "")
    Dvr <- sqlQuery(myCon, MySQL)
    names(Dvr) <- c("TxDate", "Divisor")
    
    #Calculate Futures Market Value
    FUT <- merge(FUT, Dvr, by = c("TxDate"), all = T)
    n <- dim(FUT)[1]
    for (i in n:1)
    {
        if (i == n)
        {
            FUT[i, 6] <- FUT[i, 3] * FUT[i, 5]
        }else
        {
            FUT[i, 6] <- FUT[i + 1, 6] * (FUT[i, 3] / FUT[i + 1, 3])
        }
        
    }
    names(FUT) <- c("TxDate", FUTName[index], paste("CloPrice_", FUTName[index], sep = ""), paste("OpenPrice_", FUTName[index], sep = ""), 
                    paste(FUTName[index], "_Divisor", sep = ""), paste(FUTName[index], "_MktVal", sep = ""))
    tmp <- paste("FUT_", FUTName[index], sep = "")
    assign(tmp, FUT)
    
    #Get Stock Closing Price
    MySQL <- "select 日期, 股票代號, 收盤價"
    MySQL <- paste(MySQL, " from [Cmoney].[dbo].[日收盤還原表排行]", sep = "")
    MySQL <- paste(MySQL, " where 股票代號 = '", StockCode[index], "'", sep = "")
    MySQL <- paste(MySQL, " and 日期 between '", sep = "")
    MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
    MySQL <- paste(MySQL, EndDate, "'", sep = "")
    Stock <- sqlQuery(myCon, MySQL)
    names(Stock) <- c("TxDate", "Code", "CloPrice")
    StkTmp <- paste("Stock_", StockName[index], sep = "")
    assign(StkTmp, Stock)
}

#Get Futures Settlement Date 
MySQL <- "select 結算日"
MySQL <- paste(MySQL, " from [DBMain].[dbo].[期貨結算日]", sep = "")
MySQL <- paste(MySQL, " where 結算日 between '", sep = "")
MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
MySQL <- paste(MySQL, EndDate, "'", sep = "")

settleDate <- sqlQuery(myCon, MySQL)
names(settleDate) <- c("TxDate")
settleDate$settle <- 1

Stock <- merge(Stock_Cathay, Stock_TSMC, by = c("TxDate"), all = T)
names(Stock) <- c("TxDate", "Cathay", "Cath_CloPrice", "TSMC", "TSMC_CloPrice")
Stock <- Stock[, -c(2, 4)]

#Select Trading Year 
MySQL <- "select distinct(year(Tradingdate))"
MySQL <- paste(MySQL, " from [DBMain].[dbo].[Tradingdate]", sep = "")
MySQL <- paste(MySQL, " where year(Tradingdate) between year('", StartDate, "') and year('", EndDate, "')", sep = "")
MySQL <- paste(MySQL, " order by year(Tradingdate)", sep = "")
TrdYear <- sqlQuery(myCon, MySQL)

#Close ODBC Connection
odbcClose(myCon)

names(TrdDate) <- c("TxDate")
settleDate <- merge(settleDate, TrdDate, by = c("TxDate"), all = T)
n <- dim(settleDate)[1]
for (i in 1:n)
{
   if (is.na(settleDate[i, 2]))
       settleDate[i, 2] <- 0
}

FUT <- merge(FUT_TE, FUT_TF, by = c("TxDate"), all = T)
FUT <- merge(FUT, settleDate, by = c("TxDate"), all = T)
FUT <- FUT[, -c(2, 5, 7, 10)]
# FUT$A_Value <- 2 * FUT$TE_MktVal - 3 * FUT$TF_MktVal
FUT$A_Value <- 3 * FUT$TE_MktVal - 4 * FUT$TF_MktVal

for (i in 36:nrow(FUT))
{
    FUT[i, 10] <- min(FUT[(i - 35):(i - 1), 9])
    FUT[i, 11] <- max(FUT[(i - 35):(i - 1), 9])
}

# FUT <- FUT[, -c(2, 4, 6, 8)]
FUT <- na.omit(FUT)
names(FUT) <- c("TxDate", "CloPrice_TE", "OpenPrice_TE", "TE_MktVal", "CloPrice_TF", "OpenPrice_TF", "TF_MktVal",
                "Settle", "A_Value", "35_min", "35_max")
# write.table(FUT, file = "EBRS_aDay.csv", sep = ",")

for (i in 1:nrow(FUT))
{
    if (FUT[i, 9] < FUT[i, 10])
    {
        FUT[i, 12] <- 1
        FUT[i, 13] <- 0
    }else if (FUT[i, 9] > FUT[i, 11])
    {
        FUT[i, 12] <- 0
        FUT[i, 13] <- 1
    }else
    {
        FUT[i, 12] <- 0
        FUT[i, 13] <- 0
    }
}
names(FUT) <- c("TxDate", "CloPrice_TE", "OpenPrice_TE", "TE_MktVal", "CloPrice_TF", "OpenPrice_TF", "TF_MktVal",
                "Settle", "A_Value", "35_min", "35_max", "Short E Long B", "Long E Short B")


FUT_Sig <- FUT
# FUT_Sig <- na.omit(FUT_Sig)
FUT_Sig <- FUT_Sig[, -c(4, 7, 9:11)]
FUT_Sig$TE_Pos <- 0

# write.table(FUT_Sig, file = "FUT_Sig_EBRS_aDay.csv", sep = ",")

#Set FUT Triggering Amount and Maximum Futures Position
TE_Amt <- -3
TF_Amt <- -4
TE_Margin <- 68000
TF_Margin <- 53000
TE_Maintain <- 52000
TF_Maintain <- 41000
TE_FUT_Max <- 50

for (i in 1:(nrow(FUT_Sig) - 1))
{
    #If in the Settlement Date
    if (FUT_Sig[i, 6] == 1)     
    {
        FUT_Sig[i, 10] <- FUT_Sig[i - 1, 9] * FUT_Sig[i, 2] * 4000 + (-FUT_Sig[i - 1, 9] * 3 / 2) * FUT_Sig[i, 4] * 1000
        FUT_Sig[i, 9] <- 0
    }else                       
    {
        #Not in the Settlement Date
        if (i == 1)
        {
            
            if (FUT_Sig[i, 7] == 1 & FUT_Sig[i, 8] == 0)
            {   
                #Short E, Long B
                FUT_Sig[i, 10] <- TE_Amt * FUT_Sig[i + 1, 3] * 4000 - TF_Amt * FUT_Sig[i + 1, 5] * 1000
                FUT_Sig[i, 9] <- -(TE_Amt)
            }else if (FUT_Sig[i, 7] == 0 & FUT_Sig[i, 8] == 1)
            {    
                #Long E, Short B
                FUT_Sig[i, 10] <- -(TE_Amt) * FUT_Sig[i + 1, 3] * 4000 + TF_Amt * FUT_Sig[i + 1, 5] * 1000
                FUT_Sig[i, 9] <- TE_Amt
            }else
            {
                FUT_Sig[i, 10] <- 0
                FUT_Sig[i, 9] <- 0
            }
        }else
        {
            #Short E, Long B
            if (FUT_Sig[i, 7] == 1 & FUT_Sig[i, 8] == 0)
            {
                if (abs(FUT_Sig[i - 1, 9] - TE_Amt) <= TE_FUT_Max)
                {
                    FUT_Sig[i, 10] <- TE_Amt * FUT_Sig[i + 1, 3] * 4000 - TF_Amt * FUT_Sig[i + 1, 5] * 1000
                    FUT_Sig[i, 9] <- FUT_Sig[i - 1, 9] - TE_Amt
                }else
                {
                    # FUT_Sig[i, 8] <- FUT_Sig[i - 1, 8]
                    FUT_Sig[i, 9] <- FUT_Sig[i - 1, 9]
                    FUT_Sig[i, 10] <- FUT_Sig[i, 9] * (FUT_Sig[i, 2] - FUT_Sig[i - 1, 2]) * 4000 - 
                                        (FUT_Sig[i, 9] * 3 / 2) * (FUT_Sig[i, 4] - FUT_Sig[i - 1, 4]) * 1000
                }
            }else if (FUT_Sig[i, 7] == 0 & FUT_Sig[i, 8] == 1)
            {
                #Long E, Short B
                if (abs(FUT_Sig[i - 1, 9] + TE_Amt) <= TE_FUT_Max)
                {
                    #Set Up New Position
                    FUT_Sig[i, 10] <- -(TE_Amt) * FUT_Sig[i + 1, 3] * 4000 + TF_Amt * FUT_Sig[i + 1, 5] * 1000
                    FUT_Sig[i, 9] <- FUT_Sig[i - 1, 9] + TE_Amt
                }else
                {
                    #Can not Set Up New Position
                    # FUT_Sig[i, 8] <- FUT_Sig[i - 1, 8]
                    FUT_Sig[i, 9] <- FUT_Sig[i - 1, 9]
                    FUT_Sig[i, 10] <- FUT_Sig[i, 9] * (FUT_Sig[i, 2] - FUT_Sig[i - 1, 2]) * 4000 - 
                                        (FUT_Sig[i, 9] * 3 / 2) * (FUT_Sig[i, 4] - FUT_Sig[i - 1, 4]) * 1000
                }
            }else
            {
                # else if (FUT_Sig[i, 5] == 0 & FUT_Sig[i, 6] == 0)
                #No Trade
                FUT_Sig[i, 9] <- FUT_Sig[i - 1, 9]
                FUT_Sig[i, 10] <- FUT_Sig[i, 9] * (FUT_Sig[i, 2] - FUT_Sig[i - 1, 2]) * 4000 - 
                                    (FUT_Sig[i, 9] * 3 / 2) * (FUT_Sig[i, 4] - FUT_Sig[i - 1, 4]) * 1000
            }
        }
    }
    # print(FUT_Sig[i, 1])
}
FUT_Sig <- na.omit(FUT_Sig)
FUT_Sig[, 11] <- cumsum(FUT_Sig[1:nrow(FUT_Sig), 10])

names(FUT_Sig) <- c("TxDate", "CloPrice_TE", "OpenPrice_TE", "CLoPrice_TF", "OpenPrice_TF", "settle",
                    "Short E Long B", "Long E Short B", "TE_Pos", "PL_FUT", "Cum_PL_FUT")

FUT_Sig$Margin <- abs(FUT_Sig$TE_Pos) * TE_Margin + abs(FUT_Sig$TE_Pos) * 3 / 2 * TF_Margin
FUT_Sig$Maintain <- abs(FUT_Sig$TE_Pos) * TE_Maintain + abs(FUT_Sig$TE_Pos) * 3 / 2 * TF_Maintain

#Use Market Value to Calculate Daily Return
FUT_Sig <- merge(FUT_Sig, FUT_TE, by = c("TxDate"))
FUT_Sig <- FUT_Sig[, -c(14:17)]
FUT_Sig$TE_MktVal <- FUT_Sig$TE_MktVal * abs(FUT_Sig$TE_Pos)

for (i in 1:nrow(FUT_Sig))
{
    if (FUT_Sig[i, 12] == 0 & FUT_Sig[i, 10] != 0)
        FUT_Sig[i, 12] <- FUT_Sig[i - 1, 12]
    if (FUT_Sig[i, 13] == 0 & FUT_Sig[i, 10] != 0)
        FUT_Sig[i, 13] <- FUT_Sig[i - 1, 13]
    if (FUT_Sig[i, 14] == 0 & FUT_Sig[i, 10] != 0)
        FUT_Sig[i, 14] <- FUT_Sig[i - 1, 14]
}
for (i in 1:nrow(FUT_Sig))
{
    if (FUT_Sig[i, 10] < 0)
    {
        if ((FUT_Sig[i, 10] + FUT_Sig[i, 12]) < FUT_Sig[i, 13])
            FUT_Sig[i, 12] <- FUT_Sig[i, 12] + abs(FUT_Sig[i, 10])
    }
}

# FUT_Sig$Return <- FUT_Sig$PL_FUT / FUT_Sig$Margin
FUT_Sig$Return <- FUT_Sig$PL_FUT / FUT_Sig$TE_MktVal

# FUT_Sig <- FUT_Sig[, -c(2:9, 12, 13, 14)]
FUT_Sig <- FUT_Sig[, -c(2:9, 12, 13)]
FUT_Sig <- xts(FUT_Sig, order.by = as.Date(FUT_Sig$TxDate))
# FUT_Sig <- FUT_Sig[, -1]

# write.table(FUT_Sig, file = "FUT_Sig_EBRS_aDay.csv", sep = ",")
#Backtesting Statistical 
riskfree <- 0.01
BackTesting <- matrix(0, 7, nrow(TrdYear) + 1)
for (i in 1:nrow(TrdYear))
{
    # tmp <- paste("Strat_", TrdYear[i, 1], sep = "")
    # assign(tmp, FUT_Sig[range])
    range <- paste("", TrdYear[i, 1], "", sep = "")
    tmp <- FUT_Sig[range]
    
    tmp <- na.omit(as.data.frame(tmp))
    tmp <- xts(tmp, order.by = as.Date(tmp$TxDate))
    tmp <- tmp[, -1]
    
    BackTesting[1, i] <- mean(as.numeric(tmp$Return)) * 100
    BackTesting[2, i] <- sd(as.numeric(tmp$Return))
    BackTesting[3, i] <- (mean(as.numeric(tmp$Return)) - riskfree) / sd(as.numeric(tmp$Return))
    
    tmp$CumRet <- cumprod(1 + as.numeric(tmp$Return))
    tmp$D <- cummax(as.numeric(tmp$CumRet)) - as.numeric(tmp$CumRet)
    tmp$d <- as.numeric(tmp$D) / cummax(as.numeric(tmp$CumRet))
    BackTesting[4, i] <- max(tmp$d)
    
    BackTesting[5, i] <- nrow(tmp[tmp$PL_FUT > 0]) / nrow(tmp)
    BackTesting[6, i] <- mean(as.numeric(tmp[as.numeric(tmp$Return) > 0]$Return)) * 100
    BackTesting[7, i] <- mean(as.numeric(tmp[as.numeric(tmp$Return) < 0]$Return)) * 100
    
    Strat <- paste("Strat_", TrdYear[i, 1], sep = "")
    assign(Strat, tmp)
}

FUT_Sig <- na.omit(as.data.frame(FUT_Sig))
FUT_Sig <- xts(FUT_Sig, order.by = as.Date(FUT_Sig$TxDate))
FUT_Sig <- FUT_Sig[, -1]

BackTesting[1, 9] <- mean(as.numeric(FUT_Sig$Return)) * 100
BackTesting[2, 9] <- sd(as.numeric(FUT_Sig$Return))
BackTesting[3, 9] <- (mean(as.numeric(FUT_Sig$Return)) - riskfree) / sd(as.numeric(FUT_Sig$Return))

FUT_Sig$CumRet <- cumprod(1 + as.numeric(FUT_Sig$Return))
FUT_Sig$D <- cummax(as.numeric(FUT_Sig$CumRet)) - as.numeric(FUT_Sig$CumRet)
FUT_Sig$d <- as.numeric(FUT_Sig$D) / cummax(as.numeric(FUT_Sig$CumRet))
BackTesting[4, 9] <- max(FUT_Sig$d)

BackTesting[5, 9] <- nrow(FUT_Sig[FUT_Sig$PL_FUT > 0]) / nrow(FUT_Sig)
BackTesting[6, 9] <- mean(as.numeric(FUT_Sig[as.numeric(FUT_Sig$Return) > 0]$Return)) * 100
BackTesting[7, 9] <- mean(as.numeric(FUT_Sig[as.numeric(FUT_Sig$Return) < 0]$Return)) * 100

colnames(BackTesting) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "all")
rownames(BackTesting) <- c("AvgRet(%)", "StdRet", "Sharpe", "MDD", "P", "U(Ret > 0, %)", "D(Ret < 0, %)")
# write.table(FUT_Sig, file = "EBRS_aDay.csv", sep = ",")
# 
# FUT_Sig <- merge(FUT_Sig, Stock, by = c("TxDate"), all = T)
# FUT_Sig <- na.omit(FUT_Sig)
# 
# TSMC_Shr <- 2
# Cath_Shr <- 3
# 
# n <- dim(FUT_Sig)[1]
# for (i in 1:n)
# {
#     if (FUT_Sig[i, 4] == 1 & FUT_Sig[i, 5] == 0)
#     {
#         FUT_Sig[i, 10] <- (TSMC_Shr * FUT_Sig[i, 9] - Cath_Shr * FUT_Sig[i, 8]) * 1000
#     }
#     if (FUT_Sig[i, 4] == 0 & FUT_Sig[i, 5] == 1)
#     {
#         FUT_Sig[i, 10] <- (-(TSMC_Shr) * FUT_Sig[i, 9] + Cath_Shr * FUT_Sig[i, 8]) * 1000
#     }
# }
# FUT_Sig[, 11] <- cumsum(FUT_Sig[1:n, 10])
# names(FUT_Sig) <- c("TxDate", "CloPrice_TE", "CLoPrice_TF", "Short E Long B", "Long E Short B", "PL_FUT", "Cum_PL_FUT","Cath_CloPrice", "TSMC_CloPrice", "PL_STK", "Cum_PL_Stk")
# 
# FUT_Sig <- FUT_Sig[, -c(2:5, 8, 9)]
# FUT_Sig <- xts(FUT_Sig, order.by = as.Date(FUT_Sig$TxDate))
# FUT_Sig <- FUT_Sig[, -1]
# # write.table(FUT_Sig, file = "EBRS_aDay.csv", sep = ",")
# 
# StartTradedYr <- substr(StartDate, 1, 4)
# EndTrdYr <- substr(EndDate, 1, 4)
# YrRange <- paste(StartTradedYr, "::", EndTrdYr, sep = "")
# 
# # plot(FUT_Sig$Cum_PL_FUT)











