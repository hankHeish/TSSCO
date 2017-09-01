library(RODBC)
library(dplyr)

myCon_41 <- odbcConnect(dsn = 'cmoney_41', uid = 'hank', pwd = 'hank')

StartYr <- 2010
EndYr <- 2016
# TestYr <- 2016
Period <- seq(StartYr, EndYr, 1)

# Step I. Calculating Financial Index EV/EBITDA
for (TestYr in Period){
    # Calculate EV, EBITDA
    # Select EBITDA from [Cmoney].[dbo].[季IFRS財報(損益單季)]
    mySQL <- "select 年季, 股票代號, 股票名稱, [營業利益(千)]"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[季IFRS財報(損益單季)]")
    mySQL <- paste0(mySQL, " where left(年季, 4) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " order by 年季, 股票代號")
    EBITDA <- sqlQuery(myCon_41, mySQL)
    
    mySQL <- "select 年季, 股票代號, 股票名稱, [股本(千)], [負債總計(千)], [現金及約當現金(千)]"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[季IFRS財報(資產負債)]")
    mySQL <- paste0(mySQL, " where LEFT(年季, 4) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " order by 股票代號, 年季")
    Share <- sqlQuery(myCon_41, mySQL)
    
    mySQL <- "select 日期, CONCAT('0', DATEPART(QUARTER, 日期)) as Qtr, 股票代號, 收盤價"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[日收盤表排行]")
    mySQL <- paste0(mySQL, " where LEN(股票代號) = 4 and YEAR(日期) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " and (股票代號 >= '1101' and 股票代號 <= '9962')")
    mySQL <- paste0(mySQL, " order by 股票代號, 日期")
    Clo.Price <- sqlQuery(myCon_41, mySQL)
    
    Clo.Price <- Clo.Price %>%
        group_by(股票代號, Qtr) %>%
        mutate(my_rank = order(日期, decreasing = TRUE)) %>%
        filter(my_rank == 1) %>%
        select(日期, Qtr, 股票代號, 收盤價) %>%
        mutate(年季 = paste0(substring(日期, 1, 4), '0', Qtr))
    # write.table(Clo.Price, file = "Clo.Price.csv", sep = ",")
    
    # Calculate EV
    Stock.MktVal <- merge(x = Clo.Price, y = Share, by = c("年季", "股票代號"), all.x = TRUE) 
    Stock.MktVal$EV <- as.numeric(Stock.MktVal$收盤價) * as.numeric(Stock.MktVal$`股本(千)`) + as.numeric(Stock.MktVal$`負債總計(千)`) - as.numeric(Stock.MktVal$`現金及約當現金(千)`)
    Stock.MktVal <- Stock.MktVal %>%
        select(日期, 年季, 股票代號, EV)
    
    # Calculate EV/EBITDA
    Stock <- merge(x = Stock.MktVal, y = EBITDA, by = c("年季", "股票代號"), all.x = TRUE)
    Stock$Index <- Stock$EV / Stock$`營業利益(千)`
    Stock <- Stock %>%
        na.omit() %>%
        select(年季, 股票代號, Index) %>%
        arrange(年季, -Index) %>%
        group_by(年季) %>%
        mutate(TopRank = row_number()) %>%
        arrange(年季, Index) %>%
        mutate(BottomRank = row_number()) %>%
        filter(TopRank <= 90 | BottomRank <= 90) 
    
    Classify.Group <- function(data){
        
        if (data[1] <= 30){
            return (1)
        }else if (data[1] > 30 & data[1] <= 60){
            return (2)
        }else if (data[1] > 60 & data[1] <= 90){
            return (3)
        }
        
        if (data[2] <= 30){
            return (6)
        }else if (data[2] > 30 & data[2] <= 60){
            return (5)
        }else if (data[2]){
            return (4)
        }
    }
    Stock[, 6] <- apply(Stock[, 4:5], 1, Classify.Group)
    Stock <- Stock %>%
        rename(GroupRank = V6) %>%
        select(年季, 股票代號, GroupRank)
    
    if (TestYr == StartYr){
        Stock.Group.Rank <- Stock
    }else{
        Stock.Group.Rank <- rbind(Stock.Group.Rank, Stock)
    }
    
}
# ===============End of Step I===============

# Part II. Calculate Stock Dauly Adjust by Dividend and Find Their Corresponding YYQQ
# Calculate Stock Return - Daily
# Company's Dividend Policy 
for (TestYr in Period){
    mySQL <- "select 年度, 股票代號, 股票名稱, 除權日, 除權參考價"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[股利政策表]")
    mySQL <- paste0(mySQL, " where 年度 = '", TestYr - 1, "'")
    mySQL <- paste0(mySQL, " and (股票代號 >= '1101' and 股票代號 <= '9962')")
    stockDiv.Policy <- sqlQuery(myCon_41, mySQL)
    colnames(stockDiv.Policy) <- c("年度", "股票代號", "股票名稱", "日期", "除權參考價")
    stockDiv.Policy <- na.omit(stockDiv.Policy)

    mySQL <- "select 年度, 股票代號, 股票名稱, 除息日, 除息參考價"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[股利政策表]")
    mySQL <- paste0(mySQL, " where 年度 = '", TestYr - 1, "'")
    mySQL <- paste0(mySQL, " and (股票代號 >= '1101' and 股票代號 <= '9962')")
    CashDiv.Policy <- sqlQuery(myCon_41, mySQL)
    colnames(CashDiv.Policy) <- c("年度", "股票代號", "股票名稱", "日期", "除息參考價")
    CashDiv.Policy <- na.omit(CashDiv.Policy)

    # Company's Daily Closing Price
    mySQL <- "select 日期, 股票代號, 股票名稱, 收盤價"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[日收盤表排行]")
    mySQL <- paste0(mySQL, " where LEN(股票代號) = 4 and YEAR(日期) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " and (股票代號 >= '1101' and 股票代號 <= '9962')")
    mySQL <- paste0(mySQL, " order by 日期, 股票代號")
    StockPrice <- sqlQuery(myCon_41, mySQL)
    
    # Select Last Trading Date in Test Year
    mySQL <- "select top 1 Tradingdate"
    mySQL <- paste0(mySQL, " from [DBMain].[dbo].[Tradingdate]")
    mySQL <- paste0(mySQL, " where YEAR(Tradingdate) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " order by Tradingdate desc")
    TrdDate <- sqlQuery(myCon_41, mySQL)
    LastTrdDate <- TrdDate[1, 1]

    Adj.StockPrice <- merge(x = StockPrice, y = stockDiv.Policy, by = c('日期', '股票代號'), all.x = TRUE)
    Adj.StockPrice <- merge(x = Adj.StockPrice, y = CashDiv.Policy, by = c('日期', '股票代號'), all.x = TRUE)

    
    # Find Each Corresponding YYQQ
    Correspond.YYQQ <- function(data){
        sDate <- substring(data[1], 1, 10)
        sDate <- gsub("-", "", sDate)
        MMDD <- substring(sDate, 5, 8)
        YY <- substring(sDate, 1, 4)
        
        if (as.integer(MMDD) >= as.integer('0101') & as.integer(MMDD) <= as.integer('0331')){
            YYQQ <- paste0(YY, '01')
        }else if (as.integer(MMDD) >= as.integer('0401') & as.integer(MMDD) <= as.integer('0630')){
            YYQQ <- paste0(YY, '02')
        }else if (as.integer(MMDD) >= as.integer('0701') & as.integer(MMDD) <= as.integer('0930')){
            YYQQ <- paste0(YY, '03')
        }else if (as.integer(MMDD) >= as.integer('1001') & as.integer(MMDD) <= as.integer('1231')){
            YYQQ <- paste0(YY, '04')
        }
        
        return (YYQQ)
    }
    Adj.StockPrice[, 11] <- apply(Adj.StockPrice, 1, Correspond.YYQQ)
    
    Adj.Stock.Ret <- Adj.StockPrice %>%
        select(-年度.x, -年度.y, -股票名稱.y, -股票名稱) %>%
        rename(股票名稱 = 股票名稱.x, RealYYQQ = V11) %>%
        group_by(股票代號, RealYYQQ) %>% 
        mutate(LagPrice = lag(收盤價, k = 1)) 
    
    
    Adj.LagPrice <- function(data){
        if (!is.na(data[1])){
            return (data[1])
        }else if (!is.na(data[2])){
            return (data[2])
        }else{
            return (data[4])
        }
    }
    Adj.Stock.Ret[, 8] <- apply(Adj.Stock.Ret[, 5:8], 1, Adj.LagPrice)
    Adj.Stock.Ret <- Adj.Stock.Ret %>%
        ungroup() %>%
        select(-RealYYQQ)

    # Modify Stock Return Cause by Survivial Bias
    Modify.Stock.Ret <- Adj.Stock.Ret %>%
        group_by(股票代號) %>%
        arrange(desc(日期)) %>%
        mutate(RowIndex = row_number()) %>%
        filter(RowIndex == 1) %>%
        ungroup() %>%
        select(-RowIndex)

    for (i in 1:nrow(Modify.Stock.Ret)){

        if (Modify.Stock.Ret[i, 1] != LastTrdDate){
            mdy.data <- Modify.Stock.Ret[i, ]

            LastListDate <- substring(as.character.Date(mdy.data[1])[4], 3, 13)

            mySQL <- "select top 1 Tradingdate "
            mySQL <- paste0(mySQL, " from [DBMain].[dbo].[Tradingdate]")
            mySQL <- paste0(mySQL, " where Tradingdate > '", LastListDate, "'")
            mySQL <- paste0(mySQL, " order by Tradingdate")
            vOut <- sqlQuery(myCon_41, mySQL)

            mdy.data[1] <- vOut[1]
            mdy.data[7] <- mdy.data[4]
            mdy.data[4] <- 0.01

            # print(mdy.data, "\n")
            Adj.Stock.Ret <- rbind(Adj.Stock.Ret, mdy.data)
        }
    }

    Adj.Stock.Ret <- Adj.Stock.Ret %>%
        select(-除息參考價, -除權參考價) %>%
        na.omit() %>%
        mutate(LogRet = log(收盤價) - log(as.numeric(LagPrice))) %>%
        select(日期, 股票代號, 股票名稱, LogRet) %>%
        arrange(日期, 股票代號)
       
    # Delete Stock Return in the First Day of Each Quarter
    Adj.Stock.Ret <- Adj.Stock.Ret[!substring(Adj.Stock.Ret$日期, 6, 10) %in% c("01-01", "04-01", "07-01", "10-01"), ]

    
    # Find Each Corresponding YYQQ
    Correspond.YYQQ <- function(data){
        sDate <- substring(data[1], 1, 10)
        sDate <- gsub("-", "", sDate)
        MMDD <- substring(sDate, 5, 8)
        YY <- substring(sDate, 1, 4)

        if (as.integer(MMDD) >= as.integer('0515') & as.integer(MMDD) <= as.integer('0814')){
            YYQQ <- paste0(YY, '01')
        }else if (as.integer(MMDD) >= as.integer('0815') & as.integer(MMDD) <= as.integer('1114')){
            YYQQ <- paste0(YY, '02')
        }else if (as.integer(MMDD) >= as.integer('1115') & as.integer(MMDD) <= as.integer('1231')){
            YYQQ <- paste0(YY, '03')
        }else if (as.integer(MMDD) >= as.integer('0101') & as.integer(MMDD) <= as.integer('0331')){
            YYQQ <- paste0(as.integer(YY) - 1, '03')
        }else{
            YYQQ <- paste0(as.integer(YY) - 1, '04')
        }

        return (YYQQ)
    }
    Adj.Stock.Ret[, 5] <- apply(Adj.Stock.Ret, 1, Correspond.YYQQ)
    Adj.Stock.Ret <- Adj.Stock.Ret %>%
        rename(年季 = V5)

    Stock.Ret.by.Group <- merge(x = Adj.Stock.Ret, y = Stock.Group.Rank, by = c("年季", "股票代號"), all.x = TRUE)
    Stock.Ret.by.Group <- na.omit(Stock.Ret.by.Group)

    # test <- Stock.Ret.by.Group %>%
    #     group_by(年季, 股票代號) %>%
    #     mutate(RowIndex = row_number())
    file.name <- "Stock.Ret.by.Group"
    write.table(Stock.Ret.by.Group, file = paste0(file.name, "_", TestYr, ".csv"), sep = ",")

    if (TestYr == StartYr){
        Stock.Ret.Daily.Group <- Stock.Ret.by.Group
    }else
    {
        Stock.Ret.Daily.Group <- rbind(Stock.Ret.Daily.Group, Stock.Ret.by.Group)
    }
}
# ===============End of Step II===============

# Calculate Daily Return by GroupRank
# AvgGroupRet: 每一Group每一天平均Return
# AvgIndexRet: 每天Index平均Return
Daily.Ret.CrossSection <- Stock.Ret.Daily.Group %>%
    group_by(日期, GroupRank) %>%
    mutate(AvgGroupRet = mean(LogRet)) %>%
    group_by(日期) %>%
    mutate(AvgIndexRet = mean(LogRet))

# 不同Group(Tier1, Tier2, ..., Tier6)每天的平均報酬
Daily.Ret.by.Group <- Daily.Ret.CrossSection %>%
    group_by(日期, GroupRank) %>%
    mutate(Count = row_number()) %>%
    filter(Count == 1) %>%
    select(年季, 日期, GroupRank, AvgGroupRet) %>%
    arrange(GroupRank, 日期) %>%
    group_by(GroupRank) %>%
    mutate(CumGroupRet = cumsum(AvgGroupRet)) %>%
    mutate(CumGroupIndex = 100 * exp(CumGroupRet))

file.name <- "Daily.Ret.by.Group"
write.table(Daily.Ret.by.Group, file = paste0(file.name, ".csv"), sep = ",")

# AvgIndexRet: 平均Index(Tier1 ~ Tier6)每天的平均報酬
# CumIndexRet: 平均Index累計報酬
Cum.Index.Ret <- Daily.Ret.CrossSection %>%
    group_by(日期) %>%
    mutate(Count = row_number()) %>%
    filter(Count == 1) %>%
    select(年季, 日期, AvgIndexRet) %>%
    ungroup() %>%
    arrange(日期) %>%
    mutate(CumIndexRet = 100 * exp(cumsum(AvgIndexRet)))

file.name <- "Cum.Index.Ret"
write.table(Cum.Index.Ret, file = paste0(file.name, ".csv"), sep = ",")


Sharpe.Ratio <- Daily.Ret.by.Group %>%
    group_by(GroupRank) %>%
    mutate(ExpRet = mean(AvgGroupRet)) %>%
    mutate(Std = sd(AvgGroupRet)) %>%
    mutate(Sharpe = ExpRet / Std) %>%
    mutate(RowIndex = row_number()) %>%
    filter(RowIndex == 1) %>%
    select(GroupRank, ExpRet, Std, Sharpe)

# Stock.by.Group.Count <- Stock.Ret.Daily.Group %>%
#     group_by(年季, 股票代號) %>%
#     group_by(年季, GroupRank, 股票代號) %>%
#     summarise(Holding.Period = n()) 
# 
# 
# # 先針對同一季同一Group內的股票取mean return，
# # 接著再透過這個mean return做運算
# Stock.Ret.by.Group <- Stock.Ret.Daily.Group %>%
#     group_by(年季, 股票代號) %>%
#     mutate(AvgLogRet = mean(LogRet)) %>%
#     mutate(index = row_number()) %>%
#     filter(index == 1) %>%
#     select(-index, -日期) 
# 
# Stock.Ret.by.Group <- merge(x = Stock.Ret.by.Group, y = Stock.by.Group.Count, by = c('年季', '股票代號'))
# 
# # 計算出每一檔股票的年化報酬率後，
# # 接著計算出依照不同年季的大盤平均報酬，
# # 此處將Log Return轉成Discrete Return後，再轉成Year Return 
# # DailyRet:     將continuous return 轉成discrete return
# # AnnualRet:    某股票在某一年季之年化報酬
# # IndexAvgRet:  Tier1 ~ Tier6在某一年季之平均年化報酬
# Stock.AnnRet.by.Group <- Stock.Ret.by.Group %>%
#     rename(GroupRank = GroupRank.x) %>%
#     # mutate(DailyRet = exp(LogRet) - 1) %>%
#     mutate(DailyRet = exp(AvgLogRet) - 1) %>%
#     mutate(AnnualRet = (1 + DailyRet)^(252/Holding.Period) - 1) %>%
#     # mutate(AnnLogRet = (1 + AvgLogRet)^(252/Holding.Period) - 1) %>% 
#     select(-LogRet, -GroupRank.y, -AvgLogRet) %>%
#     group_by(年季) %>%
#     mutate(IndexAvgRet = mean(AnnualRet)) 
# 
# # 此時依照不同Group，計算每一個Group的報酬率，
# # 平均年季報酬
# # 將每季的平均報酬計算出來後，取第一筆
# # 接著計算每一季的有效利率，
# # 最後，計算出每一Group的累積報酬
# # AvgGroupRank: Tier1 ~ Tier6在某一年季之平均年化報酬
# Portfolio.Retrun <- Stock.AnnRet.by.Group %>%
#     group_by(年季, GroupRank) %>%
#     mutate(AvgGroupRet = mean(AnnualRet)) %>%
#     mutate(RowCount = row_number()) %>%
#     filter(RowCount == 1) %>%
#     select(-股票代號, -股票名稱, -RowCount, -DailyRet, -IndexAvgRet) %>%
#     group_by(GroupRank) %>%
#     arrange(GroupRank, 年季) %>%
#     mutate(RealEffectRet = (1 + AvgGroupRet)^(Holding.Period/252) - 1) %>%
#     mutate(CumGroupRet = cumprod(1 + RealEffectRet) - 1) %>%
#     mutate(CumGroupIndex = 100*(1 + CumGroupRet)) %>%
#     select(年季, GroupRank, CumGroupIndex)
# 
# 
# # 計算大盤在不同年季下的累積報酬
# # 此時須先將年畫報酬轉成持有期間內的累積報酬，再相加
# Cum.Index.Return <- Stock.AnnRet.by.Group %>%
#     group_by(年季) %>%
#     mutate(RowIndex = row_number()) %>%
#     filter(RowIndex == 1) %>%
#     select(年季, Holding.Period, IndexAvgRet, RowIndex) %>%
#     group_by(RowIndex) %>%
#     mutate(RealIndexRet = (1 + IndexAvgRet)^(Holding.Period/252) - 1) %>%
#     arrange(年季) %>%
#     mutate(CumIndexRet = cumprod(1 + RealIndexRet) - 1) %>%
#     select (年季, CumIndexRet) %>%
#     mutate(Index = 100 * (1 + CumIndexRet))


file.name <- "Daily.Ret.CrossSection"
write.table(Daily.Ret.CrossSection, file = paste0(file.name, ".csv"), sep = ",")

plot(Portfolio.Retrun[Portfolio.Retrun$GroupRank == 1, ]$年季, Portfolio.Retrun[Portfolio.Retrun$GroupRank == 1, ]$CumGroupIndex, 
     xlab = "年季", ylab = "Index", lty = 1, type = "b")
for (i in 2:6){
    abline(Portfolio.Retrun[Portfolio.Retrun$GroupRank == i, ]$年季, Portfolio.Retrun[Portfolio.Retrun$GroupRank == i, ]$CumGroupIndex,
           lty = i, lwd = i)
}







