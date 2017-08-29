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
    # Select EBITDA from [Cmoney].[dbo].[�uIFRS�]��(�l�q��u)]
    mySQL <- "select �~�u, �Ѳ��N��, �Ѳ��W��, [��~�Q�q(�d)]"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[�uIFRS�]��(�l�q��u)]")
    mySQL <- paste0(mySQL, " where left(�~�u, 4) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " order by �~�u, �Ѳ��N��")
    EBITDA <- sqlQuery(myCon_41, mySQL)
    
    mySQL <- "select �~�u, �Ѳ��N��, �Ѳ��W��, [�ѥ�(�d)], [�t���`�p(�d)], [�{���ά����{��(�d)]"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[�uIFRS�]��(�겣�t��)]")
    mySQL <- paste0(mySQL, " where LEFT(�~�u, 4) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " order by �Ѳ��N��, �~�u")
    Share <- sqlQuery(myCon_41, mySQL)
    
    mySQL <- "select ���, CONCAT('0', DATEPART(QUARTER, ���)) as Qtr, �Ѳ��N��, ���L��"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[�馬�L���Ʀ�]")
    mySQL <- paste0(mySQL, " where LEN(�Ѳ��N��) = 4 and YEAR(���) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " and (�Ѳ��N�� >= '1101' and �Ѳ��N�� <= '9962')")
    mySQL <- paste0(mySQL, " order by �Ѳ��N��, ���")
    Clo.Price <- sqlQuery(myCon_41, mySQL)
    
    Clo.Price <- Clo.Price %>%
        group_by(�Ѳ��N��, Qtr) %>%
        mutate(my_rank = order(���, decreasing = TRUE)) %>%
        filter(my_rank == 1) %>%
        select(���, Qtr, �Ѳ��N��, ���L��) %>%
        mutate(�~�u = paste0(substring(���, 1, 4), '0', Qtr))
    # write.table(Clo.Price, file = "Clo.Price.csv", sep = ",")
    
    # Calculate EV
    Stock.MktVal <- merge(x = Clo.Price, y = Share, by = c("�~�u", "�Ѳ��N��"), all.x = TRUE) 
    Stock.MktVal$EV <- as.numeric(Stock.MktVal$���L��) * as.numeric(Stock.MktVal$`�ѥ�(�d)`) + as.numeric(Stock.MktVal$`�t���`�p(�d)`) - as.numeric(Stock.MktVal$`�{���ά����{��(�d)`)
    Stock.MktVal <- Stock.MktVal %>%
        select(���, �~�u, �Ѳ��N��, EV)
    
    # Calculate EV/EBITDA
    Stock <- merge(x = Stock.MktVal, y = EBITDA, by = c("�~�u", "�Ѳ��N��"), all.x = TRUE)
    Stock$Index <- Stock$EV / Stock$`��~�Q�q(�d)`
    Stock <- Stock %>%
        na.omit() %>%
        select(�~�u, �Ѳ��N��, Index) %>%
        arrange(�~�u, -Index) %>%
        group_by(�~�u) %>%
        mutate(TopRank = row_number()) %>%
        arrange(�~�u, Index) %>%
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
        select(�~�u, �Ѳ��N��, GroupRank)
    
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
    mySQL <- "select �~��, �Ѳ��N��, �Ѳ��W��, ���v��, ���v�Ѧһ�"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[�ѧQ�F����]")
    mySQL <- paste0(mySQL, " where �~�� = '", TestYr - 1, "'")
    mySQL <- paste0(mySQL, " and (�Ѳ��N�� >= '1101' and �Ѳ��N�� <= '9962')")
    stockDiv.Policy <- sqlQuery(myCon_41, mySQL)
    colnames(stockDiv.Policy) <- c("�~��", "�Ѳ��N��", "�Ѳ��W��", "���", "���v�Ѧһ�")
    stockDiv.Policy <- na.omit(stockDiv.Policy)

    mySQL <- "select �~��, �Ѳ��N��, �Ѳ��W��, ������, �����Ѧһ�"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[�ѧQ�F����]")
    mySQL <- paste0(mySQL, " where �~�� = '", TestYr - 1, "'")
    mySQL <- paste0(mySQL, " and (�Ѳ��N�� >= '1101' and �Ѳ��N�� <= '9962')")
    CashDiv.Policy <- sqlQuery(myCon_41, mySQL)
    colnames(CashDiv.Policy) <- c("�~��", "�Ѳ��N��", "�Ѳ��W��", "���", "�����Ѧһ�")
    CashDiv.Policy <- na.omit(CashDiv.Policy)

    # Company's Daily Closing Price
    mySQL <- "select ���, �Ѳ��N��, �Ѳ��W��, ���L��"
    mySQL <- paste0(mySQL, " from [Cmoney].[dbo].[�馬�L���Ʀ�]")
    mySQL <- paste0(mySQL, " where LEN(�Ѳ��N��) = 4 and YEAR(���) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " and (�Ѳ��N�� >= '1101' and �Ѳ��N�� <= '9962')")
    mySQL <- paste0(mySQL, " order by ���, �Ѳ��N��")
    StockPrice <- sqlQuery(myCon_41, mySQL)
    
    # Select Last Trading Date in Test Year
    mySQL <- "select top 1 Tradingdate"
    mySQL <- paste0(mySQL, " from [DBMain].[dbo].[Tradingdate]")
    mySQL <- paste0(mySQL, " where YEAR(Tradingdate) = '", TestYr, "'")
    mySQL <- paste0(mySQL, " order by Tradingdate desc")
    TrdDate <- sqlQuery(myCon_41, mySQL)
    LastTrdDate <- TrdDate[1, 1]

    Adj.StockPrice <- merge(x = StockPrice, y = stockDiv.Policy, by = c('���', '�Ѳ��N��'), all.x = TRUE)
    Adj.StockPrice <- merge(x = Adj.StockPrice, y = CashDiv.Policy, by = c('���', '�Ѳ��N��'), all.x = TRUE)

    
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
        select(-�~��.x, -�~��.y, -�Ѳ��W��.y, -�Ѳ��W��) %>%
        rename(�Ѳ��W�� = �Ѳ��W��.x, RealYYQQ = V11) %>%
        group_by(�Ѳ��N��, RealYYQQ) %>% 
        mutate(LagPrice = lag(���L��, k = 1)) 
    
    
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
        group_by(�Ѳ��N��) %>%
        arrange(desc(���)) %>%
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
        select(-�����Ѧһ�, -���v�Ѧһ�) %>%
        na.omit() %>%
        mutate(LogRet = log(���L��) - log(as.numeric(LagPrice))) %>%
        select(���, �Ѳ��N��, �Ѳ��W��, LogRet) %>%
        arrange(���, �Ѳ��N��)
       
    # Delete Stock Return on the First Day of Quarter
    Adj.Stock.Ret <- Adj.Stock.Ret[!substring(Adj.Stock.Ret$���, 6, 10) %in% c("01-01", "04-01", "07-01", "10-01"), ]

    
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
        rename(�~�u = V5)

    Stock.Ret.by.Group <- merge(x = Adj.Stock.Ret, y = Stock.Group.Rank, by = c("�~�u", "�Ѳ��N��"), all.x = TRUE)
    Stock.Ret.by.Group <- na.omit(Stock.Ret.by.Group)

    # test <- Stock.Ret.by.Group %>%
    #     group_by(�~�u, �Ѳ��N��) %>%
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
# AvgGroupRet: �C�@Group�C�@�ѥ���Return
# AvgIndexRet: �C��Index����Return
Daily.Ret.CrossSection <- Stock.Ret.Daily.Group %>%
    group_by(���, GroupRank) %>%
    mutate(AvgGroupRet = mean(LogRet)) %>%
    group_by(���) %>%
    mutate(AvgIndexRet = mean(LogRet))

# ���PGroup(Tier1, Tier2, ..., Tier6)�C�Ѫ��������S
Daily.Ret.by.Group <- Daily.Ret.CrossSection %>%
    group_by(���, GroupRank) %>%
    mutate(Count = row_number()) %>%
    filter(Count == 1) %>%
    select(�~�u, ���, GroupRank, AvgGroupRet) %>%
    arrange(GroupRank, ���) %>%
    group_by(GroupRank) %>%
    mutate(CumGroupRet = cumsum(AvgGroupRet)) %>%
    mutate(CumGroupIndex = 100 * exp(CumGroupRet))

file.name <- "Daily.Ret.by.Group"
write.table(Daily.Ret.by.Group, file = paste0(file.name, ".csv"), sep = ",")

# AvgIndexRet: ����Index(Tier1 ~ Tier6)�C�Ѫ��������S
# CumIndexRet: ����Index�֭p���S
Cum.Index.Ret <- Daily.Ret.CrossSection %>%
    group_by(���) %>%
    mutate(Count = row_number()) %>%
    filter(Count == 1) %>%
    select(�~�u, ���, AvgIndexRet) %>%
    ungroup() %>%
    arrange(���) %>%
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
#     group_by(�~�u, �Ѳ��N��) %>%
#     group_by(�~�u, GroupRank, �Ѳ��N��) %>%
#     summarise(Holding.Period = n()) 
# 
# 
# # ���w��P�@�u�P�@Group�����Ѳ���mean return�A
# # ���ۦA�z�L�o��mean return���B��
# Stock.Ret.by.Group <- Stock.Ret.Daily.Group %>%
#     group_by(�~�u, �Ѳ��N��) %>%
#     mutate(AvgLogRet = mean(LogRet)) %>%
#     mutate(index = row_number()) %>%
#     filter(index == 1) %>%
#     select(-index, -���) 
# 
# Stock.Ret.by.Group <- merge(x = Stock.Ret.by.Group, y = Stock.by.Group.Count, by = c('�~�u', '�Ѳ��N��'))
# 
# # �p��X�C�@�ɪѲ����~�Ƴ��S�v��A
# # ���ۭp��X�̷Ӥ��P�~�u���j�L�������S�A
# # ���B�NLog Return�নDiscrete Return��A�A�নYear Return 
# # DailyRet:     �Ncontinuous return �নdiscrete return
# # AnnualRet:    �Y�Ѳ��b�Y�@�~�u���~�Ƴ��S
# # IndexAvgRet:  Tier1 ~ Tier6�b�Y�@�~�u�������~�Ƴ��S
# Stock.AnnRet.by.Group <- Stock.Ret.by.Group %>%
#     rename(GroupRank = GroupRank.x) %>%
#     # mutate(DailyRet = exp(LogRet) - 1) %>%
#     mutate(DailyRet = exp(AvgLogRet) - 1) %>%
#     mutate(AnnualRet = (1 + DailyRet)^(252/Holding.Period) - 1) %>%
#     # mutate(AnnLogRet = (1 + AvgLogRet)^(252/Holding.Period) - 1) %>% 
#     select(-LogRet, -GroupRank.y, -AvgLogRet) %>%
#     group_by(�~�u) %>%
#     mutate(IndexAvgRet = mean(AnnualRet)) 
# 
# # ���ɨ̷Ӥ��PGroup�A�p��C�@��Group�����S�v�A
# # �����~�u���S
# # �N�C�u���������S�p��X�ӫ�A���Ĥ@��
# # ���ۭp��C�@�u�����ħQ�v�A
# # �̫�A�p��X�C�@Group���ֿn���S
# # AvgGroupRank: Tier1 ~ Tier6�b�Y�@�~�u�������~�Ƴ��S
# Portfolio.Retrun <- Stock.AnnRet.by.Group %>%
#     group_by(�~�u, GroupRank) %>%
#     mutate(AvgGroupRet = mean(AnnualRet)) %>%
#     mutate(RowCount = row_number()) %>%
#     filter(RowCount == 1) %>%
#     select(-�Ѳ��N��, -�Ѳ��W��, -RowCount, -DailyRet, -IndexAvgRet) %>%
#     group_by(GroupRank) %>%
#     arrange(GroupRank, �~�u) %>%
#     mutate(RealEffectRet = (1 + AvgGroupRet)^(Holding.Period/252) - 1) %>%
#     mutate(CumGroupRet = cumprod(1 + RealEffectRet) - 1) %>%
#     mutate(CumGroupIndex = 100*(1 + CumGroupRet)) %>%
#     select(�~�u, GroupRank, CumGroupIndex)
# 
# 
# # �p��j�L�b���P�~�u�U���ֿn���S
# # ���ɶ����N�~�e���S�ন�������������ֿn���S�A�A�ۥ[
# Cum.Index.Return <- Stock.AnnRet.by.Group %>%
#     group_by(�~�u) %>%
#     mutate(RowIndex = row_number()) %>%
#     filter(RowIndex == 1) %>%
#     select(�~�u, Holding.Period, IndexAvgRet, RowIndex) %>%
#     group_by(RowIndex) %>%
#     mutate(RealIndexRet = (1 + IndexAvgRet)^(Holding.Period/252) - 1) %>%
#     arrange(�~�u) %>%
#     mutate(CumIndexRet = cumprod(1 + RealIndexRet) - 1) %>%
#     select (�~�u, CumIndexRet) %>%
#     mutate(Index = 100 * (1 + CumIndexRet))


file.name <- "Daily.Ret.CrossSection"
write.table(Daily.Ret.CrossSection, file = paste0(file.name, ".csv"), sep = ",")

plot(Portfolio.Retrun[Portfolio.Retrun$GroupRank == 1, ]$�~�u, Portfolio.Retrun[Portfolio.Retrun$GroupRank == 1, ]$CumGroupIndex, 
     xlab = "�~�u", ylab = "Index", lty = 1, type = "b")
for (i in 2:6){
    abline(Portfolio.Retrun[Portfolio.Retrun$GroupRank == i, ]$�~�u, Portfolio.Retrun[Portfolio.Retrun$GroupRank == i, ]$CumGroupIndex,
           lty = i, lwd = i)
}






