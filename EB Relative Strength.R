#EB Relative Strength 
#20170711

library(RODBC)
library(xts)

myCon <- odbcConnect(dsn = 'cmoney_41', uid = 'hank', pwd = 'hank')

#Select Trading Date
StartDate <- "2017-01-01"
EndDate <- "2017-07-10"
range <- paste(StartDate, "::", EndDate, sep = "")

MySQL <- "select Tradingdate"
MySQL <- paste(MySQL, " from [DBMain].[dbo].[Tradingdate]", sep = "")
MySQL <- paste(MySQL, " where Tradingdate between '", sep = "")
MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
MySQL <- paste(MySQL, EndDate, "'", sep = "")

TrdDate <- sqlQuery(myCon, MySQL)

#Select FITE Closing Price
# MySQL <- "select 日期, 代號, 收盤價"
# MySQL <- paste(MySQL, " from Cmoney.dbo.期貨交易行情表", sep = "")
# MySQL <- paste(MySQL, " where 日期 between '", sep = "")
# MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
# MySQL <- paste(MySQL, EndDate, "'", sep = "")
# MySQL <- paste(MySQL, " and 代號 = 'TE'")
MySQL <- "select TxDate, TimeTag, TickPrice_EXF現貨, TickPrice_EXF近月 ,TickQty_EXF近月 ,B1Price_EXF近月 ,B1Qty_EXF近月 ,A1Price_EXF近月 ,A1Qty_EXF近月"
MySQL <- paste(MySQL, " from [Intraday].[dbo].[指數期貨每分鐘]", sep = "")
MySQL <- paste(MySQL, " where TxDate between '", sep = "")
MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
MySQL <- paste(MySQL, EndDate, "'", sep = "")

TE <- sqlQuery(myCon, MySQL)
TE <- na.omit(TE)
names(TE) <- c("TxDate", "TimeTag", "Index_TE", "LastPrice_TE", "TickQty_TE", "B1Price_TE", "B1Qty_TE", "A1Price_TE", "A1Qty_TE")

#Select TE Divisor 
MySQL <- "select distinct(年月日), 指數基值"
MySQL <- paste(MySQL, " from [MarketData].[dbo].[TEJ指數成分股股數]", sep = "")
MySQL <- paste(MySQL, "where 公司代碼 like 'M2300%'", sep = "")
MySQL <- paste(MySQL, " and 年月日 between '", sep = "")
MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
MySQL <- paste(MySQL, EndDate, "'", sep = "")

TEDvr <- sqlQuery(myCon, MySQL)
names(TEDvr) <- c("TxDate", "TE Divisor")

#TE <- merge(TE, TEDvr, by = c("Date"))
TE <- merge(x = TE, y = TEDvr, by.x = 'TxDate', by.y = 'TxDate', fill = -9999)
TE$TE_MktVal <- TE$LastPrice_TE * TE$`TE Divisor`
# TE <- TE[, -c(2, 3, 4)]

#Select FITF Closing Price
# MySQL <- "select 日期, 代號, 收盤價"
# MySQL <- paste(MySQL, " from Cmoney.dbo.期貨交易行情表", sep = "")
# MySQL <- paste(MySQL, " where 日期 between '", sep = "")
# MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
# MySQL <- paste(MySQL, EndDate, "'", sep = "")
# MySQL <- paste(MySQL, " and 代號 = 'TF'")
# MySQL <- paste(MySQL, " order by 日期")
MySQL <- "select TxDate, TimeTag, TickPrice_FXF現貨, TickPrice_FXF近月 ,TickQty_FXF近月 ,B1Price_FXF近月 ,B1Qty_FXF近月 ,A1Price_FXF近月 ,A1Qty_FXF近月"
MySQL <- paste(MySQL, " from [Intraday].[dbo].[指數期貨每分鐘]", sep = "")
MySQL <- paste(MySQL, " where TxDate between '", sep = "")
MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
MySQL <- paste(MySQL, EndDate, "'", sep = "")

TF <- sqlQuery(myCon, MySQL)
TF <- na.omit(TF)
names(TF) <- c("TxDate", "TimeTag", "Index_TF", "LastPrice_TF", "TickQty_TF", "B1Price_TF", "B1Qty_TF", "A1Price_TF", "A1Qty_TF")

#Select TF Divisor
MySQL <- "select distinct(年月日), 指數基值"
MySQL <- paste(MySQL, " from [MarketData].[dbo].[TEJ指數成分股股數]", sep = "")
MySQL <- paste(MySQL, "where 公司代碼 like 'M2800%'", sep = "")
MySQL <- paste(MySQL, " and 年月日 between '", sep = "")
MySQL <- paste(MySQL, StartDate, "' and '", sep = "")
MySQL <- paste(MySQL, EndDate, "'", sep = "")

TFDvr <- sqlQuery(myCon, MySQL)
names(TFDvr) <- c("TxDate", "TF Divisor")

#Close ODBC
odbcClose(myCon)

# TF <- merge(TF, TFDvr, by = c("Date"), all = T)
TF <- merge(x = TF, y = TFDvr, by.x = 'TxDate', by.y = 'TxDate', fill = -9999)
TF$TF_MktVal <- TF$LastPrice_TF * TF$`TF Divisor`
# TF <- TF[, -c(2, 3, 4)]

FUT <- merge(TE, TF, by = c("Date"), all = T)
FUT$A_Val <- 2 * FUT$TE_MktVal - 3 * FUT$TF_MktVal

n <- dim(FUT)[1]
for (i in 36:n)
{
    FUT[i, 5] <- min(FUT[(i-35):i, 4])
    FUT[i, 6] <- max(FUT[(i-35):i, 4])
}
names(FUT) <- c("Date", "TE_MktVal", "TF_MktVal", "A_Val", "35_min", "35_max")

for (i in 36:n)
{
    # print(FUT[i, 4] > FUT[i, 6])
    if(FUT[i, 4] < FUT[i, 5])
        print(i)
    if(FUT[i, 4] > FUT[i, 6])
        print(i)
}


