#Merge Table to Create Daily_Tick_01min
#20170714

library(RODBC)
library(RODBCext)

StartDate = "2017-01-01"
EndDate <- "2017-12-31"

#Connect to 172.24.26.43
myCon_43 <- odbcConnect(dsn = 'cmoney', uid = 'hank', pwd = 'hank')
#Connect to 172.24.26.41
myCon_41 <- odbcConnect(dsn = 'cmoney_41', uid = 'hank', pwd = 'hank')

FutNames <- c("TXF", "EXF", "FXF", "XIF", "NYF", "OAF", "NZF", "OCF", "OKF")
CashNames <- c("TWA00", "TWB23", "TWB28", "TWA06", "0050", "006205", "0061", "006207", "00639")
sDate <- "2017-05-23"

FutLen <- length(FutNames)
# for (k in 1:FutLen)
for (k in 1:FutLen)
{
    #Collecting INDEX Data For 1MIN
    if (k <= 4)
    {
        MySQL <- "select 日期, 時間, 指數"
        MySQL <- paste(MySQL, " from [Cmoney].[dbo].[日指數分價量統計表]", sep = "")
        MySQL <- paste(MySQL, " where 股票代號 = '", CashNames[k], "'", sep = "")
        MySQL <- paste(MySQL, "and 日期 = '", sDate, "'", sep = "")
        INDEX <- sqlQuery(myCon_41, MySQL)
        names(INDEX) <- c("TxDate", "TimeTag", paste("TickPrice_", FutNames[k], "現貨", sep =""))
    }else
    {
        MySQL <- "select TxDate, TimeTag, Tick_Price"
        MySQL <- paste(MySQL, " from [Intraday].[dbo].[DailyTick_01min]", sep = "")
        MySQL <- paste(MySQL, " where StockId = '", CashNames[k], "'", sep = "")
        MySQL <- paste(MySQL, "and TxDate = '", sDate, "'", sep = "")
        INDEX <- sqlQuery(myCon_43, MySQL)
        names(INDEX) <- c("TxDate", "TimeTag", paste("TickPrice_", FutNames[k], "現貨", sep =""))
        
        n <- dim(INDEX)[1]
        for (i in 1:n)
        {
            if (nchar(INDEX[i, 2]) == 7)
            {
                INDEX[i, 2] <- substr(INDEX[i, 2], 1, 3)
            }else
            {
                INDEX[i, 2] <- substr(INDEX[i, 2], 1, 4)
            }
        }
    }

    #Futures Data For 1Min
    MySQL <- "select TxDate, TimeTag, StockId, Tick_Price, Tick_Qty, B1_Price, B1_Qty, A1_Price, A1_Qty"
    MySQL <- paste(MySQL, " from [Intraday].[dbo].[DailyTick_01min_FU]", sep = "")
    MySQL <- paste(MySQL, " where TxDate = '", sDate, "'", sep = "")
    MySQL <- paste(MySQL, " and StockId Like '", FutNames[k], "%'", sep = "")
    MySQL <- paste(MySQL, " and len(StockId) = 5", sep = "")
    FUT <- sqlQuery(myCon_43, MySQL)

    #Futures name
    MySQL <- "select 數字月, 英文月"
    MySQL <- paste(MySQL, " from [DBMain].[dbo].[期貨_英文月份對照表]", sep = "")
    FutMn <- sqlQuery(myCon_41, MySQL)
    names(FutMn) <- c("Num", "Eng")
    
    #Settlement Month
    MySQL <- "select 結算日"
    MySQL <- paste(MySQL, " from [DBMain].[dbo].[期貨結算日]", sep = "")
    MySQL <- paste(MySQL, " where 結算日 between '", StartDate, "' and '", EndDate, "'" ,sep = "")
    FUTSettle <- sqlQuery(myCon_41, MySQL)
    names(FUTSettle) <- c("Settle Date")
    
    FutMn <- cbind(FutMn, FUTSettle)
    
    # Organizing Futures Data
    # Step 1. Dispose TimeTag
    n <- dim(FUT)[1]
    for (i in 1:n)
    {
        if (nchar(FUT[i, 2]) == 7)
        {
            FUT[i, 2] <- substr(FUT[i, 2], 1, 3)
        }else if (nchar(FUT[i, 2]) == 8)
        {
            FUT[i, 2] <- substr(FUT[i, 2], 1, 4)
        }
    }
    
    #Step 2. Dispose Settlement Month(Near)
    Len <- dim(FutMn)[1]
    FUT_Near <- FUT
    n <- dim(FUT_Near)[1]
    for (i in 1:n)
    {
        for (j in 1:Len)
        {
            if (j == 1)
            {
                if (FUT_Near[i, 1] < FutMn[j, 3])
                {
                    FUT_Near[i, 10] <- FutMn[j, 2]
                    break
                }
            }else
            {
                if (FUT_Near[i, 1] > FutMn[j - 1, 3] & FUT_Near[i, 1] < FutMn[j, 3])
                {
                    FUT_Near[i, 10] <- FutMn[j, 2]
                    break
                }
            }
        }
    }
    
    DelRow <- c()
    for (i in 1:n)
    {
        if (substr(FUT_Near[i, 3], 4, 4) != FUT_Near[i, 10])
        {
            if (is.null(DelRow))
            {
                DelRow <- i
            }else
            {
                DelRow <- c(DelRow, i)
            }
        }
    }
    
    FUT_Near <- FUT_Near[-DelRow, ]
    FUT_Near <- FUT_Near[, -c(3, 10)]
    FUT_Near$MPrice <- (FUT_Near$A1_Price + FUT_Near$B1_Price) / 2
    # FITX_Near <- FITX_Near[order(FITX_Near$TxDate, FITX_Near$TimeTag), ]
    
    names(FUT_Near) <- c("TxDate", "TimeTag", 
                         paste("TickPrice_", FutNames[k], "_NEAR", sep = ""), paste("TickQty_", FutNames[k], "_NEAR", sep = ""), 
                         paste("B1Price_", FutNames[k], "_NEAR", sep = ""), paste("B1Qty_", FutNames[k], "_NEAR", sep = ""), 
                         paste("A1Price_", FutNames[k], "_NEAR", sep = ""), paste("A1Qty_", FutNames[k], "_NEAR", sep = ""), 
                         paste("MPrice_", FutNames[k], "_NEAR", sep = ""))
    
    #Step 3. Dispose Settlement Month(Next)
    Len <- dim(FutMn)[1]
    FUT_NEXT <- FUT
    for (i in 1:n)
    {
        for (j in 1:Len)
        {
            if (j == 1)
            {
                if (FUT_NEXT[i, 1] < FutMn[j, 3])
                {
                    FUT_NEXT[i, 10] <- FutMn[j + 1, 2]
                    break
                }
            }else
            {
                if (FUT_NEXT[i, 1] > FutMn[j - 1, 3] & FUT_NEXT[i, 1] < FutMn[j, 3])
                {
                    FUT_NEXT[i, 10] <- FutMn[(j + 1) %% 12, 2]
                    # if (j == 12)
                    # {
                    #     FUT_NEXT[i, 10] <- FutMn[1, 2]
                    # }else
                    # {
                    #     FUT_NEXT[i, 10] <- FutMn[j + 1, 2]
                    # }
                    break
                }
            }
        }
    }
    
    DelRow <- c()
    for (i in 1:n)
    {
        if (substr(FUT_NEXT[i, 3], 4, 4) != FUT_NEXT[i, 10])
        {
            if (is.null(DelRow))
            {
                DelRow <- i
            }else
            {
                DelRow <- c(DelRow, i)
            }
        }
    }
    
    FUT_NEXT <- FUT_NEXT[-DelRow, ]
    FUT_NEXT <- FUT_NEXT[, -c(3, 10)]
    FUT_NEXT$MPrice <- (FUT_NEXT$A1_Price + FUT_NEXT$B1_Price) / 2
    # FITX_Near <- FITX_Near[order(FITX_Near$TxDate, FITX_Near$TimeTag), ]
    
    names(FUT_NEXT) <- c("TxDate", "TimeTag", 
                         paste("TickPrice_", FutNames[k], "_NEXT", sep = ""), paste("TickQty_", FutNames[k], "_NEXT", sep = ""), 
                         paste("B1Price_", FutNames[k], "_NEXT", sep = ""), paste("B1Qty_", FutNames[k], "_NEXT", sep = ""), 
                         paste("A1Price_", FutNames[k], "_NEXT", sep = ""), paste("A1Qty_", FutNames[k], "_NEXT", sep = ""), 
                         paste("MPrice_", FutNames[k], "_NEXT", sep = ""))
    
    FUT_TEMP <- merge(FUT_Near, FUT_NEXT, by = c("TxDate", "TimeTag"), all = T)
    
    # for (nROW in 1:nrow(FUT_TEMP))
    # {
    #     if (nROW == 1)
    #     {
    #         next
    #     }else
    #     {
    #         for (nCOL in 1:ncol(FUT_TEMP))
    #         {
    #             if (is.na(FUT_TEMP[nROW, nCOL]))
    #                 FUT_NEXT[nROW, nCOL] <- FUT_NEXT[nROW - 1, nCOL]
    #         }
    #     }
    # }
    
    INDEX_FUT_01min <- merge(INDEX, FUT_TEMP, by = c("TxDate", "TimeTag"), all = T)
    INDEX_FUT_01min <- INDEX_FUT_01min[with(INDEX_FUT_01min, order(INDEX_FUT_01min$TxDate, as.numeric(INDEX_FUT_01min$TimeTag))), ]
    
    TMP <- paste("INDEX_FUT_01min_", FutNames[k], sep = "")
    assign(TMP, INDEX_FUT_01min)
    
    if (k == 1)
    {
        FUT_Tick_Data_01MIN <- INDEX_FUT_01min
    }else
    {
        FUT_Tick_Data_01MIN <- merge(FUT_Tick_Data_01MIN, INDEX_FUT_01min, by = c("TxDate", "TimeTag"))
    }
}
FUT_Tick_Data_01MIN <- FUT_Tick_Data_01MIN[with(FUT_Tick_Data_01MIN, order(FUT_Tick_Data_01MIN$TxDate, as.numeric(FUT_Tick_Data_01MIN$TimeTag))), ]



# write.table(FUT_Tick_Data_01MIN, file = "FUT_Tick_Data_01MIN.csv", sep = ",")

# query_MySQL <- "insert into [TestSherlock].[dbo].[指數期貨每分鐘] (TxDate, TimeTag, TickPrice_TXF現貨) 
#                 values FUT_Tick_Data_01MIN$TxDate, FUT_Tick_Data_01MIN$TimeTag, FUT_Tick_Data_01MIN$TickPrice_TXFㄒ"
# query_MySQL <- "insert into [TestSherlock].[dbo].[指數期貨每分鐘] select * from FUT_Tick_Data_01MIN"
# sqlSave(myCon_41, FUT_Tick_Data_01MIN, rownames = FALSE)
# sqlQuery(myCon_41, query_MySQL)
# 

# odbcClose(myCon_41)
# odbcClose(myCon_43)










