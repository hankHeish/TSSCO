library(RODBC)
library(xts)

#Connect to 172.24.26.43
myCon <- odbcConnect(dsn = 'cmoney', uid = 'hank', pwd = 'hank')

monthCode <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
settlement <- c("2017-01-18", "2017-02-15", "2017-03-15", "2017-04-19", "2017-05-17", "2017-06-21", "2017-07-19")

#Trading Day Settlement 
TrdDate <- read.csv("C:/Users/J1060019/Desktop/TrdDate.csv", header = T)[-1]
TrdDate <- xts(TrdDate, order.by = as.Date(TrdDate[, 1]))

#Data Training Date
#Start
StartDay <- "2017-01-01"
#End
EndDay <- "2017-06-30"
range <- paste(StartDay, "::", EndDay, sep = "")
TrdDate <- TrdDate[range]

#Select FITX Tick Data
for (i in 1:length(TrdDate))
{
    date <- TrdDate[i]
    
    mySql <- "select TxDate, TimeTag, StockId, Tick_Price, Tick_Qty, B1_Price, B1_Qty, A1_Price, A1_Qty"
    mySql <- paste(mySql, " from [DailyTick_01min_FU]", sep = "")
    mySql <- paste(mySql, " where TxDate = '", sep = "")
    mySql <- paste(mySql, date, sep = "")
    mySql <- paste(mySql, "'", sep = "")
    mySql <- paste(mySql, " and len(StockId) = 5 and StockId like 'TX%'", sep = "")
    mySql <- paste(mySql, " and TimeTag between '08450000' and '13450000'", sep = "")
    
    vOut <- sqlQuery(myCon, mySql)
    
    if (i == 1)
    {
        FITX <- vOut
    }else
        FITX <- rbind(FITX, vOut)
}

#Select NYF Tick Data
for (i in 1:length(TrdDate))
{
    date <- TrdDate[i]
    
    mySQL <- "select TxDate, TimeTag, StockId, Tick_Price, Tick_Qty, B1_Price, B1_Qty, A1_Price, A1_Qty"
    mySQL <- paste(mySQL, " from [DailyTick_01min_FU]", sep = "")
    mySQL <- paste(mySQL, " where TxDate = '", sep = "")
    mySQL <- paste(mySQL, date, sep = "")
    mySQL <- paste(mySQL, "'", sep = "")
    mySQL <- paste(mySQL, " and len(StockId) = 5 and StockId like 'NYF%'", sep = "")
    mySQL <- paste(mySQL, " and TimeTag between '08450000' and '13450000'", sep = "")
    
    vOut <- sqlQuery(myCon, mySQL)
    
    if (i == 1)
    {
        NYF <- vOut
    }else
        NYF <- rbind(NYF, vOut)
}

#Get 0050 Cash Price
for (i in 1:length(TrdDate))
{
    date <- TrdDate[i]
    
    mySQL <- "select TxDate, TimeTag, StockId, Tick_Price, Tick_Qty, B1_Price, B1_Qty, A1_Price, A1_Qty"
    mySQL <- paste(mySQL, " from [DailyTick_01min]", sep = "")
    mySQL <- paste(mySQL, " where TxDate = '", sep = "")
    mySQL <- paste(mySQL, date, sep = "")
    mySQL <- paste(mySQL, "'", sep = "")
    mySQL <- paste(mySQL, " and StockId = '0050'", sep = "")
    
    vOut <- sqlQuery(myCon, mySQL)
    
    if (i == 1)
    {
        Yuanta_50 <- vOut
    }else
        Yuanta_50 <- rbind(Yuanta_50, vOut)
}

odbcClose(myCon)

#Connect to 172.24.26.41
myCon2 <- odbcConnect(dsn = 'cmoney_41', uid = 'sherlock', pwd = 'sherlock')

for (i in 1:length(TrdDate))
{
    date <- TrdDate[i]
    
    mySQL <- "select ら戳, 丁, 布腹, 计"
    mySQL <- paste(mySQL, " from ら计だ基秖参璸", sep = "")
    mySQL <- paste(mySQL, " where ら戳 = '", sep = "")
    mySQL <- paste(mySQL, date, sep = "")
    mySQL <- paste(mySQL, "'", sep = "")
    mySQL <- paste(mySQL, " and 布腹 = 'TWA00'")
    
    vOut <- sqlQuery(myCon2, mySQL)
    
    if (i == 1)
    {
        Index <- vOut
    }else
        Index <- rbind(Index, vOut)
}
odbcClose(myCon2)

names(Index) <- c("TxDate", "TimeTag", "Code", "Index")
Index$Code <- "TWSE"
# TWSE <- read.csv("C:/Users/J1060019/Desktop/TWSE_Tick_Data.csv", header = T)
# TWSE <- TWSE[, -3]
# names(TWSE) <- c("TxDate", "Code", "TimeTag", "Index")
Index$TimeTag <- paste(as.character(Index$TimeTag), "0000", sep = "")
# TWSE$TxDate <- as.POSIXct(TWSE$TxDate)
# TWSE$Code <- "TWSE"
# TWSE <- xts(TWSE, order.by = as.Date(TWSE$TxDate))
# TWSE <- TWSE[range]

FITX <- na.omit(FITX)
NYF <- na.omit(NYF)
Yuanta_50 <- na.omit(Yuanta_50)

#Dispose of FITX to Get Near Month Futures
n<- dim(FITX)[1]

for (i in 1:n)
{
    for (j in 1:length(settlement))
    {
        if (j == 1)
        {
            if (FITX[i, 1] < settlement[j])
            {
                FITX[i, 10] <- monthCode[j]
                break;
            }

        }else
        {
            if (FITX[i, 1] > settlement[j - 1] & FITX[i, 1] < settlement[j])
            {
                FITX[i, 10] <- monthCode[j]
                break;
            }

        }
    }
}

FITX$StockId <- as.character.default(FITX$StockId)
names(FITX) <- c("TxDate", "TimeTag", "StockId", "Tick_Price", "Tick_Qty", "B1_Price", "B1_Qty", "A1_Price", "A1_Qty", "Code")

DltRow <- c()
for (i in 1:n)
{
    if (!is.na(FITX[i, 3]) & !is.na(FITX[i, 10]) & substr(FITX[i, 3], 4, 4) != FITX[i, 10])
    {
        if (is.null(DltRow))
        {
            DltRow <- i
        }else
        {
            DltRow <- c(DltRow, i)
        }
        
    }
}

FITX <- FITX[-DltRow, ]
FITX <- FITX[, -10]

#Dispose of NYF to Get Near Month Futures
n<- dim(NYF)[1]

for (i in 1:n)
{
    for (j in 1:length(settlement))
    {
        if (j == 1)
        {
            if (NYF[i, 1] < settlement[j])
            {
                NYF[i, 10] <- monthCode[j]
                break;
            }
            
        }else
        {
            if (NYF[i, 1] > settlement[j - 1] & NYF[i, 1] < settlement[j])
            {
                NYF[i, 10] <- monthCode[j]
                break;
            }
            
        }
    }
}

NYF$StockId <- as.character.default(NYF$StockId)
names(NYF) <- c("TxDate", "TimeTag", "StockId", "Tick_Price", "Tick_Qty", "B1_Price", "B1_Qty", "A1_Price", "A1_Qty", "Code")

DltRow <- c()
for (i in 1:n)
{
    if (!is.na(NYF[i, 3]) & !is.na(NYF[i, 10]) & substr(NYF[i, 3], 4, 4) != NYF[i, 10])
    {
        if (is.null(DltRow))
        {
            DltRow <- i
        }else
        {
            DltRow <- c(DltRow, i)
        }
        
    }
}

NYF <- NYF[-DltRow, ]
NYF <- NYF[, -10]

TW50 <- merge(Yuanta_50, NYF, by = c("TxDate", "TimeTag"), all = T)
names(TW50) <- c("TxDate", "TimeTag", "StockId", "Last_Price", "Last_Qty", "Bid1_Price", "Bid1_Qty", "Ask1_Price", "Ask1_Qty", 
                 "Fut", "Last_Price_Fut", "Last_Qty_FUT", "Bid1_Price_FUT", "Bid1_Qty_FUT", "Ask1_Price_FUT", "Ask1_Qty_FUT")
TW50 <- na.omit(TW50)

TW_Index <- merge(Index, FITX, by = c("TxDate", "TimeTag"), all = T)
names(TW_Index) <- c("TxDate", "TimeTag", "Code", "Index", "FUT", "Last_Price_FUT", "Last_Qty_FUT", "Bid1_Price_FUT", 
                     "Bid1_Qty_FUT", "Ask1_Price_FUT", "Ask1_Qty_FUT")
TW_Index <- TW_Index[order(TW_Index$TxDate, TW_Index$TimeTag), ]
TW_Index <- na.omit(TW_Index)
TW_Index$Spread_TX <- TW_Index$Tick_Price - TW_Index$Index
TW_Index <- TW_Index[, -c(3:11)]


# Spread <- merge(TW_Index, TW50, by = c("TxDate", "TimeTag"), all = T)
# Spread <- Spread[, -c(4:11, 13:19)]
# Spread <- na.omit(Spread)
# 
# Spread_TX <- Spread$Last_Price_Fut - Spread$Index
# 
# 
# par(mfrow = c(1, 1))
# plot(Spread$TimeTag, Spread$Spread_TX, col = "red")








