# Term Strucutre - Bond
library(stringr)
library(RODBC)
library(dplyr)

# Connect to ODBC
myCon_41 <- odbcConnect(dsn = 'cmoney_41', uid = 'hank', pwd = 'hank')

# Select Last Trading Date
MySQL <- "select top 2 Tradingdate"
MySQL <- paste0(MySQL, " from [DBMain].[dbo].[Tradingdate]")
MySQL <- paste0(MySQL, " where Tradingdate <= '", substr(Sys.Date(), 1, 10), "'")
MySQL <- paste0(MySQL, " order by Tradingdate desc")

LastTrdDate <- sqlQuery(myCon_41, MySQL)
TrdDate <- c(substr(LastTrdDate[1, 1], 1, 10), substr(LastTrdDate[2, 1], 1, 10))

# Select Convertible Bond
MySQL <- "select 資料日期, 債券代號, 債券名稱, 市場利率, DV01"
MySQL <- paste0(MySQL, " from [PL].[dbo].[債券部位檔]")
MySQL <- paste0(MySQL, " where 資料日期 = '", LastTrdDate[2, 1], "'")

CB <- sqlQuery(myCon_41, MySQL)
colnames(CB) <- c("TxDate", "Bond_Code", "Bond_Name", "ir", "DV01")

# Select Bond Due Date
MySQL <- "select 年月, 名稱, 債券代碼, 發行日期, 到期日期"
MySQL <- paste0(MySQL, " from [Cmoney].[dbo].[債券發行基本資料表]")
MySQL <- paste0(MySQL, " where 到期日期 >= '", substr(Sys.Date(), 1, 10), "' and 債券代碼 is not NULL")
MySQL <- paste0(MySQL, " order by 債券代碼, 年月 desc")

BondDue <- sqlQuery(myCon_41, MySQL)
BondDue <- BondDue %>%
    distinct(BondDue[, 3], .keep_all = TRUE)
n <- dim(BondDue)[2]
BondDue <- BondDue[, -n]
colnames(BondDue) <- c("TxDate", "Bond_Name", "Bond_Code", "Issue_Date", "Due_Date")

# Merge Cb and BondDue
CB <- merge(x = CB, y = BondDue, by = "Bond_Code", all.x = TRUE)
CB <- CB %>%
    mutate(Durat = CB[, 9] - CB[, 2]) %>%
    arrange(Durat) %>%
    select(Bond_Code:DV01, Durat)

# Select Government Bond
BondName <- c("sDate", "yDate")
for (i in 1:length(TrdDate))
{
    LastTrdDate <- TrdDate[i]
    
    MySQL <- "select *"
    MySQL <- paste0(MySQL, " from [TestSherlock].[dbo].[Bond]")
    MySQL <- paste0(MySQL, " where TxDate = '", LastTrdDate, "' and [Bond Code] like 'A0610%'")
    
    GovBond <- sqlQuery(myCon_41, MySQL)
    
    GovBond <- GovBond %>%
        arrange(desc(as.integer(TimeTag))) %>% 
        distinct(GovBond[, 3], .keep_all = TRUE) 
    n <- dim(GovBond)[2]
    GovBond <- GovBond[, -n]
    colnames(GovBond)[3:4] <- c("Bond_Code", "Bond_Name")
    
    # Merge GovBond and BonDue
    GovBond <- merge(x = GovBond, y = BondDue, by = "Bond_Code", all.x = TRUE)
    GovBond <- GovBond %>%
        mutate(Durat = as.integer(GovBond[, 10] - GovBond[, 2])) %>%
        arrange(Durat) %>%
        select(Bond_Code:Ask, Durat)

    temp <- paste0("GovBond_", BondName[i])
    assign(temp, GovBond)
}

# Dispose NA Value in GovBond Duration
GovCode <- c("A06107", "A06105", "A06102", "A06104", "A06108", "A06106")
GovDue <- c(730, 1825, 1825, 3650, 7300, 10950)

Fill_NULL_DueDate <- function(x){
    if (is.na(x[7])){
        for (i in 1:length(GovCode)){
            if (x[1] == GovCode[i]){
                x[7] <- GovDue[i]

            }
        }
    }
    
    return (x)
}

GovBond_sDate <- apply(GovBond_sDate, 1, Fill_NULL_DueDate) %>%
    t() %>% 
    as.data.frame() %>%
    arrange(as.numeric(levels(Durat))[Durat])
GovBond_yDate <- apply(GovBond_yDate, 1, Fill_NULL_DueDate) %>%
    t() %>% 
    as.data.frame() %>%
    arrange(as.numeric(levels(Durat))[Durat])

GovBond_sDate[, 5:7] <- sapply(GovBond_sDate[, 5:7], function(x) as.numeric(as.character(x)))
GovBond_yDate[, 5:7] <- sapply(GovBond_yDate[, 5:7], function(x) as.numeric(as.character(x)))

# Plot Term Structure
par(mfrow = c(1, 1))
plot(GovBond_sDate$Durat, GovBond_sDate$Bid, type = "b", lwd = 1, 
     xlab = "Days", ylab = "interest rate(%)", main = "Term Structure(Bid)")
lines(GovBond_yDate$Durat, GovBond_yDate$Bid, lty = 2, lwd = 1, col = "red")
legend("bottomright", c("Today", "Yesterday"), lty = c(1, 2), lwd = 1, 
       col = c("black", "red"), cex = 0.7)

plot(GovBond_sDate$Durat, GovBond_sDate$Ask, type = "b", lwd = 1, 
     xlab = "Days", ylab = "interest rate(%)", main = "Term Structure(Ask)")
lines(GovBond_yDate$Durat, GovBond_yDate$Ask, lty = 2, lwd = 1, col = "red")
legend("bottomright", c("Today", "Yesterday"), lty = c(1, 2), lwd = 1, 
       col = c("black", "red"), cex = 0.7)

# Calculate Interest Rate Change 
Chg.Rate.Bid <- cbind(GovBond_yDate$Bid, GovBond_sDate$Bid, GovBond_sDate$Durat)
colnames(Chg.Rate.Bid) <- c("Bid_y", "Bid_s", "Durat")
Chg.Rate.Ask <- cbind(GovBond_yDate$Ask, GovBond_sDate$Ask, GovBond_sDate$Durat)
colnames(Chg.Rate.Ask) <- c("Ask_y", "Ask_s", "Durat")

# Interpolation Method
Interpolation <- function(x1, y1, x2, y2, x){
    return(y1 + (x-x1)/(x2-x1) * (y2-y1))
    
}
Find_Range <- function(x, Rate, i){
    if (x < Rate[1, 3]){
        Interpolation(0, 0.1, Rate[1, 3], Rate[1, i], x)
       
    }else if (Rate[1, 3] < x & x < Rate[2, 3]){
        Interpolation(Rate[1, 3], Rate[1, i], Rate[2, 3], Rate[2, i], x)
        
    }else if (Rate[2, 3] < x & x < Rate[3, 3]){
        Interpolation(Rate[2, 3], Rate[2, i], Rate[3, 3], Rate[3, i], x)
        
    }else if (Rate[3, 3] < x & x < Rate[4, 3]){
        Interpolation(Rate[3, 3], Rate[3, i], Rate[4, 3], Rate[4, i], x)
        
    }else if (Rate[4, 3] < x & x < Rate[5, 3]){
        Interpolation(Rate[4, 3], Rate[4, i], Rate[5, 3], Rate[5, i], x)
        
    }else if (Rate[5, 3] < x & x < Rate[6, 3]){
        Interpolation(Rate[5, 3], Rate[5, i], Rate[6, 3], Rate[6, i], x)
        
    }else{
        Interpolation(Rate[6, 3], Rate[6, i], 12775, 2, x)
        
    }
    
}
# Check Missing Value in Bid, Ask
# check_NA_value <- function(x){
#     if (is.na(x)){
#         
#     }
# }
# 
# sapply(Chg.Rate.Bid$Bid_y, check_NA_value)
# 

# Interest Rate Change bp
CB$y.Bid.IR <- sapply(CB[, 6], Find_Range, Chg.Rate.Bid, 1)
CB$s.Bid.IR <- sapply(CB[, 6], Find_Range, Chg.Rate.Bid, 2)
CB$Bid.chg.bp <- (CB$s.Bid.IR - CB$y.Bid.IR) / CB$y.Bid.IR * 10000
CB$DV01.Bid <- (1 + CB$Bid.chg.bp / 10000) * CB$DV01

CB$y.Ask.IR <- sapply(CB[, 6], Find_Range, Chg.Rate.Ask, 1)
CB$s.Ask.IR <- sapply(CB[, 6], Find_Range, Chg.Rate.Ask, 2)
CB$Ask.chg.bp <- (CB$s.Ask.IR - CB$y.Ask.IR) / CB$y.Ask.IR * 10000
CB$DV01.Ask <- (1 + CB$Ask.chg.bp / 10000) * CB$DV01

# Insert Result to [TestSherlock].[dbo].[Instant_DV01.Bond]
Insert_Instant_DV01 <- function(x, MyCon){
    sDate <- substr(Sys.Date(), 1, 10)
    sTime <- gsub(":", "", substr(Sys.time(), 12, 20))
    
    MySQL <- "Insert into [TestSherlock].[dbo].[Instant_DV01.Bond] values('"
    MySQL <- paste0(MySQL, sDate, "', ", sTime, ", '", x[1], "', '", x[3], "', ")
    MySQL <- paste0(MySQL, paste0(x[4:5], collapse = ", "), ", ", paste(x[7:14], collapse = ","), ")")
    
    sqlQuery(MyCon, MySQL)
}
apply(CB, 1, FUN = Insert_Instant_DV01, myCon_41)

# CB <- CB[, -c(6:10)]

# Close Connection to ODBC
odbcClose(myCon_41)
