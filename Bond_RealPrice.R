#Taipei Exchange Bond Interest Rate 
#R Crawler 

library(rvest)
library(tmcn)
library(stringr)
library(RODBC)
library(RCurl)

url.path <-  "http://www.tpex.org.tw"
DL.csv.path <- "C:/Users/J1060019/Desktop/R"
myCon_41 <- odbcConnect(dsn = 'cmoney_41', uid = 'hank', pwd = 'hank')

# 1.公債等值買賣斷即時行情-盤中報價行情表
url <- read_html("http://www.tpex.org.tw/web/bond/tradeinfo/market/ebts_ops/quotes.php?l=zh-tw")

Bond.csv <- url %>% 
    html_nodes(".btn-csv") %>%
    html_attr('href')

csv.path <- paste0(url.path, Bond.csv)
download.file(csv.path, destfile = "Bond Quote.csv", method = "libcurl")

Bond <- read.csv(paste0(DL.csv.path, "/Bond Quote.csv"), header = F)

n <- dim(Bond)[1]
Bond <- Bond[-c(1:3, n), ]
colnames(Bond) <- c("Bond Code", "Bond Name", "Bid", "Ask")

#Insert Into [TestSherlock].[dbo].[Bond]
Insert.Bond <- function(data, myCon){
    if(grepl("-", as.character(data[3]))){
        data[3] <- "NULL"
    }
    if(grepl("-", as.character(data[4]))){
        data[4] <- "NULL"
    }
    sDate <- substr(Sys.time(), 1, 10)
    sTime <- gsub(":", "", substr(Sys.time(), 12, 20))
    
    mySQL <- "insert into [TestSherlock].[dbo].[Bond] values("
    mySQL <- paste(mySQL, sDate, ", ", sTime, ", ", sep = "")
    mySQL <- paste(mySQL, "'", data[1], "', '", data[2], "', ",sep = "")
    mySQL <- paste(mySQL, paste0(data[3:4], collapse = ", "), ")", sep = "")

    sqlQuery(myCon, mySQL)
}
apply(Bond, 1, FUN = Insert.Bond, myCon_41)

# 2.公債等殖附條件即時行情-盤中報價行情表
url <- read_html("http://www.tpex.org.tw/web/bond/tradeinfo/market/ebts_rps/quotes.php?l=zh-tw")

Bond_RPRS.csv <- url %>%
    html_nodes(".btn-csv") %>%
    html_attr('href')

RPRS_csv.path <- paste0(url.path, Bond_RPRS.csv)
download.file(RPRS_csv.path, destfile = "Bond_RPRS.csv", method = "libcurl")

Bond_RPRS <- read.csv(paste0(DL.csv.path, "/Bond_RPRS.csv"), header = F)

len <- dim(Bond_RPRS)[2]
for (i in 1:len){
    if (i == 1){
        RPRS_name <- as.character(Bond_RPRS[3, i])
    }else{
        RPRS_name <- cbind(RPRS_name, as.character(Bond_RPRS[3, i]))
    }
}
Bond_RPRS <- Bond_RPRS[-c(1:3), ]
colnames(Bond_RPRS) <- RPRS_name

#Insert Bond_RPRS into [TestSherlock].[dbo].[Bond_RPRS]
if (dim(Bond_RPRS)[1] > 0){
    
    Insert.Bond_RPRS <- function(data, myCon){
    
        insert.data <- gsub("-", "NULL", data)
        sDate <- substr(Sys.time(), 1, 10)
        sTime <- gsub(":", "", substr(Sys.time(), 12, 20))
        
        mySQL <- "insert into [TestSherlock].[dbo].[Bond_RPRS] values("
        mySQL <- paste0(mySQL, sDate, ", ", sTime, ", ")
        mySQL <- paste0(mySQL, "'", insert.data[1], "', '", insert.data[2], "', ")
        mySQL <- paste0(mySQL, paste0(insert.data[3:20], collapse = ", "), ")")
        
        sqlQuery(myCon, mySQL)
    }
    apply(Bond_RPRS, 1, FUN = Insert.Bond_RPRS, myCon_41)
}


# 3.公司債即時行情- a.等殖買賣斷盤中報價行情
url <- read_html("http://www.tpex.org.tw/web/bond/tradeinfo/market/cb/quotes.php?l=zh-tw")

Bond_CB.csv <- url %>%
    html_nodes(".btn-csv") %>%
    html_attr('href')

CB_csv.path <- paste0(url.path, Bond_CB.csv)
download.file(CB_csv.path, destfile = "Bond_CB.csv", method = "libcurl")

Bond_CB <- read.csv(paste0(DL.csv.path, "/Bond_CB.csv"), header = F)

len <- dim(Bond_CB)[2]
for (i in 1:len){
    if (i == 1){
        CB_name <- as.character(Bond_CB[3, i]) 
    }else{
        CB_name <- cbind(CB_name, as.character(Bond_CB[3, i]))
    }
}
Bond_CB <- Bond_CB[-c(1:4), ]
colnames(Bond_CB) <- CB_name

#Insert Bond_CB into [TestSherlock].[dbo].[Bond_CB]
if (dim(Bond_CB)[1] != 0)
{
    Insert.Bond_CB <- function(data, myCon){
        if (data[3] == "-"){
            data[3] <- "NULL"
        }
        if (data[4] == "-"){
            data[4] <- "NULL"
        }
        sDate <- substr(Sys.time(), 1, 10)
        sTime <- gsub(":", "", substr(Sys.time(), 12, 20))
        
        mySQL <- "insert into [TestSherlock].[dbo].[Bond_CB] values("
        mySQL <- paste0(mySQL, sDate, ", ", sTime, ", ")
        mySQL <- paste(mySQL, "'", data[1], "', '", data[2], "', ",sep = "")
        mySQL <- paste(mySQL, paste0(data[3:4], collapse = ", "), ")", sep = "")
        
        sqlQuery(myCon, mySQL)
    }
    apply(Bond_CB, 1, FUN = Insert.Bond_CB, myCon_41)
}

# 3.公司債即時行情- b.處所議價盤中報價行情
url <- read_html("http://www.tpex.org.tw/web/bond/tradeinfo/market/fixed_income/WAW30601.php?l=zh-tw")

Bond_FX.csv <- url %>% 
    html_nodes(".btn-csv") %>%
    html_attr('href')

FX_csv.path <- paste0(url.path, Bond_FX.csv)
download.file(FX_csv.path, destfile = "Bond_FX.csv", method = "libcurl")

Bond_FX <- read.csv(paste0(DL.csv.path, "/Bond_FX.csv"), header = F)

len <- dim(Bond_FX)[2]
for (i in 1:len){
    if (i == 1){
        FX_name <- as.character(Bond_FX[3, i])
    }else{
        FX_name <- cbind(FX_name, as.character(Bond_FX[3, i]))
    }
}

Bond_FX <- Bond_FX[-c(1:3), ]
colnames(Bond_FX) <- FX_name

#Insert Bond_FX into [TestSherlock].[dbo].[Bond_FX]
if (dim(Bond_FX)[1] != 0){
    
    Insert.Bond_FX <- function(data, myCon){
        insert.data <- gsub("-", "NULL", data)
        sDate <- substr(Sys.time(), 1, 10)
        sTime <- gsub(":", "", substr(Sys.time(), 12, 20))
        
        mySQL <- "insert into [TestSherlock].[dbo].[Bond_FX] values("
        mySQL <- paste0(mySQL, sDate, ", ", sTime, ", ")
        mySQL <- paste(mySQL, "'", insert.data[1], "', '", insert.data[2], "', ",sep = "")
        mySQL <- paste(mySQL, paste0(as.character(insert.data[3:4]), collapse = ","), ", ", sep = "")
        mySQL <- paste(mySQL, paste0(insert.data[5:9], collapse = ", "), ")", sep = "")
        
        # print(mySQL)
        sqlQuery(myCon, mySQL)
    }
    
    apply(Bond_FX, 1, FUN = Insert.Bond_FX, myCon_41)
}




