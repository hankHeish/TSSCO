library(RODBC)
library(rvest)
library(RCurl)

# Download File 
url.path <- "http://www.twse.com.tw/zh/page/trading/exchange/MI_5MINS_INDEX.html"

url <- read_html(url.path)
Index.Data <- url %>%
    html_nodes(".csv") %>%
    html_attr('href')

DLfile <- paste0(CSV.Path, Index.Data)
download.file(url.path, destfile = "MI_5MINS_INDEX_RCralwer.csv", method = "libcurl")

"/exchangeReport/MI_5MINS_INDEX?response=csv&date=20170901"

# Insert DB
myCon_41 <- odbcConnect(dsn = 'cmoney_41', uid = 'hank', pwd = 'hank')

data.import <- read.csv(file = "C:/Users/J1060019/Downloads/MI_5MINS_INDEX.csv",
                        header = F, sep = ",", stringsAsFactors = F)
data.import <- data.import[, -ncol(data.import)]

Yr <- as.integer(substring(data.import[1, 1], 1, 3)) + 1911
sDate <- paste0(Yr, 
                substring(data.import[1, 1], 5, 6), 
                substring(data.import[1, 1], 8, 9))

for (i in 1:nrow(data.import)){
    if(data.import[i, 1] == "時間"){
        sStart <- i + 1
    }
    if (data.import[i, 1] == "說明:"){
        sEnd <- i - 1
    }
}

data.import <- data.import[sStart:sEnd, ]

data.import[, 1] <- sapply(data.import[, 1], function(x) gsub("=", "", x))
for (i in 2:ncol(data.import)){
    data.import[, i] <- sapply(data.import[, i], function(x) as.numeric(gsub(",", "", x)))
}

insert.data <- function(data, myCon){
    mySQL <- "insert into [TestSherlock].[dbo].[MI_5MINS_INDEX] values('"
    mySQL <- paste0(mySQL, sDate, "', '", data[1], "', ", paste0(data[2:35], collapse = ", "), ")")
    
    # cat(mySQL, "\n")
    sqlQuery(myCon, mySQL)
}
apply(data.import, 1, insert.data, myCon_41)


