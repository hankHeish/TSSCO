#R Crawler
library(rvest)
library(tmcn)
library(stringr)

#IMDB
html <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- html_nodes(html, "span.itemprop")
html_text(cast)

cast <- html_nodes(html, "#titleCast .itemprop")
html_text(cast)

cast <- html_nodes(html, "#titleCast span.itemprop")
html_text(cast)

lego.movie <- read_html("http://www.imdb.com/title/tt1490017/")
rating <- html_nodes(lego.movie, "strong span") %>% html_text()
# html_text(rating)

poster <- lego.movie %>%
          html_nodes(".poster img") %>%
          html_attr("src")

#Liberty Times News
html <- read_html("http://news.ltn.com.tw/list/BreakingNews")
news.title <- html_nodes(html, ".tit")
news.title.utf8 <- toUTF8(html_text(news.title))
title.href <- html_attr(news.title, "href")
my.news <- data.frame(title = news.title.utf8, href = title.href, stringsAsFactors = FALSE)


#Apple Daily News
ApplyDaily <- "http://www.appledaily.com.tw/"
url.main <- paste0(ApplyDaily, "realtimenews/section/new/")
apple.news <- read_html(url.main)

news.time <- html_text(html_nodes(apple.news, '.rtddt time'))
news.title <- html_text(html_nodes(apple.news, '.rtddt h1'))
news.category <- html_text(html_nodes(apple.news, '.rtddt h2'))
news.url <- html_attr(html_nodes(apple.news, '.rtddt a'), 'href')

realtimenews <- data.frame(time = news.title, title = news.title, category = news.category, 
                           url = paste0(ApplyDaily, news.url))

getContent <- function(x){
    tag <- html_nodes(read_html(x), '.trans')
    text <- html_text(tag)
    text
}
getContent(as.character(realtimenews$url[1]))

# =============================Bank of Taiwan Interest Rate==================================
#Interest Rate 
BOT.rate <- read_html("http://rate.bot.com.tw/xrt?lang=zh-tw")

dollar <- html_nodes(BOT.rate, ".print_show")
dollar.utf8 <- toUTF8(html_text(dollar))

# news.category <- html_text(html_nodes(apple.news, '.rtddt h2'))
# Cash_Bid <- html_text(html_nodes(BOT.rate, ".print_hide"))
# Cash <- as.matrix(Cash_Bid)

Spot <- html_text(html_nodes(BOT.rate, ".print_width"))
Mtx <- as.matrix(Spot)

Spot_Bid <- html_text(html_nodes(BOT.rate, ".print_width"))
Spot_Bid <- as.matrix(Spot_Bid)

# Cash_Bid_utf8 <- toUTF8(html_text(Cash_Bid))
Rate <- data.frame(currency = dollar.utf8)


# =============================Yahoo Finance Domestic Fund==================================
yahoo.finance <- read_html("https://tw.money.yahoo.com/fund/domestic")

fund.comp <- html_text(html_nodes(yahoo.finance, ".Bgc-w"))
# fund.comp <- html_text(html_nodes(yahoo.finance, ".Pstart-4"))


Fund <- data.frame(Comp = fund.comp)
# ==========================================================================================

#PTT R-Language
url.main <- 'https://www.ptt.cc/bbs/R_Language/index.html'
href.title <- html_nodes(read_html(url.main), ".title a")
R.hrefs <- html_attr(href.title, 'href')

R.article.data <- c()
for (i in 1:length(R.hrefs))
{
    article.url <- paste0('https://www.ptt.cc/', R.hrefs[i])
    article <- html_nodes(read_html(article.url), "#main-content")
    article.content <- html_text(article)
    article.utf8 <- iconv(article.content, 'utf8')
    R.article.data <- c(R.article.data, article.utf8)
    
    Sys.sleep(sample(3:5, 1))
}

#Read Weather Data
get_tianqi_table <- function(city = "taibei", year.from = 2015, year.to = 2016)
{
    month <- c(paste0("0", 1:9), "10", "11", "12")
    url.list <- c()
    for (year in year.from:year.to)
    {
        url <- paste0("http://lishi.tianqi.com/taibei/", year, month, ".html")
        # print(url)
        url.list <- c(url.list, url)
    }
    
    tianqi.table <- NULL
    
    for (x in url.list)
    {
        html <- read_html(x)
        wdata <- html_text(html_nodes(html, '.tqtongji2'))
        content <- toTrad(wdata)
        
        content.tmp <- str_replace_all(content, "[\r\n\t]", "")
        content.tmp2 <- strsplit(str_trim(content.tmp), "\\s+")[[1]]
        tmp <- as.data.frame(matrix(content.tmp[-(1:6)], ncol = 6, byrow = T))
        
        colnames(tmp) <- content.tmp2[1:6]
        tianqi.table <- rbind(tianqi.table, tmp)
        
    }
    # tianqi.table
}









