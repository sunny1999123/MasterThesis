#Load Libraries
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(edgarWebR)
library(xml2)
library(XBRL)
library(tidyr)
library(dplyr)
library(XBRL)
library(edgar)    
library(httr)
library(plyr)
library(stringr)
library(jsonlite)
library(data.table)
library(progress)

#Import Ticker Symbols and extra CIK report, only use first three columns
Tickers <- read.csv("Tickers.csv")
cik_ticker <- read.csv("cik_ticker.csv", sep = ";")
cik_ticker <- subset(cik_ticker, select = c(1:3))

#Get CIK symbols per ticker and make a dataframe of it from SEC api
INFO <- read_json("https://www.sec.gov/files/company_tickers.json")
INFO <- rbindlist(INFO)

#Check which Tickers are not in Info DF
Difference <- setdiff(Tickers$Ticker, INFO$ticker)

#Check with difference of second CIK file (cik_ticker)
Difference_2 <- setdiff(Difference, cik_ticker$Ticker)

#How many CIK symbols can I retrieve from cik_ticker
Difference_3 <- setdiff(Difference, Difference_2)
subset_tickers <- distinct(cik_ticker[cik_ticker$Ticker %in%Difference_3,],Ticker, .keep_all = TRUE)
colnames(subset_tickers) <- c("cik_str", "ticker", "title")

#Combine the two dataframes
INFO <- rbind(INFO, subset_tickers)

#There are still thirteen tickers left, these are manually added
Difference_2
new_rows <- data.frame(cik_str=c(1769824, 1867883, 1310243, 14693, 1067983, 1064728, 1620546, 1629995, 1646383,1024333, 801898, 813828, 865436)
                       ,ticker=c("AABA" , "ANDV"  ,"ANRZQ" ,"BF.B"  ,"BRK.B" ,"BTUUQ" ,"BXLT" , "CPGX" , "CSRA" , "DISCK" ,"JOY"  , "VIAB" , "WFM")
                       ,title=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
INFO <- rbind(INFO, new_rows)

#Add zero's before CIK, needed for API and store as data frame
INFO$CIK = do.call(rbind, lapply(as.list(1:nrow(INFO)), function(ii){
  ZEROS = 10-as.numeric(str_count(INFO$cik_str[ii]))
  paste0(c(rep(0,ZEROS),INFO$cik_str[ii]), collapse = "")
}))
INFO <- as.data.frame(INFO)

#Create function that takes ticker as input and gives CIK as output. Including test
getCIK = function(symbol){
  subset(INFO, INFO$ticker == paste(symbol))$CIK
}

#Create function that retrieves
DataAccess = function(ticker, year){
  CIK = getCIK(ticker)
  url <- paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",CIK,".json")
  pg <- GET(url = url,
            config = httr::add_headers(`User-Agent` = "Sunny Bhatia 590913ab@student.eur.nl",
                                       `Accept-Encoding` = 'gzip, deflate'))
  data_raw <- try(content(pg, as="text", encoding="UTF-8") %>% fromJSON(pg, flatten=TRUE),silent = TRUE)
  N = length(data_raw$facts$dei)
  if(N >= 1)
  {
    DEI = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$dei[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$dei)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      filter(tmp, form=="10-K",fy==year, substr(end,1,4)==year)
    }),use.names=TRUE, fill=TRUE)}else{
      DEI = NULL
  }
  N = length(data_raw$facts$`us-gaap`)
  if(N >= 1)
  {
    GAAP = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$`us-gaap`[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$`us-gaap`)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      filter(tmp, form=="10-K",fy==year, substr(end,1,4)==year)
    }),use.names=TRUE, fill=TRUE)
    # re-order
    GAAP = GAAP[,c("start","end","val","accn","fy","fp","form","filed","frame","desc","symbol" )]
  }else{
    GAAP = NULL
  }
  N = length(data_raw$facts$invest)
  if(N >= 1)
  {
    INVEST = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$invest[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$invest)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      filter(tmp, form=="10-K",fy==year, substr(end,1,4)==year)
    }),use.names=TRUE, fill=TRUE)
  }else{
    INVEST = NULL
  }
  N = length(data_raw$facts$srt)
  if(N >= 1)
  {
    SRT = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$srt[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$srt)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      filter(tmp, form=="10-K",fy==year, substr(end,1,4)==year)
    }),use.names=TRUE, fill=TRUE)
  }else{
    SRT = NULL
  }
  # combine ALL data
  ALL <- rbind.fill(GAAP,DEI,SRT,INVEST)
  # return data frame
  ALL
}

tickers <- Tickers$Ticker
years <- Tickers$Year
pb <- progress_bar$new(total = length(tickers) * length(years))
result_df <- data.frame()
Error_count <- 0


for (i in seq_along(tickers)) {
  tryCatch({
    temp_df <- DataAccess(tickers[i], years[i])
    result_df <- rbind(result_df, temp_df)
    pb$tick()
    Sys.sleep(1)
  }, error = function(e) {
    message(sprintf("Skipping %s %d due to error: %s", tickers[i], years[i], e$message))
    Error_count <<- Error_count +1 
  })
}



depreciation_rows <- grepl("depreciation", result_df$desc, ignore.case = TRUE)

results_depreciation <- result_df[depreciation_rows, ]


ticker_symbols <- unique(as.character(result_df$symbol))
ticker_symbols[139]
ticker_symbols2 <- unique(tickers)
ticker_symbols3 <- ticker_symbols2[1:148]

Difference_4 <-setdiff(ticker_symbols3,ticker_symbols)
Difference_2

cat(sprintf("Number of errors: %d", Error_count))

