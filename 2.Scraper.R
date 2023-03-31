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
library(foreach)


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


#Create subsets per year 
tickers_list <- split(Tickers, Tickers$Year)

# print data frames
for (i in seq_along(tickers_list)) {
  assign(paste0("DF_Tickers_", names(tickers_list[i])), tickers_list[[i]], envir = .GlobalEnv)
}

Tickers_2015 <- DF_Tickers_2015$Ticker
Tickers_2016 <- DF_Tickers_2016$Ticker
Tickers_2017 <- DF_Tickers_2017$Ticker
Tickers_2018 <- DF_Tickers_2018$Ticker
Tickers_2019 <- DF_Tickers_2019$Ticker
Tickers_2020 <- DF_Tickers_2020$Ticker
Tickers_2021 <- DF_Tickers_2021$Ticker
Tickers_2022 <- DF_Tickers_2022$Ticker


Year_2015 <- DF_Tickers_2015$Year
Year_2016 <- DF_Tickers_2016$Year
Year_2017 <- DF_Tickers_2017$Year
Year_2018 <- DF_Tickers_2018$Year
Year_2019 <- DF_Tickers_2019$Year
Year_2020 <- DF_Tickers_2020$Year
Year_2021 <- DF_Tickers_2021$Year
Year_2022 <- DF_Tickers_2022$Year



tickers <- Tickers$Ticker
years <- Tickers$Year
pb <- progress_bar$new(total = length(tickers) * length(years))
result_df <- data.frame()
Error_count <- 0




# for (i in seq_along(tickers)) {
#   tryCatch({
#     temp_df <- DataAccess(tickers[i], years[i])
#     result_df <- rbind(result_df, temp_df)
#     pb$tick()
#     Sys.sleep(1)
#   }, error = function(e) {
#     message(sprintf("Skipping %s %d due to error: %s", tickers[i], years[i], e$message))
#     Error_count <<- Error_count +1 
#   })
# }


# Define a function to process a single ticker and year
process_ticker <- function(ticker, year) {
  tryCatch({
    DataAccess(ticker, year)
  }, error = function(e) {
    message(sprintf("Skipping %s %d due to error: %s", ticker, year, e$message))
    Error_count <<- Error_count + 1
    NULL
  })
}

# Pass vectors of tickers and years to the function
result_list <- foreach(i = seq_along(Tickers_2015), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2015[i], Year_2015[i])
}

result_list_2016 <- foreach(i = seq_along(Tickers_2016), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2016[i], Year_2016[i])
}

result_list_2017 <- foreach(i = seq_along(Tickers_2017), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2017[i], Year_2017[i])
}

result_list_2018 <- foreach(i = seq_along(Tickers_2018), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2018[i], Year_2018[i])
}

result_list_2019 <- foreach(i = seq_along(Tickers_2019), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2019[i], Year_2019[i])
}

result_list_2020 <- foreach(i = seq_along(Tickers_2020), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2020[i], Year_2020[i])
}

result_list_2021 <- foreach(i = seq_along(Tickers_2021), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2021[i], Year_2021[i])
}

result_list_2021 <- foreach(i = seq_along(Tickers_2021), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2021[i], Year_2021[i])
}

result_list_2022 <- foreach(i = seq_along(Tickers_2022), .combine = "rbind") %dopar% {
  process_ticker(Tickers_2022[i], Year_2022[i])
}

write.csv(result_list_2022, "result_2022_df.csv", row.names = FALSE)


result_df <- rbind(result_list,result_list_2016,result_list_2017,result_list_2018,result_list_2019,result_list_2020,result_list_2021,result_list_2022 )
write.csv(result_df, "result_df.csv", row.names = FALSE)


cat(sprintf("Number of errors: %d", Error_count))

