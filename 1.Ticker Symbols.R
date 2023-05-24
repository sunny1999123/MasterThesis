#PART 1

#Load Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(reshape)
#Import Ticker Symbols 
Ticker_Symbols <- read_excel("Data/Ticker_Symbols.xlsx")


#First impression to data and Convert date column to date
str(Ticker_Symbols$date)
Ticker_Symbols$date <- as.Date(Ticker_Symbols$date)
Ticker_Symbols$year <- format(as.Date(Ticker_Symbols$date, format="%d/%m/%Y"),"%Y")

#Get latest ticker symbols of each year
Ticker_symbols_last <- Ticker_Symbols %>%
  group_by(year) %>%
  summarise_all(last)

#Change from wide to long format and remove date. Only select interested years
Ticker_symbols_last <- subset(Ticker_symbols_last, select = -c(date))
Years <- c("2015","2016","2017","2018","2019","2020","2021","2022")
Ticker_symbols_last <- Ticker_symbols_last[Ticker_symbols_last$year %in%Years,]


Ticker_symbbol_Long_format <- reshape(Ticker_symbols_last,
                       idvar = "year",
                       varying = 2:514,
                       sep="",
                       new.row.names = 1:6156,
                       direction = "long"
                       )
#change colnames
colnames(Ticker_symbbol_Long_format) <- c("Year", "Id","Ticker")

#Export to csv and get industry 
#Create an Unique list of ticker symbols to export to csv (more than 500 because of change in Index)

Unique_Tickers <- Ticker_symbbol_Long_format %>%
  distinct(Ticker, .keep_all = TRUE) %>%
  subset(select= -c(Year, Id))

#Export to text file for WRDS 
write.table(Unique_Tickers, "Data/Unique_Tickers.txt", row.names = F, col.names = F, quote = F)

#Import SIC codes that is retrieved from WRDS
Industry_codes <- read.csv("Data/Industry_Codes.csv", sep=";")

#Change colnames to get similar format
colnames(Industry_codes) <- c("Ticker", "SIC")

#Merge two dataframes and remove NA
Ticker_symbbol_Long_format <- merge(x=Ticker_symbbol_Long_format, y=Industry_codes, by="Ticker", all.Y=TRUE)
Ticker_symbbol_Long_format <- drop_na(Ticker_symbbol_Long_format)

#Remove all firms which SIC codes start with 6 (Financials)
#Get first character of SIC code
#Get Ticker and Year
Ticker_symbbol_Long_format$SicFirstLetter <- substr(Ticker_symbbol_Long_format$SIC,1,1)
Tickers_years <- Ticker_symbbol_Long_format[Ticker_symbbol_Long_format$Year %in% c(2018,2019,2020,2021,2022),]
Tickers <- Ticker_symbbol_Long_format[Ticker_symbbol_Long_format$SicFirstLetter!= "6",]
Tickers <- subset(Tickers, select=c(Ticker, Year))


#Export to CSV 
write.csv(Tickers, "Data/Tickers.csv", row.names = FALSE)





