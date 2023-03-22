#PART 1

#Set Working Directory
#setwd("C:/Users/sunny/Desktop/Thesis/Data Analysis")

#Load Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(reshape)
#Import Ticker Symbols 
Ticker_Symbols <- read_excel("Ticker_Symbols.xlsx")


#First impression to data and Convert date column to date
str(Ticker_Symbols$date)
Ticker_Symbols$date <- as.Date(Ticker_Symbols$date)
Ticker_Symbols$year <- format(as.Date(Ticker_Symbols$date, format="%d/%m/%Y"),"%Y")

#Get latest ticker symbols of each year
Ticker_symbols_last <- Ticker_Symbols %>%
  group_by(year) %>%
  summarise_all(last)

#Change from wide to long format and remove date 
Ticker_symbols_last <- subset(Ticker_symbols_last, select = -c(date))



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
write.table(Unique_Tickers, "Unique_Tickers.txt", row.names = F, col.names = F, quote = F)

#Import SIC codes that is retrieved from WRDS
Industry_codes <- read.csv("Industry_Codes.csv", sep=";")

#Change colnames to get similar format
colnames(Industry_codes) <- c("Ticker", "SIC")

#Merge two dataframes and remove NA
Ticker_symbbol_Long_format <- merge(x=Ticker_symbbol_Long_format, y=Industry_codes, by="Ticker", all.Y=TRUE)
Ticker_symbbol_Long_format <- drop_na(Ticker_symbbol_Long_format)

#Remove all firms which SIC codes start with 6 (Financials)
#Get first character of SIC code
#Get Ticker and Year
Ticker_symbbol_Long_format$SicFirstLetter <- substr(Ticker_symbbol_Long_format$SIC,1,1)
Tickers <- Ticker_symbbol_Long_format[Ticker_symbbol_Long_format$SicFirstLetter!= "6",]
Tickers <- subset(Tickers, select=c(Ticker, Year))


#Export to CSV 
write.csv(Tickers, "Tickers.csv", row.names = FALSE)





