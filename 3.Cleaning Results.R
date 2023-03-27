
#Load libraries
library(dplyr)
library(XBRL)


#Read results of financial info
Results <- read.csv("result_df.csv")
DF_results <- subset(Results, select = c("val", "fy","desc", "symbol"))


#Clean results 
#First only keep the interesting columns, and then change from long to wide format
#Some variables are stored with different names, every variable needs to be evaluated. 

#Check the sum of the number of unique firms per year , which should equal the number of firms per item
DF_results$FY_symbol <- paste(DF_results$fy, DF_results$symbol, sep=" ")
NumberFirms <- unique(DF_results$FY_symbol)
length(NumberFirms) #answer is 2602
#So each item used should occur 2602 times 

#-----------------------------------------------------------------
#                                                           Revenue

Revenue <- grepl("revenue", DF_results$desc, ignore.case = TRUE)
Revenue <- DF_results[Revenue,]

desc_counts_rev <- Revenue %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))

head(desc_counts_rev,10) #first two names seem to indicate revenue

#Modify 
DF_results$desc <- if_else(DF_results$desc =="RevenueFromContractWithCustomerExcludingAssessedTax", "Revenues",DF_results$desc)
any(grepl("RevenueFromContractWithCustomerExcludingAssessedTax", DF_results$desc, ignore.case = TRUE)) #Check

#name= "Revenues"

#-----------------------------------------------------------------
#Receivable

Receivable <- grepl("Receivable", DF_results$desc, ignore.case = TRUE)
Receivable <- DF_results[Receivable,]

desc_counts_receiv <- Receivable %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_receiv,10) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="ReceivablesNetCurrent", "AccountsReceivableNetCurrent",DF_results$desc)
DF_results$desc <- if_else(DF_results$desc =="AccountsReceivableNetCurrent", "AccountsReceivable",DF_results$desc)
any(grepl("ReceivablesNetCurrent", DF_results$desc, ignore.case = TRUE)) #Check

#Name = AccountsReceivable

#-----------------------------------------------------------------
#CurrentAssets

CurrentAssets <- grepl("Current", DF_results$desc, ignore.case = TRUE)
CurrentAssets <- DF_results[CurrentAssets,]

desc_counts_currentasset <- CurrentAssets %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_currentasset,10) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="AssetsCurrent", "CurrentAssets",DF_results$desc)
any(grepl("AssetsCurrent", DF_results$desc, ignore.case = TRUE)) #Check

#Name = CurrentAssets

#-----------------------------------------------------------------
#CurrentLiabilities

liabilities <- grepl("liabilities", DF_results$desc, ignore.case = TRUE)
liabilities <- DF_results[liabilities,]

desc_counts_currentliabilities <- liabilities %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_currentliabilities,10) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="LiabilitiesCurrent", "CurrentLiabilities",DF_results$desc)
any(grepl("LiabilitiesCurrent", DF_results$desc, ignore.case = TRUE)) #Check

#Name = CurrentLiabilities

#-----------------------------------------------------------------
#Inventory

inventory <- grepl("inventory", DF_results$desc, ignore.case = TRUE)
inventory <- DF_results[inventory,]

desc_counts_inventory <- inventory %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_inventory,10) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="InventoryNet", "Inventory",DF_results$desc)
any(grepl("InventoryNet", DF_results$desc, ignore.case = TRUE)) #Check

#Name = Inventory


#-----------------------------------------------------------------
#Debt

Debt <- grepl("Debt", DF_results$desc, ignore.case = TRUE)
Debt <- DF_results[Debt,]

desc_counts_debt <- Debt %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_debt,20) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="LongTermDebt", "Debt",DF_results$desc)
any(grepl("LongTermDebt", DF_results$desc, ignore.case = TRUE)) #Check

#Name = Debt


#-----------------------------------------------------------------
#equity

equity <- grepl("equity", DF_results$desc, ignore.case = TRUE)
equity <- DF_results[equity,]

desc_counts_equity <- equity %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_equity,20) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="StockholdersEquity", "Equity",DF_results$desc)
any(grepl("StockholdersEquity", DF_results$desc, ignore.case = TRUE)) #Check

#Name = Equity


#-----------------------------------------------------------------
#NET INCOME

income <- grepl("income", DF_results$desc, ignore.case = TRUE)
income <- DF_results[income,]

desc_counts_NetIncome <- income %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_NetIncome,20) #first two names seem to indicate revenue

#Name = NetIncomeLoss



