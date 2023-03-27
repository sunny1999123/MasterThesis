
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
#Revenue

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


#-----------------------------------------------------------------
#Assets

Assets <- grepl("Assets", DF_results$desc, ignore.case = TRUE)
Assets <- DF_results[Assets,]

desc_counts_Assets <- Assets %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_Assets,20) #first two names seem to indicate revenue

#Name = Assets

#-----------------------------------------------------------------
#Cost of goods sold

Cost <- grepl("Cost", DF_results$desc, ignore.case = TRUE)
Cost <- DF_results[Cost,]

desc_counts_CostGoodsSold <- Cost %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_CostGoodsSold,10) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="CostOfGoodsAndServicesSold", "CostGoodsSold",DF_results$desc)
any(grepl("CostOfGoodsAndServicesSold", DF_results$desc, ignore.case = TRUE)) #Check

#Name = CostGoodsSold

#-----------------------------------------------------------------
#Depreciation

Depreciation <- grepl("Depreciation", DF_results$desc, ignore.case = TRUE)
Depreciation <- DF_results[Depreciation,]

desc_counts_Depreciation <- Depreciation %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_Depreciation,10) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="DepreciationDepletionAndAmortization", "DepreciationAmortization",DF_results$desc)
DF_results$desc <- if_else(DF_results$desc =="DepreciationAndAmortization", "Depreciation",DF_results$desc)
any(grepl("DepreciationAndAmortization", DF_results$desc, ignore.case = TRUE)) #Check

#Name = DepreciationAmortization

#-----------------------------------------------------------------
#plant

plant <- grepl("plant", DF_results$desc, ignore.case = TRUE)
plant <- DF_results[plant,]

desc_counts_Plant <- plant %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_Plant,10) #first two names seem to indicate revenue

DF_results$desc <- if_else(DF_results$desc =="PropertyPlantAndEquipmentNet", "PropertyPlantAndEquipment",DF_results$desc)
any(grepl("PropertyPlantAndEquipmentNet", DF_results$desc, ignore.case = TRUE)) #Check

#Name = PropertyPlantAndEquipment

#-----------------------------------------------------------------
#long-term debt

Long_term_debt <- grepl("long", DF_results$desc, ignore.case = TRUE)
Long_term_debt <- DF_results[Long_term_debt,]

desc_counts_LongDebt <- Long_term_debt %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_LongDebt,10) #first two names seem to indicate revenue




DF_results$desc <- if_else(DF_results$desc =="LongTermDebtNoncurrent", "LongTermDebt",DF_results$desc)
any(grepl("LongTermDebt", DF_results$desc, ignore.case = TRUE)) #Check

#Name = LongTermDebt


#-----------------------------------------------------------------
#FixedAssets

FixedAssets <- grepl("current", DF_results$desc, ignore.case = TRUE)
FixedAssets <- DF_results[FixedAssets,]

desc_counts_FixedAssets <- FixedAssets %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_FixedAssets,20) #first two names seem to indicate revenue




DF_results$desc <- if_else(DF_results$desc =="OtherAssetsNoncurrent", "FixedAssets",DF_results$desc)
any(grepl("OtherAssetsNoncurrent", DF_results$desc, ignore.case = TRUE)) #Check


#Name = FixedAssets

#-----------------------------------------------------------------
#EBITDA

OperatingIncome <- grepl("operating", DF_results$desc, ignore.case = TRUE)
OperatingIncome <- DF_results[OperatingIncome,]

desc_counts_OperatingIncome <- OperatingIncome %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_OperatingIncome,20) #first two names seem to indicate revenue

#Name = OperatingIncomeLoss
#EBITDA still needs to be calculated: Operating income +depreciation & amortization


#-----------------------------------------------------------------
#Interest

Interest <- grepl("Interest", DF_results$desc, ignore.case = TRUE)
Interest <- DF_results[Interest,]

desc_counts_Interest <- Interest %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_Interest,20) #first two names seem to indicate revenue




DF_results$desc <- if_else(DF_results$desc =="InterestExpense", "Interest",DF_results$desc)
any(grepl("InterestExpense", DF_results$desc, ignore.case = TRUE)) #Check

#Name = Interest


#-----------------------------------------------------------------
#pre-tax income

PreTaxIncome <- grepl("income", DF_results$desc, ignore.case = TRUE)
PreTaxIncome <- DF_results[PreTaxIncome,]

desc_counts_PreTaxIncome <- income %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_PreTaxIncome,20) #first two names seem to indicate revenue




DF_results$desc <- if_else(DF_results$desc =="IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest", "PreTaxIncome",DF_results$desc)
any(grepl("IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest", DF_results$desc, ignore.case = TRUE)) #Check

#Name = PreTaxIncome

#-----------------------------------------------------------------
#cash

Cash <- grepl("cash", DF_results$desc, ignore.case = TRUE)
Cash <- DF_results[Cash,]

desc_counts_Cash <- Cash %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_Cash,20) #first two names seem to indicate revenue


DF_results$desc <- if_else(DF_results$desc =="CashAndCashEquivalentsAtCarryingValue", "Cash",DF_results$desc)
any(grepl("CashAndCashEquivalentsAtCarryingValue", DF_results$desc, ignore.case = TRUE)) #Check


#Name = cash

#-----------------------------------------------------------------
#dividend

Dividend <- grepl("dividend", DF_results$desc, ignore.case = TRUE)
Dividend <- DF_results[Dividend,]

desc_counts_Dividend <- Dividend %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_Dividend,20) #first two names seem to indicate revenue


DF_results$desc <- if_else(DF_results$desc =="PaymentsOfDividendsCommonStock", "Dividend",DF_results$desc)
any(grepl("PaymentsOfDividendsCommonStock", DF_results$desc, ignore.case = TRUE)) #Check


#Name = Dividend

#-----------------------------------------------------------------
#cash flow Operations

Cash_flow <- grepl("cash", DF_results$desc, ignore.case = TRUE)
Cash_flow <- DF_results[Cash_flow,]

desc_counts_CashFlowOperations <- Cash_flow %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_CashFlowOperations,20) #first two names seem to indicate revenue


DF_results$desc <- if_else(DF_results$desc =="NetCashProvidedByUsedInOperatingActivitiesContinuingOperations", "CashFlowOperations",DF_results$desc)
any(grepl("CashAndCashEquivalentsAtCarryingValue", DF_results$desc, ignore.case = TRUE)) #Check


#Name = CashFlowOperations

#-----------------------------------------------------------------
#Research & Development

Research <- grepl("Research", DF_results$desc, ignore.case = TRUE)
Research <- DF_results[Research,]

desc_counts_ResearchDevelopment <- Research %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_ResearchDevelopment,20) #first two names seem to indicate revenue


DF_results$desc <- if_else(DF_results$desc =="ResearchAndDevelopmentExpense", "ResearchDevelopment",DF_results$desc)
any(grepl("ResearchAndDevelopmentExpense", DF_results$desc, ignore.case = TRUE)) #Check


#Name = ResearchDevelopment

#-------------
#End of variable investigation




InterestedVariables <- c("Revenues","AccountsReceivable", "CurrentAssets","CurrentLiabilities","Inventory",
                         "Debt","Equity", "NetIncomeLoss", "Assets", "CostGoodsSold", "DepreciationAmortization",
                         "PropertyPlantAndEquipment","LongTermDebt","FixedAssets", "OperatingIncomeLoss","Interest", 
                         "PreTaxIncome", "Cash", "Dividend", "CashFlowOperations", "ResearchDevelopment")



Apple_2021 <- filter(DF_results, FY_symbol=="2021 AAPL")




