
#Load libraries
library(dplyr)
library(XBRL)
library(tidyr)
library(ggplot2)

#Read results of financial info
Results <- read.csv("result_df.csv")
Results_2022 <- read.csv("result_2022_df.csv")
Results_2022_remaining <- read.csv("result_2022_remaining_df.csv")
Results <- rbind(Results,Results_2022,Results_2022_remaining)

#Delete quarterly data 
# Frames <- unique(Results$frame)
# FramesUsed <- c(NA,"CY2015", "CY2016", "CY2017", "CY2018", "CY2019", "CY2020", "CY2021", "CY2022")
# Results <-Results[Results$frame %in% FramesUsed,]

#Modify start and end to date format
Results$start <- as.Date(Results$start)
Results$end <- as.Date(Results$end)

#Keep only last row of each firm, each year, each financial statement item (deletions are quarterly numbers)
df_filtered <- Results %>%
  arrange(symbol, fy, end) %>%  # sort by symbol, fy, and end
  group_by(symbol, fy, desc) %>%  # group by symbol and fy
  slice_tail(n = 1) %>%  # keep only the last row within each group
  ungroup()  # remove grouping

#Get a subset of the data and change order
DF_results <- subset(df_filtered, select = c("symbol", "fy","desc","val" ))


#Clean results 
#Some variables are stored with different names, every variable needs to be evaluated. 

#Check the sum of the number of unique firms per year , which should equal the number of firms per item
DF_results$FY_symbol <- paste(DF_results$fy, DF_results$symbol, sep=" ")
NumberFirms <- unique(DF_results$FY_symbol)
length(NumberFirms) #answer is 2999
#So each item used should occur 2999 times 

#VARIABLE INVESTIGATION


#-----------------------------------------------------------------
#Revenue

#Check all items that consist the name of the variable
Revenue <- grepl("revenue", DF_results$desc, ignore.case = TRUE)
Revenue <- DF_results[Revenue,]

#Create subset with count of occurence
desc_counts_rev <- Revenue %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
desc_counts_rev$desc

#(Removal of) labels that are (not) of interest
Not_Rev <- c("CostOfRevenue", "InterestRevenueExpenseNet")

#Create list of interesting variables
Rev_list <- desc_counts_rev[!(desc_counts_rev$desc %in% Not_Rev), ]$desc

#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    rev_index = match(desc, Rev_list),
    rev_index = ifelse(is.na(rev_index), Inf, rev_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, rev_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Rev_list & !any(desc == "Revenues") & row_number() == match(desc[desc %in% Rev_list][1], desc),
      "Revenues",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-rev_index)

#Modify 
# DF_results <- DF_results %>%
#   group_by(FY_symbol) %>%
#   mutate(
#     desc = ifelse(
#       desc == "RevenueFromContractWithCustomerExcludingAssessedTax" & 
#         !any(desc == "Revenues"), 
#       "Revenues", 
#       desc
#     )
#   ) %>%
#   ungroup()
# 


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
desc_counts_receiv$desc

#(Removal of) labels that are (not) of interest
Receiv <- c("AccountsReceivableNetCurrent", "ReceivablesNetCurrent", "AccountsReceivableGrossCurrent", "AccountsNotesAndLoansReceivableNetCurrent",
            "AccountsReceivableNet","AccountsAndOtherReceivablesNetCurrent", "AccountsReceivableNetNoncurrent", "OtherReceivablesGrossCurrent",
            "AccountsReceivableGross", "AccountsAndNotesReceivableNet", "ReceivablesFromCustomers", "AccountsReceivableSale",
            "AccountsReceivableGrossNoncurrent")

#Create list of interesting variables (Not always needed)
Receiv_list <- desc_counts_receiv[(desc_counts_receiv$desc %in% Receiv), ]$desc

#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    receiv_index = match(desc, Receiv_list),
    receiv_index = ifelse(is.na(receiv_index), Inf, receiv_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, receiv_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Receiv_list & !any(desc == "AccountsReceivable") & row_number() == match(desc[desc %in% Receiv_list][1], desc),
      "AccountsReceivable",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-receiv_index)



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
desc_counts_currentasset$desc

#(Removal of) labels that are (not) of interest
Cur_ass <- c("AssetsCurrent", "OtherAssetsCurrent")

#Create list of interesting variables (Not always needed)
Curr_ass_list <- desc_counts_currentasset[(desc_counts_currentasset$desc %in% Cur_ass), ]$desc

#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Curr_ass_index = match(desc, Curr_ass_list),
    Curr_ass_index = ifelse(is.na(Curr_ass_index), Inf, Curr_ass_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Curr_ass_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Curr_ass_list & !any(desc == "CurrentAssets") & row_number() == match(desc[desc %in% Curr_ass_list][1], desc),
      "CurrentAssets",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Curr_ass_index)


#Name = CurrentAssets

#-----------------------------------------------------------------
#CurrentLiabilities

liabilities <- grepl("Current", DF_results$desc, ignore.case = TRUE)
liabilities <- DF_results[liabilities,]

desc_counts_currentliabilities <- liabilities %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_currentliabilities,10) #first two names seem to indicate revenue
desc_counts_currentliabilities$desc

#(Removal of) labels that are (not) of interest
Cur_lia <- c("LiabilitiesCurrent", "OtherLiabilitiesCurrent", "OtherAccruedLiabilitiesCurrent","ContractWithCustomerLiabilityCurrent")

#Create list of interesting variables (Not always needed)
Curr_lia_list <- desc_counts_currentliabilities[(desc_counts_currentliabilities$desc %in% Cur_lia), ]$desc

#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Curr_lia_index = match(desc, Curr_lia_list),
    Curr_lia_index = ifelse(is.na(Curr_lia_index), Inf, Curr_lia_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Curr_lia_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Curr_lia_list & !any(desc == "CurrentLiabilities") & row_number() == match(desc[desc %in% Curr_lia_list][1], desc),
      "CurrentLiabilities",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Curr_lia_index)

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
desc_counts_inventory$desc

#(Removal of) labels that are (not) of interest
Inve <- c("InventoryNet", "InventoryFinishedGoodsNetOfReserves","InventoryFinishedGoods","InventoryGross","LIFOInventoryAmount","InventoryNoncurrent"
             , "InventorySuppliesNetOfReserves")

#Create list of interesting variables (Not always needed)
Inve_list <- desc_counts_inventory[(desc_counts_inventory$desc %in% Inve), ]$desc


#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Curr_inv_index = match(desc, Inve_list),
    Curr_inv_index = ifelse(is.na(Curr_inv_index), Inf, Curr_inv_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Curr_inv_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Inve_list & !any(desc == "Inventory") & row_number() == match(desc[desc %in% Inve_list][1], desc),
      "Inventory",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Curr_inv_index)



#Name = Inventory


#-----------------------------------------------------------------
#Debt
# 
# Debt <- grepl("Debt", DF_results$desc, ignore.case = TRUE)
# Debt <- DF_results[Debt,]
# 
# desc_counts_debt <- Debt %>%
#   group_by(desc) %>%
#   summarise(count = n())%>%
#   arrange(desc(count))
# head(desc_counts_debt,20) #first two names seem to indicate revenue
# desc_counts_debt$desc
# 
# 
# 
# #(Removal of) labels that are (not) of interest
# Debt <- c("LongTermDebtNoncurrent", "LongTermDebtCurrent", "LongTermDebt","LongTermDebtAndCapitalLeaseObligations","DebtCurrent"
#               , "LongTermDebtFairValue", "DebtInstrumentFaceAmount", "SecuredDebt","UnsecuredDebt")
# 
# #Create list of interesting variables (Not always needed)
# Debt_list <- desc_counts_debt[(desc_counts_debt$desc %in% Debt), ]$desc
# 
# 
# 
# 
# #Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
# DF_results <- DF_results %>%
#   group_by(FY_symbol) %>%
#   mutate(
#     Curr_debt_index = match(desc, Debt_list),
#     Curr_debt_index = ifelse(is.na(Curr_debt_index), Inf, Curr_debt_index) # move non-matching to the end
#   ) %>%
#   arrange(FY_symbol, Curr_debt_index) %>%
#   mutate(
#     desc = ifelse(
#       desc %in% Debt_list & !any(desc == "Debt") & row_number() == match(desc[desc %in% Debt_list][1], desc),
#       "Debt",
#       desc
#     )
#   ) %>%
#   ungroup() %>%
#   select(-Curr_debt_index)
# 

#Name = Debt
#DEBT IS LATER CALCULATED

#-----------------------------------------------------------------
#equity

equity <- grepl("equity", DF_results$desc, ignore.case = TRUE)
equity <- DF_results[equity,]

desc_counts_equity <- equity %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_equity,20) #first two names seem to indicate revenue
desc_counts_equity$desc


#(Removal of) labels that are (not) of interest
Equity <- c("StockholdersEquity", "StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest"
              , "StockholdersEquityOther")

#Create list of interesting variables (Not always needed)
Equity_list <- desc_counts_equity[(desc_counts_equity$desc %in% Equity), ]$desc




#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Curr_equity_index = match(desc, Equity_list),
    Curr_equity_index = ifelse(is.na(Curr_equity_index), Inf, Curr_equity_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Curr_equity_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Equity_list & !any(desc == "Equity") & row_number() == match(desc[desc %in% Equity_list][1], desc),
      "Equity",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Curr_equity_index)




#Name = Equity

#-----------------------------------------------------------------
#NET INCOME

income <- grepl("income", DF_results$desc, ignore.case = TRUE)
income <- DF_results[income,]

desc_counts_NetIncome <- income %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_NetIncome,20) 
desc_counts_NetIncome$desc



#(Removal of) labels that are (not) of interest
Income <- c("NetIncomeLoss", "ComprehensiveIncomeNetOfTax"
                , "OperatingIncomeLoss","IncomeLossFromContinuingOperationsBeforeIncomeTaxesForeign",
                "IncomeLossFromContinuingOperationsBeforeIncomeTaxesDomestic",
                "IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest,
                ")

#Create list of interesting variables (Not always needed)
Income_list <- desc_counts_NetIncome[(desc_counts_NetIncome$desc %in% Income), ]$desc




#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Curr_income_index = match(desc, Income_list),
    Curr_income_index = ifelse(is.na(Curr_income_index), Inf, Curr_income_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Curr_income_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Income_list & !any(desc == "NetIncomeLoss") & row_number() == match(desc[desc %in% Income_list][1], desc),
      "NetIncomeLoss",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Curr_income_index)




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
desc_counts_Assets$desc


#(Removal of) labels that are (not) of interest
Assets <- c("Assets")

#Create list of interesting variables (Not always needed)
Assets_list <- desc_counts_Assets[(desc_counts_Assets$desc %in% Assets), ]$desc




#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Assets_index = match(desc, Assets_list),
    Assets_index = ifelse(is.na(Assets_index), Inf, Assets_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Assets_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Assets_list & !any(desc == "Assets") & row_number() == match(desc[desc %in% Assets_list][1], desc),
      "Assets",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Assets_index)






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
desc_counts_CostGoodsSold$desc

#(Removal of) labels that are (not) of interest
COGS <- c("CostOfGoodsAndServicesSold","CostOfRevenue")

#Create list of interesting variables (Not always needed)
Cogs_list <- desc_counts_CostGoodsSold[(desc_counts_CostGoodsSold$desc %in% COGS), ]$desc




#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    COGS_index = match(desc, Cogs_list),
    COGS_index = ifelse(is.na(COGS_index), Inf, COGS_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, COGS_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Cogs_list & !any(desc == "CostGoodsSold") & row_number() == match(desc[desc %in% Cogs_list][1], desc),
      "CostGoodsSold",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-COGS_index)


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
desc_counts_Depreciation$desc

#(Removal of) labels that are (not) of interest
Depre <- c("DepreciationDepletionAndAmortization","Depreciation", "DepreciationAndAmortization",
           "DepreciationAmortizationAndAccretionNet", "DepreciationNonproduction"
           )

#Create list of interesting variables (Not always needed)
Depre_list <- desc_counts_Depreciation[(desc_counts_Depreciation$desc %in% Depre), ]$desc




#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Depre_index = match(desc, Depre_list),
    Depre_index = ifelse(is.na(Depre_index), Inf, Depre_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Depre_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Depre_list & !any(desc == "DepreciationAmortization") & row_number() == match(desc[desc %in% Depre_list][1], desc),
      "DepreciationAmortization",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Depre_index)











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

desc_counts_Plant$desc

#(Removal of) labels that are (not) of interest
Pla <- c("PropertyPlantAndEquipmentNet","PropertyPlantAndEquipmentGross", "PropertyPlantAndEquipmentAdditions",
           "PropertyPlantAndEquipmentOther", "PropertyPlantAndEquipmentDisposals","PropertyPlantAndEquipmentAndFinanceLeaseRightOfUseAssetAfterAccumulatedDepreciationAndAmortization"
)

#Create list of interesting variables (Not always needed)
Plant_list <- desc_counts_Plant[(desc_counts_Plant$desc %in% Pla), ]$desc




#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Plant_index = match(desc, Plant_list),
    Plant_index = ifelse(is.na(Plant_index), Inf, Plant_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Plant_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Plant_list & !any(desc == "PropertyPlantAndEquipment") & row_number() == match(desc[desc %in% Plant_list][1], desc),
      "PropertyPlantAndEquipment",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Plant_index)


# DF_results <- DF_results %>%
#   group_by(FY_symbol) %>%
#   mutate(
#     desc = ifelse(
#       desc == "PropertyPlantAndEquipmentNet" &
#         !any(desc == "PropertyPlantAndEquipment"),
#       "PropertyPlantAndEquipment",
#       desc
#     )
#   ) %>%
#   ungroup()

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
desc_counts_LongDebt$desc


#(Removal of) labels that are (not) of interest
LongTermDebt <- c("LongTermDebtNoncurrent", "LongTermDebt",
         "LongTermDebtAndCapitalLeaseObligations", "LongTermDebtFairValue",
         "OtherLongTermDebt", "UnsecuredLongTermDebt")

#Create list of interesting variables (Not always needed)
Longtermdebt_list <- desc_counts_LongDebt[(desc_counts_LongDebt$desc %in% LongTermDebt), ]$desc




#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    LongDebt_index = match(desc, Longtermdebt_list),
    LongDebt_index = ifelse(is.na(LongDebt_index), Inf, LongDebt_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, LongDebt_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Longtermdebt_list & !any(desc == "LongTermDebt") & row_number() == match(desc[desc %in% Longtermdebt_list][1], desc),
      "LongTermDebt",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-LongDebt_index)



#Name = LongTermDebt


#-----------------------------------------------------------------
#FixedAssets

FixedAssets <- grepl("assets", DF_results$desc, ignore.case = TRUE)
FixedAssets <- DF_results[FixedAssets,]

desc_counts_FixedAssets <- FixedAssets %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_FixedAssets,20) #first two names seem to indicate revenue
desc_counts_FixedAssets$desc

#(Removal of) labels that are (not) of interest
FixedAss <- c("NoncurrentAssets", "AssetsNoncurrent"
)

#Create list of interesting variables (Not always needed)
FixedAss_list <- desc_counts_FixedAssets[(desc_counts_FixedAssets$desc %in% FixedAss), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    FixedAssets_index = match(desc, FixedAss_list),
    FixedAssets_index = ifelse(is.na(FixedAssets_index), Inf, FixedAssets_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, FixedAssets_index) %>%
  mutate(
    desc = ifelse(
      desc %in% FixedAss_list & !any(desc == "FixedAssets") & row_number() == match(desc[desc %in% FixedAss_list][1], desc),
      "FixedAssets",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-FixedAssets_index)








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
desc_counts_OperatingIncome$desc


#(Removal of) labels that are (not) of interest
OperInc <- c("OperatingIncomeLoss","OtherOperatingIncome")

#Create list of interesting variables (Not always needed)
OperInc_list <- desc_counts_OperatingIncome[(desc_counts_OperatingIncome$desc %in% OperInc), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    OperInc_index = match(desc, OperInc_list),
    OperInc_index = ifelse(is.na(OperInc_index), Inf, OperInc_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, OperInc_index) %>%
  mutate(
    desc = ifelse(
      desc %in% OperInc_list & !any(desc == "OperatingIncomeLoss") & row_number() == match(desc[desc %in% OperInc_list][1], desc),
      "OperatingIncomeLoss",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-OperInc_index)



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
desc_counts_Interest$desc


#(Removal of) labels that are (not) of interest
Inte <- c("InterestExpense","InterestPaidNet", "InterestPaid","InterestExpenseOther")

#Create list of interesting variables (Not always needed)
Inte_list <- desc_counts_Interest[(desc_counts_Interest$desc %in% Inte), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Inte_index = match(desc, Inte_list),
    Inte_index = ifelse(is.na(Inte_index), Inf, Inte_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Inte_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Inte_list & !any(desc == "Interest") & row_number() == match(desc[desc %in% Inte_list][1], desc),
      "Interest",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Inte_index)






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

desc_counts_PreTaxIncome$desc


#(Removal of) labels that are (not) of interest
PretaxInc <- c("IncomeLossFromContinuingOperationsBeforeIncomeTaxesForeign","IncomeLossFromContinuingOperationsBeforeIncomeTaxesMinorityInterestAndIncomeLossFromEquityMethodInvestments"
               , "IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest")

#Create list of interesting variables (Not always needed)
PretaxInc_list <- desc_counts_PreTaxIncome[(desc_counts_PreTaxIncome$desc %in% PretaxInc), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Pretaxinc_index = match(desc, PretaxInc_list),
    Pretaxinc_index = ifelse(is.na(Pretaxinc_index), Inf, Pretaxinc_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Pretaxinc_index) %>%
  mutate(
    desc = ifelse(
      desc %in% PretaxInc_list & !any(desc == "PreTaxIncome") & row_number() == match(desc[desc %in% PretaxInc_list][1], desc),
      "PreTaxIncome",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Pretaxinc_index)





#Name = PreTaxIncome

#-----------------------------------------------------------------
#cash

Cash <- grepl("cash", DF_results$desc, ignore.case = TRUE)
Cash <- DF_results[Cash,]

desc_counts_Cash <- Cash %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_Cash,40) #first two names seem to indicate revenue
desc_counts_Cash$desc

#(Removal of) labels that are (not) of interest
Cas <- c("CashAndCashEquivalentsAtCarryingValue","CashCashEquivalentsRestrictedCashAndRestrictedCashEquivalentsPeriodIncreaseDecreaseIncludingExchangeRateEffect",
         "CashCashEquivalentsRestrictedCashAndRestrictedCashEquivalents","Cash","CashEquivalentsAtCarryingValue","CashAndCashEquivalentsFairValueDisclosure"
         
)

#Create list of interesting variables (Not always needed)
Cash_list <- desc_counts_Cash[(desc_counts_Cash$desc %in% Cas), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Cash_index = match(desc, Cash_list),
    Cash_index = ifelse(is.na(Cash_index), Inf, Cash_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Cash_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Cash_list & !any(desc == "Cash") & row_number() == match(desc[desc %in% Cash_list][1], desc),
      "Cash",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Cash_index)



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
desc_counts_Dividend$desc


#(Removal of) labels that are (not) of interest
Div <- c("PaymentsOfDividendsCommonStock","DividendsCommonStockCash",
         "PaymentsOfDividends","DividendsCash","DividendsCommonStock","Dividends", "DividendsPreferredStock","DividendsCommonStockStock"
         
)

#Create list of interesting variables (Not always needed)
Div_list <- desc_counts_Dividend[(desc_counts_Dividend$desc %in% Div), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Div_index = match(desc, Div_list),
    Div_index = ifelse(is.na(Div_index), Inf, Div_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Div_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Div_list & !any(desc == "Dividend") & row_number() == match(desc[desc %in% Div_list][1], desc),
      "Dividend",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Div_index)







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
desc_counts_CashFlowOperations$desc


#(Removal of) labels that are (not) of interest
CFO <- c("NetCashProvidedByUsedInOperatingActivities","NetCashProvidedByUsedInOperatingActivitiesContinuingOperations",
         "CashProvidedByUsedInOperatingActivitiesDiscontinuedOperations"        
)

#Create list of interesting variables (Not always needed)
CFO_list <- desc_counts_CashFlowOperations[(desc_counts_CashFlowOperations$desc %in% CFO), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    CFO_index = match(desc, CFO_list),
    CFO_index = ifelse(is.na(CFO_index), Inf, CFO_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, CFO_index) %>%
  mutate(
    desc = ifelse(
      desc %in% CFO_list & !any(desc == "CashFlowOperations") & row_number() == match(desc[desc %in% CFO_list][1], desc),
      "CashFlowOperations",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-CFO_index)



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

desc_counts_ResearchDevelopment$desc

#(Removal of) labels that are (not) of interest
Res <- c("ResearchAndDevelopmentExpense","ResearchAndDevelopmentExpenseExcludingAcquiredInProcessCost",
         "ResearchAndDevelopmentExpenseSoftwareExcludingAcquiredInProcessCost", "OtherResearchAndDevelopmentExpense"         
)

#Create list of interesting variables (Not always needed)
Res_list <- desc_counts_ResearchDevelopment[(desc_counts_ResearchDevelopment$desc %in% Res), ]$desc



#Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    Res_index = match(desc, Res_list),
    Res_index = ifelse(is.na(Res_index), Inf, Res_index) # move non-matching to the end
  ) %>%
  arrange(FY_symbol, Res_index) %>%
  mutate(
    desc = ifelse(
      desc %in% Res_list & !any(desc == "ResearchDevelopment") & row_number() == match(desc[desc %in% Res_list][1], desc),
      "ResearchDevelopment",
      desc
    )
  ) %>%
  ungroup() %>%
  select(-Res_index)


#Name = ResearchDevelopment





#----------------


#END OF VARIABLE INVESTIGATION




InterestedVariables <- c("Revenues","AccountsReceivable", "CurrentAssets","CurrentLiabilities","Inventory",
                         "Debt","Equity", "NetIncomeLoss", "Assets", "CostGoodsSold", "DepreciationAmortization",
                         "PropertyPlantAndEquipment","LongTermDebt","FixedAssets", "OperatingIncomeLoss","Interest", 
                         "PreTaxIncome", "Cash", "Dividend", "CashFlowOperations", "ResearchDevelopment")

InterestedVariables <- c("Revenues","AccountsReceivable", "CurrentAssets","CurrentLiabilities",
                         "Debt","Equity", "NetIncomeLoss", "Assets", "DepreciationAmortization",
                         "PropertyPlantAndEquipment","FixedAssets","Interest", 
                         "PreTaxIncome", "Cash", "CashFlowOperations")


length(InterestedVariables)

years <- c("2017","2018","2019","2020","2021","2022" )
Clean_results_df <- DF_results[DF_results$desc %in% InterestedVariables,]
Clean_results_df <- Clean_results_df[Clean_results_df$fy %in% years,]


#Long to wide format and calculate items that can be relied from the data 
CleanResultsWide <- pivot_wider(Clean_results_df, names_from = desc, values_from = val, values_fill = NA, id_cols = c("symbol", "fy", "FY_symbol"))
CleanResultsWide$debt <- CleanResultsWide$Assets- CleanResultsWide$Equity
CleanResultsWide$EBITDA <- CleanResultsWide$PreTaxIncome +CleanResultsWide$DepreciationAmortization
CleanResultsWide$FixedAssets <- ifelse(is.na(CleanResultsWide$FixedAssets),CleanResultsWide$Assets- CleanResultsWide$CurrentAssets, CleanResultsWide$FixedAssets )
CleanResultsWide$CurrentAssets <- ifelse(is.na(CleanResultsWide$CurrentAssets),CleanResultsWide$Assets- CleanResultsWide$FixedAssets, CleanResultsWide$CurrentAssets )



#Check duplicates
duplicates <- Clean_results_df %>%
dplyr::group_by(symbol, fy, FY_symbol, desc) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 
length(unique(duplicates$FY_symbol))#No duplicates



df_complete_cases <- na.omit(CleanResultsWide) #Only 328 rows

#Number of observations per year with no missing values
df_complete_cases %>%
  group_by(fy) %>%
  summarise(count = n()) 


#Number of observations per year with at least one observation
DF_results %>%
  group_by(FY_symbol) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(fy) %>%
  summarise(count = n()) 

# # Identify rows with 1 or 2 missing values
# rows_with_missing <- apply(CleanResultsWide[, 3:21], 1, function(x) sum(is.na(x)))
# rows_with_1_or_2_missing <- which(rows_with_missing >= 1 & rows_with_missing <= 2)
# 
# # Create new data frame with selected rows
# new_df <- CleanResultsWide[rows_with_1_or_2_missing, ]


na_counts <- colSums(is.na(CleanResultsWide))
na_counts


write.csv(CleanResultsWide, "CleanedResults.csv", row.names = FALSE)
