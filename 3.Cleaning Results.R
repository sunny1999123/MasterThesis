
#Load libraries
library(dplyr)
library(XBRL)
library(tidyr)

#Read results of financial info
Results <- read.csv("result_df.csv")
Frames <- unique(Results$frame)
FramesUsed <- c(NA, "CY2016", "CY2017", "CY2018", "CY2019", "CY2020", "CY2021")
Results <-Results[Results$frame %in% FramesUsed,]
Results$start <- as.Date(Results$start)
Results$end <- as.Date(Results$end)


df_filtered <- Results %>%
  arrange(symbol, fy, end) %>%  # sort by symbol, fy, and end
  group_by(symbol, fy, desc) %>%  # group by symbol and fy
  slice_tail(n = 1) %>%  # keep only the last row within each group
  ungroup()  # remove grouping


DF_results <- subset(df_filtered, select = c("val", "fy","desc", "symbol"))

#Only get the latest row per firm, since some financial statement information is stored twice, but the first number are quarterly numbers

#Clean results 
#First only keep the interesting columns, and then change from long to wide format
#Some variables are stored with different names, every variable needs to be evaluated. 

#Check the sum of the number of unique firms per year , which should equal the number of firms per item
DF_results$FY_symbol <- paste(DF_results$fy, DF_results$symbol, sep=" ")
NumberFirms <- unique(DF_results$FY_symbol)
length(NumberFirms) #answer is 2556
#So each item used should occur 2556 times 


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
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "RevenueFromContractWithCustomerExcludingAssessedTax" & 
        !any(desc == "Revenues"), 
      "Revenues", 
      desc
    )
  ) %>%
  ungroup()

#Modify 
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "RevenueFromContractWithCustomerIncludingAssessedTax" & 
        !any(desc == "Revenues"), 
      "Revenues", 
      desc
    )
  ) %>%
  ungroup()

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "ContractWithCustomerLiabilityRevenueRecognized" & 
        !any(desc == "Revenues"), 
      "Revenues", 
      desc
    )
  ) %>%
  ungroup()




#name= "Revenues"

#-----------------------------------------------------------------
#Receivable

Receivable <- grepl("Receivable", DF_results$desc, ignore.case = TRUE)
Receivable <- DF_results[Receivable,]

desc_counts_receiv <- Receivable %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_receiv,30) #first two names seem to indicate revenue


DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "ReceivablesNetCurrent"  &
        !any(desc == "AccountsReceivable"),
      "AccountsReceivable",
      desc
    )
  ) %>%
  ungroup()

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "AccountsReceivableNetCurrent"  &
        !any(desc == "AccountsReceivable"),
      "AccountsReceivable",
      desc
    )
  ) %>%
  ungroup()



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


DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "AssetsCurrent" &
        !any(desc == "CurrentAssets"),
      "CurrentAssets",
      desc
    )
  ) %>%
  ungroup()


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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "LiabilitiesCurrent" & 
        !any(desc == "CurrentLiabilities"), 
      "CurrentLiabilities", 
      desc
    )
  ) %>%
  ungroup()


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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "InventoryNet" & 
        !any(desc == "Inventory"), 
      "Inventory", 
      desc
    )
  ) %>%
  ungroup()




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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "LongTermDebt" & 
        !any(desc == "Debt"), 
      "Debt", 
      desc
    )
  ) %>%
  ungroup()


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


DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "StockholdersEquity" & 
        !any(desc == "Equity"), 
      "Equity", 
      desc
    )
  ) %>%
  ungroup()



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

# DF_results <- DF_results %>%
#   group_by(FY_symbol) %>%
#   mutate(
#     desc = ifelse(
#       desc == "NetIncomeLoss" & 
#         !any(desc == "NetIncomeLoss"), 
#       "NetIncomeLoss", 
#       desc
#     )
#   ) %>%
#   ungroup()


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

# DF_results <- DF_results %>%
#   group_by(FY_symbol) %>%
#   mutate(
#     desc = ifelse(
#       desc == "Assets" & 
#         !any(desc == "Assets"), 
#       "Assets", 
#       desc
#     )
#   ) %>%
#   ungroup()



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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "CostOfGoodsAndServicesSold" &
        !any(desc == "CostGoodsSold"),
      "CostGoodsSold",
      desc
    )
  ) %>%
  ungroup()




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

#Modify 
DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "DepreciationDepletionAndAmortization" &
        !any(desc == "DepreciationAmortization"),
      "DepreciationAmortization",
      desc
    )
  ) %>%
  ungroup()

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "DepreciationAndAmortization" &
        !any(desc == "DepreciationAmortization"),
      "DepreciationAmortization",
      desc
    )
  ) %>%
  ungroup()



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


DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "LongTermDebtNoncurrent" &
        !any(desc == "LongTermDebt"),
      "LongTermDebt",
      desc
    )
  ) %>%
  ungroup()




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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "OtherAssetsNoncurrent" &
        !any(desc == "FixedAssets"),
      "FixedAssets",
      desc
    )
  ) %>%
  ungroup()



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

# DF_results <- DF_results %>%
#   group_by(FY_symbol) %>%
#   mutate(
#     desc = ifelse(
#       desc == "OperatingIncomeLoss" &
#         !any(desc == "OperatingIncomeLoss"),
#       "OperatingIncomeLoss",
#       desc
#     )
#   ) %>%
#   ungroup()

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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "InterestExpense" &
        !any(desc == "Interest"),
      "Interest",
      desc
    )
  ) %>%
  ungroup()


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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest" &
        !any(desc == "PreTaxIncome"),
      "PreTaxIncome",
      desc
    )
  ) %>%
  ungroup()


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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "CashAndCashEquivalentsAtCarryingValue" &
        !any(desc == "Cash"),
      "Cash",
      desc
    )
  ) %>%
  ungroup()


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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "PaymentsOfDividendsCommonStock" &
        !any(desc == "Dividend"),
      "Dividend",
      desc
    )
  ) %>%
  ungroup()




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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations" &
        !any(desc == "CashFlowOperations"),
      "CashFlowOperations",
      desc
    )
  ) %>%
  ungroup()



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

DF_results <- DF_results %>%
  group_by(FY_symbol) %>%
  mutate(
    desc = ifelse(
      desc == "ResearchAndDevelopmentExpense" &
        !any(desc == "ResearchDevelopment"),
      "ResearchDevelopment",
      desc
    )
  ) %>%
  ungroup()



#Name = ResearchDevelopment


#----------------
#End of variable investigation



InterestedVariables <- c("Revenues","AccountsReceivable", "CurrentAssets","CurrentLiabilities","Inventory",
                         "Debt","Equity", "NetIncomeLoss", "Assets", "CostGoodsSold", "DepreciationAmortization",
                         "PropertyPlantAndEquipment","LongTermDebt","FixedAssets", "OperatingIncomeLoss","Interest", 
                         "PreTaxIncome", "Cash", "Dividend", "CashFlowOperations", "ResearchDevelopment")


Clean_results_df <- DF_results[DF_results$desc %in% InterestedVariables,]

#Reorder Table
Clean_results_df <- select(Clean_results_df, symbol, fy, FY_symbol, desc, val)

#Long to wide format
CleanResultsWide <- pivot_wider(Clean_results_df, names_from = desc, values_from = val, values_fill = NA, id_cols = c("symbol", "fy", "FY_symbol"))

#Check duplicates
duplicates <- Clean_results_df %>%
dplyr::group_by(symbol, fy, FY_symbol, desc) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 


length(unique(duplicates$FY_symbol))#No duplicates

#Checks  
# COO_2021 <- filter(Results, fy=="2021"&symbol=="COO")
# COO_2021_1 <- filter(DF_results, fy=="2021"&symbol=="COO")
# 
# ECL_2021 <- filter(Results, fy=="2021"&symbol=="ECL")
# ECL_2021_1 <- filter(DF_results, fy=="2021"&symbol=="ECL")

df_complete_cases <- na.omit(CleanResultsWide) #Only two rows


na_counts <- colSums(is.na(CleanResultsWide))
na_counts


#Revenues

subset_df_rev <- CleanResultsWide[is.na(CleanResultsWide$Revenues), ]

