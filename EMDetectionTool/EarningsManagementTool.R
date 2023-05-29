library(shiny)
library(DT)
library(data.table)
library(httr)
library(purrr)
library(jsonlite)
library(stringr)
library(workflows)
library(dplyr)
library(data.table)
library(shinyWidgets)
library(dplyr)
library(XBRL)
library(tidyr)
library(ggplot2)
library(ranger)
library(xgboost)
library(recipes)
library(caret)
library(themis)
library(tidymodels)
library(parsnip)
library(DescTools)
library(scales)
library(robustHD)
library(kernlab)
library(shinydisconnect)
library(tidyverse)
originaldata <- read.csv("Filtered_Results.csv")

#Get CIK symbols per ticker and make a dataframe of it from SEC api
INFO <- read_json("https://www.sec.gov/files/company_tickers.json")
INFO <- rbindlist(INFO)

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


#Create function that retrieves data
getData <- function(ticker, year) {
  CIK <- getCIK(ticker)
  url <- paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", CIK, ".json")
  pg <- GET(url = url,
            config = httr::add_headers(`User-Agent` = "Sunny Bhatia 590913ab@student.eur.nl",
                                       `Accept-Encoding` = 'gzip, deflate'))
  data_raw <- try(content(pg, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE), silent = TRUE)
  N <- length(data_raw$facts$dei)
  if (N >= 1) {
    DEI <- rbindlist(lapply(as.list(1:N), function(ii) {
      # extract data
      tmp <- as.data.frame(data_raw$facts$dei[[ii]]$units[[1]])
      # add description column
      tmp$desc <- names(data_raw$facts$dei)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end), ]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      filter(tmp, form == "10-K", fy == year, substr(end, 1, 4) == year)
    }), use.names = TRUE, fill = TRUE)
    DEI = DEI[,c("end","val","accn","fy","fp","form","filed","frame","desc","symbol" )]
    
  } else {
    DEI <- NULL
  }
  
  N = length(data_raw$facts$`us-gaap`)
  if(N >= 1)
  {
    GAAP = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp <- as.data.frame(data_raw$facts$`us-gaap`[[ii]]$units[[1]])
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
    GAAP = GAAP[,c("end","val","accn","fy","fp","form","filed","frame","desc","symbol" )]
  }else{
    GAAP = NULL
  }
  N = length(data_raw$facts$invest)
  if(N >= 1)
  {
    INVEST = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(data_raw$facts$invest[[ii]]$units[[1]])
      # add description column
      tmp$desc <- names(data_raw$facts$invest)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      filter(tmp, form=="10-K",fy==year, substr(end,1,4)==year)
    }),use.names=TRUE, fill=TRUE)
    # re-order
    INVEST = INVEST[,c("end","val","accn","fy","fp","form","filed","frame","desc","symbol" )]
  }else{
    INVEST = NULL
  }
  N = length(data_raw$facts$srt)
  if(N >= 1)
  {
    SRT = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(data_raw$facts$srt[[ii]]$units[[1]])
      # add description column
      tmp$desc <- names(data_raw$facts$srt)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      filter(tmp, form=="10-K",fy==year, substr(end,1,4)==year)
    }),use.names=TRUE, fill=TRUE)
    # re-order
    SRT = SRT[,c("end","val","accn","fy","fp","form","filed","frame","desc","symbol" )]
  }else{
    SRT = NULL
  }
  ALL <- bind_rows(GAAP,DEI,SRT,INVEST)
}

#Clean the data
cleanData <- function(data) {
  #Pre-processing of data
  data$start <- as.Date(data$start)
  data$end <- as.Date(data$end)
  data <- subset(data, select = c("symbol", "fy","desc","val" ))
  data$FY_symbol <- paste(data$fy, data$symbol, sep=" ")
  # Perform data cleaning steps for variables
  # Revenue
  revenue <- grepl("revenue", data$desc, ignore.case = TRUE)
  revenue <- data[revenue, ]
  
  desc_counts_rev <- revenue %>%
    group_by(desc) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  not_rev <- c("CostOfRevenue", "InterestRevenueExpenseNet")
  
  rev_list <- desc_counts_rev[!(desc_counts_rev$desc %in% not_rev), ]$desc
  
  data <- data %>%
    group_by(FY_symbol) %>%
    mutate(
      rev_index = match(desc, rev_list),
      rev_index = ifelse(is.na(rev_index), Inf, rev_index)
    ) %>%
    arrange(FY_symbol, rev_index) %>%
    mutate(
      desc = ifelse(
        desc %in% rev_list & !any(desc == "Revenues") & row_number() == match(desc[desc %in% rev_list][1], desc),
        "Revenues",
        desc
      )
    ) %>%
    ungroup() %>%
    select(-rev_index)
  
  # Receivable
  receivable <- grepl("Receivable", data$desc, ignore.case = TRUE)
  receivable <- data[receivable, ]
  
  desc_counts_receiv <- receivable %>%
    group_by(desc) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  receiv <- c("AccountsReceivableNetCurrent", "ReceivablesNetCurrent", "AccountsReceivableGrossCurrent", "AccountsNotesAndLoansReceivableNetCurrent",
              "AccountsReceivableNet", "AccountsAndOtherReceivablesNetCurrent", "AccountsReceivableNetNoncurrent", "AccountsReceivableRelatedPartiesCurrent",
              "AccountsReceivableGross", "AccountsAndNotesReceivableNet", "ReceivablesFromCustomers", "AccountsReceivableSale",
              "AccountsReceivableGrossNoncurrent")
  
  receiv_list <- desc_counts_receiv[desc_counts_receiv$desc %in% receiv, ]$desc
  
  data <- data %>%
    group_by(FY_symbol) %>%
    mutate(
      receiv_index = match(desc, receiv_list),
      receiv_index = ifelse(is.na(receiv_index), Inf, receiv_index)
    ) %>%
    arrange(FY_symbol, receiv_index) %>%
    mutate(
      desc = ifelse(
        desc %in% receiv_list & !any(desc == "AccountsReceivable") & row_number() == match(desc[desc %in% receiv_list][1], desc),
        "AccountsReceivable",
        desc
      )
    ) %>%
    ungroup() %>%
    select(-receiv_index)
  #-----------------------------------------------------------------
  #CurrentAssets
  
  CurrentAssets <- grepl("Current", data$desc, ignore.case = TRUE)
  CurrentAssets <- data[CurrentAssets,]
  
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
  data <- data %>%
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
  
  liabilities <- grepl("Current", data$desc, ignore.case = TRUE)
  liabilities <- data[liabilities,]
  
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
  data <- data %>%
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
  
  inventory <- grepl("inventory", data$desc, ignore.case = TRUE)
  inventory <- data[inventory,]
  
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
  data <- data %>%
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
  # Debt <- grepl("Debt", data$desc, ignore.case = TRUE)
  # Debt <- data[Debt,]
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
  # data <- data %>%
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
  
  equity <- grepl("equity", data$desc, ignore.case = TRUE)
  equity <- data[equity,]
  
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
  data <- data %>%
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
  
  income <- grepl("income", data$desc, ignore.case = TRUE)
  income <- data[income,]
  
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
  data <- data %>%
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
  
  Assets <- grepl("Assets", data$desc, ignore.case = TRUE)
  Assets <- data[Assets,]
  
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
  data <- data %>%
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
  
  Cost <- grepl("Cost", data$desc, ignore.case = TRUE)
  Cost <- data[Cost,]
  
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
  data <- data %>%
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
  
  Depreciation <- grepl("Depreciation", data$desc, ignore.case = TRUE)
  Depreciation <- data[Depreciation,]
  
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
  data <- data %>%
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
  
  plant <- grepl("plant", data$desc, ignore.case = TRUE)
  plant <- data[plant,]
  
  desc_counts_Plant <- plant %>%
    group_by(desc) %>%
    summarise(count = n())%>%
    arrange(desc(count))
  head(desc_counts_Plant,10) #first two names seem to indicate revenue
  
  desc_counts_Plant$desc
  
  #(Removal of) labels that are (not) of interest
  Pla <- c("PropertyPlantAndEquipmentNet","PropertyPlantAndEquipmentGross", "PropertyPlantAndEquipmentAdditions",
           "PropertyPlantAndEquipmentOther", "PropertyPlantAndEquipmentDisposals","PropertyPlantAndEquipmentAndFinanceLeaseRightOfUseAssetAfterAccumulatedDepreciationAndAmortization",
           "PropertyPlantAndEquipmentAndFinanceLeaseRightOfUseAssetAfterAccumulatedDepreciationAndAmortization" 
  )
  
  #Create list of interesting variables (Not always needed)
  Plant_list <- desc_counts_Plant[(desc_counts_Plant$desc %in% Pla), ]$desc
  
  
  
  
  #Change per firm per year the variable to the correct name in original dataframe, based on occurence in Rev_list
  data <- data %>%
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
  
  
  # data <- data %>%
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
  
  Long_term_debt <- grepl("long", data$desc, ignore.case = TRUE)
  Long_term_debt <- data[Long_term_debt,]
  
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
  data <- data %>%
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
  
  FixedAssets <- grepl("assets", data$desc, ignore.case = TRUE)
  FixedAssets <- data[FixedAssets,]
  
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
  data <- data %>%
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
  
  OperatingIncome <- grepl("operating", data$desc, ignore.case = TRUE)
  OperatingIncome <- data[OperatingIncome,]
  
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
  data <- data %>%
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
  
  Interest <- grepl("Interest", data$desc, ignore.case = TRUE)
  Interest <- data[Interest,]
  
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
  data <- data %>%
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
  
  PreTaxIncome <- grepl("income", data$desc, ignore.case = TRUE)
  PreTaxIncome <- data[PreTaxIncome,]
  
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
  data <- data %>%
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
  
  Cash <- grepl("cash", data$desc, ignore.case = TRUE)
  Cash <- data[Cash,]
  
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
  data <- data %>%
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
  
  Dividend <- grepl("dividend", data$desc, ignore.case = TRUE)
  Dividend <- data[Dividend,]
  
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
  data <- data %>%
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
  
  Cash_flow <- grepl("cash", data$desc, ignore.case = TRUE)
  Cash_flow <- data[Cash_flow,]
  
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
  data <- data %>%
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
  
  Research <- grepl("Research", data$desc, ignore.case = TRUE)
  Research <- data[Research,]
  
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
  data <- data %>%
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
  InterestedVariables <- c("Revenues","AccountsReceivable", "CurrentAssets","CurrentLiabilities",
                           "Debt","Equity", "NetIncomeLoss", "Assets", "DepreciationAmortization",
                           "PropertyPlantAndEquipment","FixedAssets","Interest",
                           "PreTaxIncome", "Cash", "CashFlowOperations")
  data <- data[data$desc %in% InterestedVariables,]
  data <- pivot_wider(data, names_from = desc, values_from = val, values_fill = NA, id_cols = c("symbol", "fy", "FY_symbol"))
  data$debt <- data$Assets- data$Equity
  data$EBITDA <- data$PreTaxIncome +data$DepreciationAmortization
  if (!exists("FixedAssets", where = data) || is.na(data$FixedAssets)) {
    data$FixedAssets <- data$Assets - data$CurrentAssets
  }
  if (!exists("CurrentAssets", where = data) || is.na(data$CurrentAssets)) {
    data$CurrentAssets <- data$Assets - data$CurrentAssets
  }
  
  # data$CurrentAssets <- ifelse(is.na(data$CurrentAssets),data$Assets- data$FixedAssets, data$CurrentAssets )
  # #Return the cleaned data
  return(data)
}

#Calculate Features
FeatureCalculation <- function(data, ticker, year) {
  data <-data %>% arrange(fy)
  data$AccountsReceivableTurnover <- data$Revenues/data$AccountsReceivable
  data$CurrentRatio <- data$CurrentAssets/data$CurrentLiabilities
  data$DebtToEquity <- data$debt/data$Equity
  data$ROA <- data$NetIncomeLoss/data$Assets
  data$ROE <- data$NetIncomeLoss/data$Equity
  data$DaysSalesinAccountingReceivable <- 360 / (data$Revenues/data$AccountsReceivable)
  data$DepreciationOverPlant <- data$DepreciationAmortization/data$PropertyPlantAndEquipment
  data$EquityOverFixedAssets <- data$Equity/data$FixedAssets
  data$TimesInterestEarned <- data$EBITDA/data$Interest
  data$SalesToTotalAssets <- data$Revenues/data$Assets
  data$PreTaxIncomeToSales <- data$PreTaxIncome/data$Revenues
  data$NetIncomeLossToSales <- data$NetIncomeLoss/data$Revenues
  data$SalestoCash <- data$Revenues/data$Cash
  data$SalestoWorkingCapital <- data$Revenues/(data$CurrentAssets-data$CurrentLiabilities)
  data$SalesToFixedAssets <- data$Revenues/data$FixedAssets
  data$WorkingCaitalToAssets <- (data$CurrentAssets-data$CurrentLiabilities)/data$Assets
  data$EBITDAMarginRatio <- data$EBITDA/data$Revenues
  data$CashFlowOperationsToDebt <- data$CashFlowOperations/data$debt
  data$NetIncomeLossToCashflow <- data$NetIncomeLoss/data$CashFlowOperations
  data <- data %>%
    mutate(
      ChangeInDepreciation = DepreciationAmortization - lag(DepreciationAmortization, default = first(DepreciationAmortization)),
      ChangeInAssets = Assets - lag(Assets, default = first(Assets)),
      ChangeInRevenues = Revenues - lag(Revenues, default = first(Revenues)),
      ChangeInCurrentRatio = CurrentRatio - lag(CurrentRatio, default = first(CurrentRatio)),
      ChangeInDebtToEquity = DebtToEquity - lag(DebtToEquity, default = first(DebtToEquity)),
      ChangeInWorkingCapital = (CurrentAssets-CurrentLiabilities) - lag((CurrentAssets-CurrentLiabilities), default = first((CurrentAssets-CurrentLiabilities))),
      ChangeInDaysSalesinAccountingReceivable = DaysSalesinAccountingReceivable - lag(DaysSalesinAccountingReceivable, default = first(DaysSalesinAccountingReceivable)),
      ChangeInDepreciationOverPlant = DepreciationOverPlant - lag(DepreciationOverPlant, default = first(DepreciationOverPlant)),
      ChangeInEquityOverFixedAssets = EquityOverFixedAssets - lag(EquityOverFixedAssets, default = first(EquityOverFixedAssets)),
      ChangeInTimesInterestEarned = TimesInterestEarned - lag(TimesInterestEarned, default = first(TimesInterestEarned)),
      ChangeInSalesToTotalAssets = SalesToTotalAssets - lag(SalesToTotalAssets, default = first(SalesToTotalAssets)),
      ChangeInPreTaxIncomeToSales = PreTaxIncomeToSales - lag(PreTaxIncomeToSales, default = first(PreTaxIncomeToSales)),
      ChangeInNetIncomeLossToSales = NetIncomeLossToSales - lag(NetIncomeLossToSales, default = first(NetIncomeLossToSales)),
      ChangeInSalestoWorkingCapital = SalestoWorkingCapital - lag(SalestoWorkingCapital, default = first(SalestoWorkingCapital)),
      ChangeInWorkingCaitalToAssets = WorkingCaitalToAssets - lag(WorkingCaitalToAssets, default = first(WorkingCaitalToAssets)),
      ChangeInEBITDAMarginRatio = EBITDAMarginRatio - lag(EBITDAMarginRatio, default = first(EBITDAMarginRatio)),
      ChangeInDebt = debt - lag(debt, default = first(debt))
    )
  data[, 4:55][is.na(data[, 4:55])] <- 0
  data[] <- lapply(data, function(x) ifelse(is.infinite(x), 0, x))
  
  Results <- as.data.frame(read.csv("PreWinsorized.csv"))
  common_cols <- intersect(names(data), names(Results))
  Results <- Results[, common_cols]
  CombinedData <- rbind(Results, data)
  CombinedData <- CombinedData[!duplicated(CombinedData$FY_symbol), ]
  for (i in 20:55) {
    CombinedData[, i] <- winsorize(CombinedData[, i], probs = c(0.0001, 0.9999))
  }
  CombinedData[,20:55] <- scale(CombinedData[,20:55])
  #FY_symbol <- data$FY_symbol
  #CombinedData <- CombinedData[CombinedData$FY_symbol == data$FY_symbol, ]
  #CombinedData <- as.data.frame(CombinedData)
  filtered_data <- CombinedData[CombinedData$symbol == ticker & CombinedData$fy == year, ]
  
  
  
  # 
  # 
  # 
  
  return(filtered_data)
  #return(CombinedData)
}


#Random forest prediction
RandomForestPrediction <- function(data, originaldata) {
  originaldata <- read.csv("Filtered_Results.csv")
  originaldata$DiscretionaryAccrualsBinary <- as.factor(originaldata$DiscretionaryAccrualsBinary)
  originaldata$DiscretionaryAccrualsBinary <- factor(originaldata$DiscretionaryAccrualsBinary, levels = c("1", "0"))
  common_cols <- intersect(names(data), names(originaldata))
  data <- data[, common_cols]
  originaldata <- subset(originaldata, !(FY_symbol %in% data$FY_symbol))
  # data$DiscretionaryAccrualsBinary <- "REMOVE"
  # CombinedData <- rbind(originaldata, data)
  # duplicates <- duplicated(CombinedData$FY_symbol) | duplicated(CombinedData$FY_symbol, fromLast = TRUE)
  # originaldata <- CombinedData[!duplicates, ]
  #originaldata$DiscretionaryAccrualsBinary <- NULL
  
  #Preprocess data using recipe
  rf_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = originaldata) %>%
    update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
    step_downsample(DiscretionaryAccrualsBinary)
  
  rf_model<- rand_forest(mtry = 7, trees = 200) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  rf_wf <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model)
  
  rf_wf_fit <- fit(rf_wf, data = originaldata)
  predictions <- predict(rf_wf_fit, new_data = data)
  labels <- ifelse(predictions == 0, "Moderate Proxy for Earnings Management", "Extreme Proxy for Earnings Management")
  
  return(labels)
}

#Gradient boosting prediction
GradientBoostingPrediction <- function(data, originaldata) {
  originaldata <- read.csv("Filtered_Results.csv")
  originaldata$DiscretionaryAccrualsBinary <- as.factor(originaldata$DiscretionaryAccrualsBinary)
  originaldata$DiscretionaryAccrualsBinary <- factor(originaldata$DiscretionaryAccrualsBinary, levels = c("1", "0"))
  common_cols <- intersect(names(data), names(originaldata))
  data <- data[, common_cols]
  originaldata <- subset(originaldata, !(FY_symbol %in% data$FY_symbol))
  # data$DiscretionaryAccrualsBinary <- "REMOVE"
  # CombinedData <- rbind(originaldata, data)
  # duplicates <- duplicated(CombinedData$FY_symbol) | duplicated(CombinedData$FY_symbol, fromLast = TRUE)
  # originaldata <- CombinedData[!duplicates, ]
  #originaldata$DiscretionaryAccrualsBinary <- NULL
  
  #Preprocess data using recipe
  XGB_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = originaldata) %>%
    update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
    step_downsample(DiscretionaryAccrualsBinary)
  
  XGB_model<- boost_tree(trees = 1500, tree_depth = 3, learn_rate = 0.01) %>%
    set_mode("classification") %>%
    set_engine("xgboost")
  XGB_wf <- workflow() %>%
    add_recipe(XGB_recipe) %>%
    add_model(XGB_model)
  
  XGB_wf_fit <- fit(XGB_wf, data = originaldata)
  predictions <- predict(XGB_wf_fit, new_data = data)
  labels <- ifelse(predictions == 0, "Moderate Proxy for Earnings Management", "Extreme Proxy for Earnings Management")
  return(labels)
}




#Support Vector Machine prediction
SupportVectorPrediction <- function(data, originaldata) {
  originaldata <- read.csv("Filtered_Results.csv")
  originaldata$DiscretionaryAccrualsBinary <- as.factor(originaldata$DiscretionaryAccrualsBinary)
  originaldata$DiscretionaryAccrualsBinary <- factor(originaldata$DiscretionaryAccrualsBinary, levels = c("1", "0"))
  common_cols <- intersect(names(data), names(originaldata))
  data <- data[, common_cols]
  originaldata <- subset(originaldata, !(FY_symbol %in% data$FY_symbol))
  # data$DiscretionaryAccrualsBinary <- "REMOVE"
  # CombinedData <- rbind(originaldata, data)
  # duplicates <- duplicated(CombinedData$FY_symbol) | duplicated(CombinedData$FY_symbol, fromLast = TRUE)
  # originaldata <- CombinedData[!duplicates, ]
  #originaldata$DiscretionaryAccrualsBinary <- NULL
  
  #Preprocess data using recipe
  SVM_recipe <- recipe(DiscretionaryAccrualsBinary ~ ., data = originaldata) %>%
    update_role(symbol, fy, FY_symbol, new_role = "ignored") %>%
    step_downsample(DiscretionaryAccrualsBinary)
  
  SVM_model<- svm_rbf(cost = 10, rbf_sigma = 0.01) %>%
    set_mode("classification") 
  SVM_wf <- workflow() %>%
    add_recipe(SVM_recipe) %>%
    add_model(SVM_model)
  
  SVM_wf_fit <- fit(SVM_wf, data = originaldata)
  predictions <- predict(SVM_wf_fit, new_data = data)
  labels <- ifelse(predictions == 0, "Moderate Proxy for Earnings Management", "Extreme Proxy for Earnings Management")
  
  return(labels)
}


# Aapl <- getData("AAPL", 2021)
# Aaplclean <- cleanData(Aapl)
# Applefeature <- FeatureCalculation(Aaplclean,"AAPL", 2021)
# test <- RandomForestPrediction(Applefeature,originaldata )
# test1 <- GradientBoostingPrediction(Applefeature,originaldata )
# test2 <- SupportVectorPrediction(Applefeature,originaldata )


# A <- getData("A", 2020)
# CleanA <- cleanData(A)
# FeatureA <- FeatureCalculation(CleanA, "A", 2020)
# APrediction <-RandomForestPrediction(FeatureA,originaldata)
# APredictionBoosting <-GradientBoostingPrediction(FeatureA,originaldata)
#PredictionSVM <-SupportVectorPrediction(FeatureA,originaldata)



ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        h1 {
          font-size: 12px; /* Adjust the font size as needed */
        }
        h6 {
          font-size: 14px; /* Adjust the font size as needed */
        }
        .footer {
          font-size: 10px; /* Adjust the font size as needed */
          text-align: center;
          padding: 10px;
          background-color: #f5f5f5;
          border-top: 1px solid #ddd;
        }
      ")
    )
  ),
  titlePanel("Earnings Management Detection"),
  div(
    h1(paste("This tool is part of the thesis of Apoorv Sunny Bhatia for the
       MSc in Business Analytics & Management. The thesis can be downloaded
       here:"),a("Thesis", href = "https://github.com/sunny1999123/MasterThesis/blob/main/Thesis.pdf")),
  div(h6(paste("The goal of this tool is to enable investors and other financial statement stakeholders to apply an
         Earnings Management detection tool on a firm of their interest. The tool will extract the financial statement information
         using eXtentible Business Reporting Language (XBRL). Next, it will clean the data and provide three
         predictions based on three machine learning models. Finally, the tool will provide an overall prediction. For more information
         on the mechanics of the tool, please click on the aforementioned link to download the thesis. Please note that this tool only works for firm that publish their financial statements in the SEC's
         EDGAR system. The ticker symbol corresponding to a firm of interest can be found on the SEC's company lookup page:")
               ,a("Click Here", href = "https://www.sec.gov/edgar/searchedgar/companysearch"))),
  ),  
  disconnectMessage(text = "An error occurred. The tool was unable to extract (all) the data. 
                    Please try a different input."),
  sidebarLayout(
    sidebarPanel(
      selectInput("ticker", "Please Select Ticker Symbol :",
                     choices = INFO$ticker
                    ),
      selectInput("year", "Please Select Year:", choices = as.character(2018:(as.numeric(format(Sys.Date(), "%Y")) - 1))),
      actionButton("getData", "OK")
    ),
    mainPanel(
      progress = "progress",
      verbatimTextOutput("message"),
      textOutput("financial_info_text"),
      DT::dataTableOutput("cleanedData"),
      textOutput("result_rf") ,
      textOutput("result_xgb"),
      textOutput("result_SVM"),
      br(),
      htmlOutput("overall_prediction"),
      textOutput("keepAlive")
      
    )
  ),
  div(
    class = "footer",
    HTML(paste(
      "Developed by Apoorv Sunny Bhatia as part of the MSc in Business Analytics & Management thesis.",
      "<br>",
      "For any questions/remarks, please reach out to the developer via:",
      '<a href="mailto:sunny1999@live.nl">sunny1999@live.nl</a>'
    )),
    div(
      p("Disclaimer: This tool is provided for informational purposes only and should not be considered as financial advice. 
        The predictions and results generated by the tool are based on machine learning models used in the thesis
        and are not guaranteed to be accurate or reliable. No rights can be derived from the information
        provided by this tool."),
      class = "disclaimer"
    )
  )
)





server <- function(input, output) {
  cleanedData <- reactiveVal(NULL)  
  originaldata <- reactiveVal(NULL)  
  
  observeEvent(input$getData, {
    ticker <- input$ticker
    year <- as.numeric(input$year)
    
    withProgress(message = 'Progress...', value = 0, {
      setProgress(0.3)
      Sys.sleep(2)
      
      tryCatch({
        data <- getData(ticker, year)
        data_previous_year <- getData(ticker, year - 1)
        
        if (!is.null(data) && !is.null(data_previous_year)) {
          message <- "Data is successfully retrieved"
          
          setProgress(0.6)
          cleanedData_year <- cleanData(data)
          cleanedData_previous_year <- cleanData(data_previous_year)
          
          setProgress(0.9)
          cleanedData_combined <- rbind(cleanedData_year, cleanedData_previous_year)
          
          if (anyNA(cleanedData_combined)) {
            message <- "Data is not successfully cleaned. Missing values exist."
            showNotification(message, type = "error")
            cleanedData(NULL)  # Set cleanedData to NULL
          } else {
            message <- paste(message, "\nData is successfully cleaned.")
            
            cleanedData_calculated <- FeatureCalculation(cleanedData_combined, input$ticker, year)
            
            if (is.null(cleanedData_calculated)) {
              message <- "Problem with feature calculation."
              showNotification(message, type = "error")
              cleanedData(NULL)  # Set cleanedData to NULL
            } else {
              cleanedData(cleanedData_calculated)
              message <- paste(message, "\nFeatures are successfully calculated.")
              
              # Set the original data
              originaldata(data)
            }
          }
        } else {
          message <- "Problem with data retrieval."
          showNotification(message, type = "error")
          cleanedData(NULL)  # Set cleanedData to NULL
        }
      }, error = function(e) {
        message <- "An error occurred. The tool was unable to extract (all) the data. Please try a different input."
        showNotification(message, type = "error")
        cleanedData(NULL)  # Set cleanedData to NULL
      })
      
      output$message <- renderText({
        message
      })
      
      output$cleanedData <- DT::renderDataTable({
        if (!is.null(cleanedData())) {
          transposed_data <- cleanedData() %>%
            dplyr::select(-3) %>%
            rename(Year = fy) %>%
            mutate_at(vars(3:ncol(.)), ~sprintf("%.2f", .)) %>%
            rename(Ticker = symbol) %>%
            tidyr::gather("Name", "Value") 
          
          datatable(
            transposed_data,
            options = list(pageLength = 5),
            rownames = FALSE
          )
        }
      })
      company_name <- INFO$title[INFO$ticker == input$ticker]
      output$financial_info_text <- renderText({
        paste("Financial information of", company_name, ":")
      })
      
      
      if (!is.null(cleanedData())) {
        # Random Forest prediction
        predictions_rf <- RandomForestPrediction(cleanedData(), originaldata())
        
        output$prediction_rf <- renderText({
          predictions_rf
        })
        output$result_rf <- renderText({
          paste("Random Forest Prediction:", predictions_rf)
        })
      }
      if (!is.null(cleanedData())) {
        # Boosting prediction
        predictions_xgb <- GradientBoostingPrediction(cleanedData(), originaldata())
        
        output$predictions_xgb <- renderText({
          predictions_xgb
        })
        output$result_xgb <- renderText({
          paste("Gradient Boosting Prediction:", predictions_xgb)
        })
      }
      if (!is.null(cleanedData())) {
        # Random Forest prediction
        predictions_SVM <- SupportVectorPrediction(cleanedData(), originaldata())
        
        output$predictions_SVM <- renderText({
          predictions_SVM
        })
        output$result_SVM <- renderText({
          paste("Support Vector Machine Prediction:", predictions_SVM)
        })
      }
      moderate_count <- sum(grepl("Moderate", predictions_rf)) +
        sum(grepl("Moderate", predictions_xgb)) +
        sum(grepl("Moderate", predictions_SVM))
      
      extreme_count <- sum(grepl("Extreme", predictions_rf)) +
        sum(grepl("Extreme", predictions_xgb)) +
        sum(grepl("Extreme", predictions_SVM))
      
      
      overall_prediction <- ifelse(moderate_count >= 2, "Moderately", "Extremely")
      
      output$overall_prediction <- renderText({
        if (is.na(company_name)) {
          prediction <-  paste(
            "Based on the Machine Learning models, the overall prediction is that the company
            had a", overall_prediction,
            "Upwards/Downwards Proxy for Earnings Management.",
            "This means that the firm likely", ifelse(overall_prediction == "Moderately", "did not engage", "engaged"),
            "in Earnings Management, based on the Machine Learning models."
          )
        } else {
          prediction <- paste("Based on the aforementioned predictions, 
                              the overall predicition for", company_name, "in the year", input$year, 
                              "is that it", ifelse(overall_prediction == "Moderately", "did not engage", "engaged"),"in Earnings Management"
          )
        }
        
        HTML(paste("<span style='font-weight: bold;'>", prediction, "</span>"))
      })
      
    })
  })
}


# "Based on the Machine Learning models, the overall prediction is that",
# company_name,
# "in the year", input$year, "had a", overall_prediction,
# "Upwards/Downwards Proxy for Earnings Management.",
# "This means that the firm likely", ifelse(overall_prediction == "Moderately", "did not engage", "engaged"),
# "in Earnings Management, based on the Machine Learning models."

shinyApp(ui, server)





