
#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#Load dataframe
Results <- read.csv("CleanedResults.csv")
InterestedVariables <- c("Revenues","AccountsReceivable", "CurrentAssets","CurrentLiabilities",
                         "Debt","Equity", "NetIncomeLoss", "Assets", "DepreciationAmortization",
                         "PropertyPlantAndEquipment","FixedAssets","Interest", 
                         "PreTaxIncome", "Cash", "CashFlowOperations")
