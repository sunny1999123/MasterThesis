
#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#Load dataframe
Results <- read.csv("CleanedResults.csv")

Results <-Results %>% arrange(symbol, fy)

#Variable calculation
Results$AccountsReceivableTurnover <- Results$Revenues/Results$AccountsReceivable
Results$CurrentRatio <- Results$CurrentAssets/Results$CurrentLiabilities
Results$DebtToEquity <- Results$debt/Results$Equity
Results$ROA <- Results$NetIncomeLoss/Results$Assets
Results$ROE <- Results$NetIncomeLoss/Results$Equity
Results$DaysSalesinAccountingReceivable <- 360 / (Results$Revenues/Results$AccountsReceivable)
Results$DepreciationOverPlant <- Results$DepreciationAmortization/Results$PropertyPlantAndEquipment
Results$EquityOverFixedAssets <- Results$Equity/Results$FixedAssets
Results$TimesInterestEarned <- Results$EBITDA/Results$Interest
Results$SalesToTotalAssets <- Results$Revenues/Results$Assets
Results$PreTaxIncomeToSales <- Results$PreTaxIncome/Results$Revenues
Results$NetIncomeLossToSales <- Results$NetIncomeLoss/Results$Revenues
Results$SalestoCash <- Results$Revenues/Results$Cash
Results$SalestoWorkingCapital <- Results$Revenues/(Results$CurrentAssets-Results$CurrentLiabilities)
Results$SalesToFixedAssets <- Results$Revenues/Results$FixedAssets
Results$WorkingCaitalToAssets <- (Results$CurrentAssets-Results$CurrentLiabilities)/Results$Assets
Results$EBITDAMarginRatio <- Results$EBITDA/Results$Revenues
Results$CashFlowOperationsToDebt <- Results$CashFlowOperations/Results$debt
Results$NetIncomeLossToCashflow <- Results$NetIncomeLoss/Results$CashFlowOperations

Results <- Results %>%
  group_by(symbol) %>%
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



