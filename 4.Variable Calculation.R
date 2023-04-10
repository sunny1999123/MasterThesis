
#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(robustHD)
#Load dataframe
Results <- as.data.frame(read.csv("CleanedResults.csv"))
Results <-Results %>% arrange(symbol, fy)

#Winsorize
winsorize_cols <- function(df, lower_quantile = 0.025, upper_quantile = 0.975) {
  df_winsorized <- df
  for (i in 4:19) {
    df_winsorized[, i] <- winsorize(df[, i], probs = c(lower_quantile, upper_quantile))
  }
  df_winsorized
}

Results <- winsorize_cols(Results, 0.05, 0.95)



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

#Delete 2017 Rows (STILL DECIDE TO USE)
# Results <- Results %>% 
#   group_by(symbol) %>% 
#   slice(-1)
# Results <- filter(Results, fy!="2017")



#Delte inf rows
numeric_cols <- sapply(Results, is.numeric)
Results <- Results[apply(Results[, numeric_cols], 1, function(x) all(is.finite(x))), ]


#Rescale variables
rescale_vars <- function(data, vars, range=c(0.1, 0.9)) {
  # Select the specified variables
  selected_vars <- data[, vars]
  # Scale the variables to be between 0 and 1
  scaled_vars <- as.data.frame(scale(selected_vars))
  # Rescale the variables to the specified range
  rescaled_vars <- apply(scaled_vars, 2, rescale, to=range)
  # Combine the rescaled variables with the remaining columns
  new_data <- cbind(data[, -vars], rescaled_vars)
  # Return the new data frame
  return(new_data)
}
final_data <- rescale_vars(Results, 20:ncol(Results), c(0.1, 0.9))

#EM calculation 
final_data$TotalCurrentAccruals <- (final_data$CurrentAssets- lag(final_data$CurrentAssets, default = first(final_data$CurrentAssets)))







