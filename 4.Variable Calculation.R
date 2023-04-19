
#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(robustHD)
library(DescTools)

#Load dataframe
Results <- as.data.frame(read.csv("CleanedResults.csv"))
Results <-Results %>% arrange(symbol, fy)
#Results2 <- Results[Results$fy != 2017,]


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



#Handle inf numbers
#numeric_cols <- sapply(Results, is.numeric)
#Results <- Results[apply(Results[, numeric_cols], 1, function(x) all(is.finite(x))), ]
Results[] <- lapply(Results, function(x) ifelse(is.infinite(x), 0, x))
Results %>%
  group_by(fy) %>%
  summarize(n_unique_items = n_distinct(FY_symbol))


#EM calculation 
Results <- Results %>%
  group_by(symbol) %>%
  mutate(
    DV = ((CurrentAssets - lag(CurrentAssets, default = first(CurrentAssets)))-(CurrentLiabilities - lag(CurrentLiabilities, default = first(CurrentLiabilities)))
       -(Cash - lag(Cash, default = first(Cash)))-DepreciationAmortization)/lag(Assets, default = first(Assets)),
    IV1 = 1/lag(Assets, default = first(Assets)),
    IV2 = ((Revenues - lag(Revenues, default = first(Revenues)))-(AccountsReceivable - lag(AccountsReceivable, default = first(AccountsReceivable))))/lag(Assets, default = first(Assets)),
    IV3 = PropertyPlantAndEquipment/lag(Assets, default = first(Assets)),
    IV4 = ROA
  )


#Delete 2017 Rows/first rows (STILL DECIDE TO USE)
Results <- Results %>%
  group_by(symbol) %>%
  slice(-1)
#Resultsv2 <- filter(Results, fy!="2017")

#Regression
model <- lm(DV ~ IV1+IV2+IV3+IV4, data= Results)
Results$DiscretionaryAccruals <- resid(model)


# Compute the mean and standard deviation of the DiscretionaryAccruals column
mean <- mean(Results$DiscretionaryAccruals)
std <- sd(Results$DiscretionaryAccruals)

# Create the DiscretionaryAccrualsBinary column using ifelse() function
Results$DiscretionaryAccrualsBinary <- ifelse(Results$DiscretionaryAccruals >= mean - std & 
                                                Results$DiscretionaryAccruals <= mean + std, 
                                              0, 1)

#Checking distribution of EM proxy
Results %>% 
  group_by(DiscretionaryAccrualsBinary) %>%
  summarise(count=n())


#Winsorize
# winsorize_cols <- function(df, lower_quantile = 0.01, upper_quantile = 0.99) {
#   df_winsorized <- df
#   for (i in 20:55) {
#     df_winsorized[, i] <- winsorize(df[, i], probs = c(lower_quantile, upper_quantile))
#   }
#   df_winsorized
# }
# ResultsWinsor <- winsorize_cols(Results, 0.01, 0.99)

ResultsWinsor <- Results
for (i in 20:55) {
  ResultsWinsor[, i] <- winsorize(Results[, i], probs = c(0.01, 0.99))
}



#Rescale variables (NOT NEEDED, ONLY NORMALIZE THE VARIABLES)
# rescale_vars <- function(data, vars, range=c(0.1, 0.9)) {
#   # Select the specified variables
#   selected_vars <- data[, vars]
#   # Scale the variables to be between 0 and 1
#   scaled_vars <- as.data.frame(scale(selected_vars))
#   # Rescale the variables to the specified range
#   rescaled_vars <- apply(scaled_vars, 2, rescale, to=range)
#   # Combine the rescaled variables with the remaining columns
#   new_data <- cbind(data[, -vars], rescaled_vars)
#   # Return the new data frame
#   return(new_data)
# }
# final_data <- rescale_vars(ResultsWinsor, 20:ncol(ResultsWinsor), c(0.1, 0.9))

#Normalized variables 
normalized_results <- ResultsWinsor
normalized_results[,20:55] <- scale(normalized_results[,20:55])
colMeans(normalized_results[,20:55])
sd(normalized_results[,20:55])






cols <- names(normalized_results)[20:55]

# Set up the plotting window to show multiple histograms side by side
par(mfrow = c(2,2)) # change the numbers to adjust the layout

# Generate a histogram for each selected column
for (col in cols) {
  hist(normalized_results[[col]], main = col, xlab = "Values")
}

#Only keep the interested variables 
Final_data <- cbind(normalized_results[,1:3],normalized_results[,20:55], normalized_results[,62])
write.csv(Final_data, "PreDimensionalityData", row.names = FALSE)

