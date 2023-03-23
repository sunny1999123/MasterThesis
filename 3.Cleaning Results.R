
#Load libraries
library(dplyr)
library(XBRL)


#Read results 
Results <- read.csv("result_df.csv")


Assets <- grepl("Assets", Results$desc, ignore.case = TRUE)

Assets <- Results[Assets, ]

Cash <- grepl("Cash", Results$desc, ignore.case = TRUE)

Cash <- Results[Cash, ]

Receivable <- grepl("Receivable", Results$desc, ignore.case = TRUE)

Receivable <- Results[Receivable, ]
Inventory <- grepl("Inventory", Results$desc, ignore.case = TRUE)

Inventory <- Results[Inventory, ]
ResearchAndDevelopmentExpense <- grepl("ResearchAndDevelopmentExpense", Results$desc, ignore.case = TRUE)

ResearchAndDevelopmentExpense <- Results[ResearchAndDevelopmentExpense, ]


Earnings <- grepl("Earnings", Results$desc, ignore.case = TRUE)

Earnings <- Results[Earnings, ]
