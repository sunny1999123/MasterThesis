
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


#Revenue
Revenue <- grepl("revenue", DF_results$desc, ignore.case = TRUE)
Revenue <- DF_results[Revenue,]

desc_counts_rev <- Revenue %>%
  group_by(desc) %>%
  summarise(count = n())%>%
  arrange(desc(count))
head(desc_counts_rev,10)




