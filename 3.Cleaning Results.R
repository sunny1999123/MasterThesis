
#Load libraries
library(dplyr)
library(XBRL)


#Read results of financial info
Results <- read.csv("result_df.csv")

#Clean results 
#First only keep the interesting columns, and then change from long to wide format
#Some variables are stored with different names, every variable needs to be evaluated. 
Assets <- grepl("Assets", Results$desc, ignore.case = TRUE)

Assets <- Results[Assets, ]


