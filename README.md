# MasterThesis
Master Thesis for the MScBA Business Analytics &amp; Management at the RSM.

## Description

This GitHub repo consists of all the relevant files and documents that are used by Apoorv Sunny Bhatia in his thesis titled: "Evaluating Machine Learning Techniques
to Detect Earnings Management in Financial statements using XBRL". There are several folders. 
- Data: This folder contains all the data that is used in this study.
- EMDetectionTool: This folder contains all the relevant code that is used for the EM detection tool.
- Figures: This folder contains all the figures that are used in the thesis.
- Tuning: This folder contains the tuning results of the three machine learning models that are used in this research.

All the relevant R files are not stored in a specific folder. Next chapter elaborates on the R files that are used in this research.


## R scripts Manual 	:clipboard:

-  1.Ticker_Symbols.R  

Desription: this R file takes a set of ticker_symbols of the S&P 500 as input and is then used to clean the data.

Input file: Ticker_Symbols.xlsx

Input file: Industry_codes csv

Intermediate file: Unique_Tickers.txt

Output file: Tickers.csv


- 2.Scraper.R

Description: this R file takes the cleaned CSV file of ticker symbols as input and is then used to create a scraping algorithm for the financial statement information.

Input file: Tickers.csv

Input file: cik_ticker.csv

Output file: Results_2022_df.csv

Output file: Result_df.csv

- 3.Cleaning Results.R

Description: this R file takes the financial statement information as input and cleans the data, which resulted in a tidy dataframe with no missing values, which can be used for the analysis.

Input file: Results_2022_df.csv

Input file: Result_df.csv

Output file: CleanedResults.csv

- 4.Variable Calculation.R

Description: this R file takes the cleaned financial information as input and calculated all the various features that are needed.

Input file: CleanedResults.csv

Output file: PreDimensionalityData.csv


- 5.Feature Selection.R

Description: this R file takes the data with all its features as input and applies a lasso regularization method to select the features that will be used in the machine learning models.

Input file: PreDimensionalityData.csv

Output file: Filtered_Results.csv

- 6.Random Forest.R

Description: this R file takes the filtered data as input and applies a random forest model to the data.

Input file: Filtered_Results.csv

Intermediate file: rf_tune_res.rds

- 7.Gradient Boosting.R

Description: this R file takes the filtered data as input and applies a Gradient Boosting model to the data.

Input file: Filtered_Results.csv

Intermediate file: xgb_tune_res.rds


- 8.SupportVectorMachine.R

Description: this R file takes the filtered data as input and applies a Support Vector Machine model to the data.

Input file: Filtered_Results.csv

Intermediate file: svm_tune_res.rds

- Thesis.pdf

Description: This file contains the final version of the thesis.



