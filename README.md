# MasterThesis
MasterThesis for the MScBA Business Analytics &amp; Management at the RSM

## Description

This GitHub repo consists of all the relevant files and documents that are used by Apoorv Sunny Bhatia in his thesis titled: "Evaluating Machine Learning Techniques
to Detect Earnings Management in Financial statements using XBRL".


## File Manual	:clipboard:

-  1.Ticker_Symbols.R  

Desription: this R file takes a set of ticker_symbols of the S&P 500 as input and is then used to clean the data

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
Output file: Final_df.csv











