library(shiny)
library(data.table)
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(data.table)
library(shinyWidgets)

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



#Modify start and end to date format
Results$start <- as.Date(Results$start)
Results$end <- as.Date(Results$end)

#Keep only last row of each firm, each year, each financial statement item (deletions are quarterly numbers)
df_filtered <- Results %>%
  arrange(symbol, fy, end) %>%  # sort by symbol, fy, and end
  group_by(symbol, fy, desc) %>%  # group by symbol and fy
  slice_tail(n = 1) %>%  # keep only the last row within each group
  ungroup()  # remove grouping

#Get a subset of the data and change order
DF_results <- subset(df_filtered, select = c("symbol", "fy","desc","val" ))


#Clean results 
#Some variables are stored with different names, every variable needs to be evaluated. 

#Check the sum of the number of unique firms per year , which should equal the number of firms per item
DF_results$FY_symbol <- paste(DF_results$fy, DF_results$symbol, sep=" ")
NumberFirms <- unique(DF_results$FY_symbol)
length(NumberFirms) #answer is 2999
#So each item used should occur 2999 times 
DF_results %>%
  group_by(fy) %>%
  summarize(n_unique_items = n_distinct(FY_symbol))








#Clean the data
cleanData <- function(data) {
  #Pre-processing of data
  data$start <- as.Date(data$start)
  data$end <- as.Date(data$end)
  data <- subset(data, select = c("symbol", "fy","desc","val" ))
  data$FY_symbol <- data(data$fy, data$symbol, sep=" ")
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
  
  # Return the cleaned data
  return(data)
}





# Define UI
ui <- fluidPage(
  titlePanel("Data Access"),
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Ticker:", ""),
      numericInput("year", "Year:", min = 1900, max = 2100, value = NULL),
      actionButton("getData", "OK")
    ),
    mainPanel(
      progress = "progress",
      verbatimTextOutput("message")  # Use verbatimTextOutput for displaying text
    )
  )
)

# Define server logic

# Define server logic
server <- function(input, output) {
  cleanedData <- reactiveVal(NULL)  # Store the cleaned data
  
  # Event handler for the "OK" button click
  observeEvent(input$getData, {
    ticker <- input$ticker
    year <- input$year
    
    # Show the progress bar
    withProgress(message = 'Retrieving data...', value = 0, {
      # Increment the progress bar value
      setProgress(0.5)
      
      # Simulate data retrieval process
      Sys.sleep(2)  # Simulating delay, replace with actual data retrieval code
      
      # Call the getData function to retrieve the data
      data <- getData(ticker, year)
      
      # Increment the progress bar value
      setProgress(1)
      
      # Perform data cleaning operations
      cleanedData <- cleanData(data)
      
      # Update the message based on the retrieved data
      if (nrow(data()) > 0) {
        message <- "The data is successfully cleaned."
      } else {
        message <- "The ticker symbol is unknown or the API did not retrieve any data."
      }
      
      # Display the message
      output$message <- renderText({
        message  # Use renderText to render the text
      })
    })
  })
}




# Run the Shiny app
shinyApp(ui = ui, server = server)
















# ui <- fluidPage(
#   titlePanel("Data Access App"),
#   sidebarLayout(
#     sidebarPanel(
#       textInput("ticker", "Ticker Symbol:"),
#       numericInput("year", "Year:", value = NULL)
#     ),
#     mainPanel(
#       tableOutput("resultTable")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   output$resultTable <- renderTable({
#     ticker <- input$ticker
#     year <- input$year
#     if (is.null(year)) {
#       # Handle case when year is not provided
#       return(NULL)
#     }
#     if (is.null(ticker)) {
#       # Handle case when year is not provided
#       return(NULL)
#     }
#     DataAccess(ticker, year)
#   })
# }
# 
# 
# 
# 
# shinyApp(ui = ui, server = server)



