library(tidyquant)
library(tidyverse)

# Gets ticker from csv of stock names
get_column_from_csv <- function(file, col_name) {
  
  tryCatch({
    df <- read_csv(file)
    column <- df[[col_name]]
    return(column)
  }, error = function(e) {
    print("File Doesn't Exist")
  })
}


# Saves to csv from yahoo
save_to_csv_from_yahoo <- function(ticker, syear, smonth, sday, eyear, emonth, eday) {
  
  #Defines time period to use
  start <- as.Date(str_c(syear, smonth, sday, collapse = "-"))
  end <- as.Date(str_c(eyear, emonth, eday, collapse = "-"))
  
  #Reads data into dataframe via tidyquant
  df <- tq_get(ticker, get = "stock.prices", from = start, to = end)
  
  ticker <- as.character(ticker)
  
  #Saves data to csv
  write_csv(df, file = str_c("/Users/jackson/Documents/finance/data/stocks/", ticker, ".csv"))
  
  return(df)
  
}


# Gets Wilshire 5000 tickers
tickers <- get_column_from_csv("/Users/jackson/Documents/finance/data/Wilshire-5000-Stocks-New.csv", "Ticker")

# Downloads the stock data
tickers |>
  map(possibly(\(ticker) save_to_csv_from_yahoo(ticker, 2015, 1, 1, 2024, 3, 15), NULL))

# Gets tickers for companies by industry
tickers_indus <- get_column_from_csv("/Users/jackson/Documents/finance/data/big_stock_sectors.csv", "Ticker")


# Downloads industry data
tickers_indus |>
  map(possibly(\(ticker) save_to_csv_from_yahoo(ticker, 2015, 1, 1, 2024, 3, 15), NULL))


# States path to read in downloaded data from
paths <- list.files("/Users/jackson/Documents/finance/data/stocks/", pattern = "[.]csv$", full.names = T)

# Reads in stock data
stock_data <- paths |>
  set_names(basename) |>
  map(readr::read_csv) 

# Get column (close) from CSV

get_column_from_csv <- function(file, col_name) {
  
  tryCatch({
    df <- read_csv(file)
    column <- df[[col_name]]
    return(column)
  }, error = function(e) {
    print("File Doesn't Exist")
  })
}

# States path to read in downloaded data from
paths <- list.files("/Users/jackson/Documents/finance/data/stocks/", pattern = "[.]csv$", full.names = T)

# Reads in stock data
stock_data <- paths |>
  set_names(basename) |>
  map(readr::read_csv) 
