library(readr)


# States path to read in downloaded data from
paths <- list.files("/Users/jackson/Documents/finance/data/stocks/", pattern = "[.]csv$", full.names = T)

# Reads in stock data
stock_data <- paths |>
  set_names(basename) |>
  map(readr::read_csv) 

#stock_data_df <- map(names(stock_data), ~ str_remove(., "\\.csv"))



# Function to add returns to each stock in each dataframe
add_returns <- function(df) {
  
  #Lag is value from previous day
  df <- df |>
    mutate(daily_return = (adjusted / lag(adjusted)) - 1,
           daily_return = ifelse(is.na(daily_return), 0, daily_return),
           cumulative_return = cumprod(1 + daily_return) - 1)
  
  
  #Save data to a csv
  #write_csv(df, file = paste0("/Users/jackson/Documents/finance/data/stocks/", ticker, ".csv"))
  
  return(df)
}

stock_data_returns <- map(stock_data, add_returns)


### Combining all stock data into one dataframe ###

convert_symbol_to_character <- function(df) {
  df$symbol <- as.character(df$symbol)  
  return(df)                            
}

stock_data_returns_char <- map(stock_data_returns, convert_symbol_to_character)

stock_data_returns_bound <- list_rbind(stock_data_returns_char)

### Industry analysis ###

# Read in tickers with industry info
sec_df <- read_csv("/Users/jackson/Documents/finance/data/big_stock_sectors.csv")

indus_df <- sec_df |> filter(Sector == "Industrial")
health_df <- sec_df |> filter(Sector == "Healthcare")
it_df <- sec_df |> filter(Sector == "Information Technology")
comm_df <- sec_df |> filter(Sector == "Communications")
staples_df <- sec_df |> filter(Sector == "Staples")
discr_df <- sec_df |> filter(Sector == "Discretionary")
util_df <- sec_df |> filter(Sector == "Utilities")
fin_df <- sec_df |> filter(Sector == "Financial")
restate_df <- sec_df |> filter(Sector == "Real Estate")
energy_df <- sec_df |> filter(Sector == "Energy")

joined_data <- left_join(stock_data_returns_bound, sec_df, by = c("symbol" = "Ticker"))

