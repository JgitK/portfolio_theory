library(tidyverse)
library(brisk)

risk_free_rate = 0.0125

# Top returns by industry

joined_data <- read_csv("/Users/jackson/Documents/finance/data/combined_stock_data.csv")

top_stocks_by_sector <- function(sdate, edate, sector, top_n) {
  
  filtered_data <- joined_data |>
    filter(date >= sdate & date <= edate)
  
  filtered_data |>
    filter(Sector == {{sector}}) |> 
    group_by(symbol) |> 
    summarize(cum_ret = last(cumulative_return)) |> 
    arrange(desc(cum_ret)) |> head({{top_n}})
}

# joined_data |> 
#   filter(Sector == "Industrial") |>
#   group_by(symbol) |>
#   summarize(cum_ret = last(cumulative_return)) |> 
#   arrange(desc(cum_ret)) |> head(3)

top_it <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Information Technology", 10)
top_indus <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Industrial", 10)
top_health <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Healthcare", 10)
top_energy <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Energy", 10)
top_comm <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Communication", 10)
top_staples <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Staples", 10)
top_utils <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Utilities", 10)
top_discr <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Discretionary", 10)
top_realest <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Real Estate", 10)

# Combining industry data
top_stocks <- bind_rows(top_it, top_indus, top_health, top_energy, top_comm, top_staples, top_utils, top_discr, top_realest)

# Dataframe of industry data for each symbol
sec_df_sectors <- sec_df |> select(Ticker, Sector)

# 
top_stocks_by_sector <- left_join(top_stocks, sec_df_sectors, by = c("symbol" = "Ticker"))


# RANDOMLY PICK 10 OF THE TOP STOCKS, 1 FROM EACH SECTOR

sample_stocks <- top_stocks_by_sector |>
  group_by(Sector) |>
  sample_n(1) |>
  ungroup()

test_stocks <- c("DTST", # IT
                 "BLDR", #Industrial
                 "ZYXI", # Health
                 "NXE", # Energy
                 "GRVY", # Comms
                 "FRPT", # Staples
                 "EBR", # Utilities
                 "LMPX", # Discretionary
                 "IIPR" # Real Estate
)

portfolio_close <- joined_data |> 
  filter(symbol %in% sample_stocks$symbol) |> 
  select(symbol, date, close, daily_return) 

portfolio_cumret <- joined_data |> 
  filter(symbol %in% test_stocks) |> 
  select(symbol, date, cumulative_return) 

portfolio_wider <- portfolio_close |>
  pivot_wider(names_from = symbol, values_from = close)

portfolio_nodate <- joined_data |> # Correlation function won't work if date is a selected column
  filter(symbol %in% test_stocks) |> 
  select(symbol, date, close) |>
  pivot_wider(names_from = symbol, values_from = close) |>
  select(-date)


# FIND LEAST CORRELATED (close)
correlation <- cor(portfolio_nodate, use = "complete.obs")

least_correlated_close <- as.data.frame(correlation) |>
  rownames_to_column("stock1") |>
  pivot_longer(cols = -stock1, names_to = "stock2", values_to = "correlation") |>
  mutate(abs_corr = abs(correlation)) |>
  arrange(abs_corr) |>
  slice(seq(1, n(), by = 2)) # Removes duplicate pairs


# Define returns
portfolio_returns <- portfolio_close |>
  group_by(symbol) |>
  mutate(daily_ret = log(close / lag(close))) |>
  select(symbol, date, daily_ret) |>
  pivot_wider(names_from = symbol, values_from = daily_ret) |>
  select(-date)

# Daily returns correlation
correlation_daily_ret <- cor(portfolio_returns, use = "complete.obs")

# FIND LEAST CORRELATED (daily return)
least_correlated_close <- as.data.frame(correlation_daily_ret) |>
  rownames_to_column("stock1") |>
  pivot_longer(cols = -stock1, names_to = "stock2", values_to = "correlation") |>
  mutate(abs_corr = abs(correlation)) |>
  arrange(abs_corr) |>
  slice(seq(1, n(), by = 2)) # Removes duplicate pairs


# CREATE RANDOM WEIGHTS EQUAL TO 1
num_stocks <- 9

weights <- runif(num_stocks)
weights <- weights / sum(weights)

# PORTFOLIO RETURN USING MEAN RETURNS

mean_returns <- portfolio_close |> 
  group_by(symbol) |> 
  summarize(mean_return = mean(daily_return, na.rm = TRUE)) |> 
  pull(mean_return)

portfolio_return <- sum(weights * mean_returns) * 252

# Portfolio variance
# Calculate covariance matrix of daily returns
cov_matrix <- portfolio_close %>%
  pivot_wider(names_from = symbol, values_from = daily_return) %>%
  select(-date) %>%
  select(-close) %>%  # Remove the close column
  cov()

portfolio_volatility <- sqrt(t(weights) %*% cov_matrix %*% weights) * sqrt(252)

