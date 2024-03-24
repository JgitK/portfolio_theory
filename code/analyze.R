library(dplyr)
library(stringr)

risk_free_rate = 0.0125

# Top returns by industry

top_stocks_by_sector <- function(sdate, edate, sector, top_n) {
  
  filtered_data <- joined_data |>
    filter(date >= sdate & date <= edate)
  
  filtered_data |>
    filter(Sector == {{sector}}) |> 
    group_by(symbol) |> 
    summarize(cum_ret = last(cumulative_return, na.rm = T)) |> 
    arrange(desc(cum_ret)) |> head({{top_n}})
}

top_it <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Information Technology", 10)
top_indus <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Industrial", 10)
top_health <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Healthcare", 10)
top_energy <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Energy", 10)
top_comm <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Communication", 10)
top_staples <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Staples", 10)
top_utils <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Utilities", 10)
top_discr <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Discretionary", 10)
top_realest <- top_stocks_by_sector("2015-01-01", "2024-03-15", "Real Estate", 10)



# Merge individual columns from dataframes
portfolio <- data.frame(stock_data_returns[test_stocks]) |>
  select(ends_with("close"))

returns <- portfolio |>
  mutate_at(vars(ends_with(".close")), ~ round((log(. / lag(.))), 6))


# Correlation
cor(returns, use = "complete.obs")