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

portfolio <- joined_data |> 
  filter(symbol %in% sample_stocks) |> 
  select(symbol, date, close) |>
  pivot_wider(names_from = symbol, values_from = close) |>
  select(-date)


# FIND LEAST CORRELATED
correlation <- cor(portfolio, use = "complete.obs")

least_correlated <- as.data.frame(correlation) |>
  rownames_to_column("stock1") |>
  pivot_longer(cols = -stock1, names_to = "stock2", values_to = "correlation") |>
  mutate(abs_corr = abs(correlation)) |>
  arrange(abs_corr) |>
  slice(seq(1, n(), by = 2)) # Removes duplicate pairs
