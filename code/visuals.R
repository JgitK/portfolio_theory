library(tidyverse)
library(plotly)

# Lineplot of closing prices
lineplot_close <- ggplot(portfolio_close, aes(date, close, color = symbol)) +
  geom_line() +
  theme_minimal()

ggplotly(lineplot_close) |>
  layout(xaxis = list(rangeslider = list(type = "date")))


# Lineplot of cumulative returns
lineplot_cumret <- ggplot(portfolio_close, aes(date, cumulative_return, color = symbol)) +
  geom_line() +
  theme_minimal()

ggplotly(lineplot_cumret) |>
  layout(xaxis = list(rangeslider = list(type = "date")))
