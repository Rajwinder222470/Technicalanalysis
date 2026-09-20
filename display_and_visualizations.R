# Rajwinder Kaur - BDA400 - Assignment 2
# Technical Analysis using R - Display and Visualizations
# File: display_and_visualizations.R

library(quantmod)
library(TTR)

source("load_and_statistics.R")

# Display the imported data for one stock.
display_stock <- function(stock_data, symbol) {
  if (!symbol %in% names(stock_data)) {
    stop(paste("Stock", symbol, "was not found in the imported data."))
  }

  print(head(stock_data[[symbol]]))
  print(tail(stock_data[[symbol]]))
  print(summary(stock_data[[symbol]]))
}

# Plot OHLC price data and a 20-day simple moving average.
plot_stock_with_ma <- function(stock_data, symbol, n = 20) {
  if (!symbol %in% names(stock_data)) {
    stop(paste("Stock", symbol, "was not found in the imported data."))
  }

  data <- stock_data[[symbol]]
  close_prices <- Cl(data)
  ma <- SMA(close_prices, n = n)

  chartSeries(
    data,
    name = paste(symbol, "Price and", n, "-Day Moving Average"),
    theme = chartTheme("white")
  )
  addTA(ma, on = 1, col = "blue", lwd = 2)
}

# Bar chart of mean closing price for the portfolio.
plot_mean_prices <- function(statistics) {
  barplot(
    statistics$Mean,
    names.arg = statistics$Stock,
    las = 2,
    main = "Mean Closing Price by Stock",
    ylab = "Mean Closing Price"
  )
}

# ---- Run display/visualization examples ----
display_stock(stock_data, "AAPL")
plot_stock_with_ma(stock_data, "AAPL", n = 20)
plot_mean_prices(statistics)

# Other symbols can be displayed by changing "AAPL":
# display_stock(stock_data, "MSFT")
# plot_stock_with_ma(stock_data, "NVDA", n = 20)
