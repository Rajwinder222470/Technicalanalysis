# Rajwinder Kaur - BDA400 - Assignment 2
# Technical Analysis using R - Preliminary Stage
# File: load_and_statistics.R

# Install packages once if they are not already installed:
# install.packages(c("quantmod", "TTR"))

library(quantmod)
library(TTR)

# Read stock symbols from portfolio.txt and remove blank lines.
read_portfolio <- function(file = "portfolio.txt") {
  symbols <- readLines(file, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]
  unique(toupper(symbols))
}

# Import historical stock data.
# auto.assign = FALSE returns each symbol as its own data object.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(file)

  stock_data <- lapply(symbols, function(symbol) {
    tryCatch(
      getSymbols(Symbols = symbol,
                 src = "yahoo",
                 from = from,
                 to = to,
                 auto.assign = FALSE),
      error = function(e) {
        warning(paste("Could not load", symbol, "-", e$message))
        return(NULL)
      }
    )
  })

  names(stock_data) <- symbols
  stock_data[!vapply(stock_data, is.null, logical(1))]
}

# Statistical mode for numeric data.
# R does not use a statistical mode function for this purpose by default,
# so this function returns the most frequent rounded value.
calculate_mode <- function(x, digits = 4) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)

  rounded <- round(x, digits)
  counts <- table(rounded)
  as.numeric(names(counts)[which.max(counts)])
}

# Calculate required statistics for each stock.
calculate_statistics <- function(stock_data, moving_average_n = 20) {
  results <- lapply(names(stock_data), function(symbol) {
    data <- stock_data[[symbol]]
    close_prices <- as.numeric(Cl(data))
    close_prices <- close_prices[is.finite(close_prices)]

    moving_average <- SMA(close_prices, n = moving_average_n)

    data.frame(
      Stock = symbol,
      Moving_Average_20 = as.numeric(tail(moving_average, 1)),
      Mean = mean(close_prices),
      Mode = calculate_mode(close_prices),
      Median = median(close_prices),
      Standard_Deviation = sd(close_prices),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

# ---- Run the analysis ----
stock_data <- load_stock_data("portfolio.txt")
statistics <- calculate_statistics(stock_data, moving_average_n = 20)

print(statistics)

# Optional: save the results for checking/submission.
write.csv(statistics, "statistics_output.csv", row.names = FALSE)

# Optional inspection:
# names(stock_data)
# head(stock_data$AAPL)
# summary(stock_data$AAPL)
