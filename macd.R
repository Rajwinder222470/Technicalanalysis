# Moving Average Convergence Divergence (MACD)
# Requires ema() to be loaded first.
macd <- function(data, short_period, long_period, signal_period) {
  valid_period <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x >= 1 && x == floor(x)
  if (!valid_period(short_period) || !valid_period(long_period) || !valid_period(signal_period)) stop("All periods must be positive integers")
  if (short_period >= long_period) stop("short_period must be less than long_period")

  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line

  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  return(result)
}
