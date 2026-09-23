# Simple Moving Average (SMA)
sma <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0) stop("data must be a non-empty numeric vector")
  if (!is.numeric(period) || length(period) != 1 || is.na(period) || !is.finite(period) || period < 1 || period != floor(period)) stop("period must be a positive integer")
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  if (any(is.na(data))) stop("data must not contain NA values")

  sma_values <- numeric(length(data) - period + 1)

  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  return(sma_values)
}
