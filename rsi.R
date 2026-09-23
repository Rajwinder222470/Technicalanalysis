# Relative Strength Index (RSI), using Wilder smoothing
rsi <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0) stop("data must be a non-empty numeric vector")
  if (!is.numeric(period) || length(period) != 1 || is.na(period) || !is.finite(period) || period < 1 || period != floor(period)) stop("period must be a positive integer")
  if (length(data) <= period) stop("data length must be greater than period")
  if (any(is.na(data))) stop("data must not contain NA values")

  diff_values <- data[2:length(data)] - data[1:(length(data) - 1)]
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else if (diff_values[i] < 0) {
      losses[i] <- abs(diff_values[i])
    }
  }

  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))

  # First RSI value after the initial period
  if (avg_loss == 0) {
    rsi_values[period + 1] <- if (avg_gain == 0) 50 else 100
  } else {
    rs <- avg_gain / avg_loss
    rsi_values[period + 1] <- 100 - (100 / (1 + rs))
  }

  if (length(data) > period + 1) {
    for (i in (period + 2):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

      if (avg_loss == 0) {
        rsi_values[i] <- if (avg_gain == 0) 50 else 100
      } else {
        rs <- avg_gain / avg_loss
        rsi_values[i] <- 100 - (100 / (1 + rs))
      }
    }
  }
  return(rsi_values)
}
