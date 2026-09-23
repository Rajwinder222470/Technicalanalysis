# Exponential Moving Average (EMA)
ema <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0) stop("data must be a non-empty numeric vector")
  if (!is.numeric(period) || length(period) != 1 || is.na(period) || !is.finite(period) || period < 1 || period != floor(period)) stop("period must be a positive integer")
  if (any(is.na(data))) stop("data must not contain NA values")

  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]

  if (length(data) >= 2) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  return(ema_values)
}
