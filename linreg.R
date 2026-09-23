# Linear Regression
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || length(regressionSource) == 0)
    stop("regressionSource must be a non-empty numeric vector")
  if (any(is.na(regressionSource))) stop("regressionSource must not contain NA values")
  if (!is.numeric(regressionLength) || length(regressionLength) != 1 || is.na(regressionLength) || !is.finite(regressionLength) || regressionLength < 2 || regressionLength != floor(regressionLength))
    stop("regressionLength must be an integer of at least 2")
  if (!is.numeric(regressionOffset) || length(regressionOffset) != 1 || is.na(regressionOffset) || !is.finite(regressionOffset) || regressionOffset < 0 || regressionOffset != floor(regressionOffset))
    stop("regressionOffset must be a non-negative integer")

  n <- length(regressionSource)

  if (regressionLength > n)
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  if (regressionOffset >= regressionLength)
    stop("regressionOffset must be less than regressionLength")

  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  if (start_index >= end_index) stop("At least two regression points are required after applying the offset")
  source_subset <- regressionSource[start_index:end_index]
  if (length(source_subset) < 2) stop("At least two regression points are required after applying the offset")
  index_values <- 1:length(source_subset)

  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)

  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  if (denominator == 0) stop("Cannot compute regression because denominator is zero")

  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept

  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  return(result)
}
