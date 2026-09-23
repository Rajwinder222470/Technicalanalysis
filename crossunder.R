# Crossunder function
crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  if (!is.numeric(arr1) || !is.numeric(arr2)) stop("Both arrays must be numeric")
  if (any(is.na(arr1)) || any(is.na(arr2))) stop("Arrays must not contain NA values")

  crossunder_signals <- rep("False", length(arr1))
  if (length(arr1) > 0) crossunder_signals[1] <- "None"
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      }
    }
  }
  return(crossunder_signals)
}
