# BDA400 Assignment 5 - Test / demonstration file
# No technical-analysis packages are used.

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

cat("=== SMA ===\n")
x <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(sma(x, 3))

cat("\n=== EMA ===\n")
print(ema(x, 3))

cat("\n=== MACD ===\n")
m <- macd(c(100,105,110,115,120,125,130), 3, 5, 2)
print(m)

cat("\n=== STDEV ===\n")
print(stdev(x))

cat("\n=== LINEAR REGRESSION ===\n")
print(linreg(c(2,4,6,8,10,12), 5, 0))

cat("\n=== RSI ===\n")
prices <- c(45,50,48,55,52,49,58,60,65,62,66,68,64,70,72,69,74,76,73,78,80,77,82,84,81)
print(rsi(prices, 14))

cat("\n=== STOCH RSI ===\n")
print(stoch_rsi(prices, 14, 3, 3))

cat("\n=== CROSSOVER ===\n")
a1 <- c(10,12,15,20,18,22,25,24,21)
a2 <- c(18,20,22,18,15,12,10,11,13)
print(crossover(a1, a2))

cat("\n=== CROSSUNDER ===\n")
print(crossunder(a1, a2))

# Independently check known outputs and the shapes of compound indicators.
stopifnot(isTRUE(all.equal(sma(c(1, 2, 3, 4), 2), c(1.5, 2.5, 3.5))))
stopifnot(isTRUE(all.equal(ema(c(10, 12, 14), 3), c(10, 11, 12.5))))
stopifnot(isTRUE(all.equal(stdev(c(2, 4, 6)), sqrt(8 / 3))))
fit <- linreg(c(2, 4, 6, 8), 4, 0)
stopifnot(isTRUE(all.equal(fit$slope, 2)), isTRUE(all.equal(fit$intercept, 0)))
stopifnot(isTRUE(all.equal(fit$predicted_values, c(2, 4, 6, 8))))
stopifnot(isTRUE(all.equal(m$macd_line - m$signal_line, m$histogram)))
stopifnot(length(rsi(prices, 14)) == length(prices))
sr <- stoch_rsi(prices, 14, 3, 3)
stopifnot(length(sr$d_line) == length(sr$k_line) - 2)
stopifnot(identical(crossover(c(1, 3, 1), c(2, 2, 2)), c("None", "Up", "Down")))
stopifnot(identical(crossunder(c(1, 3, 1), c(2, 2, 2)), c("None", "False", "True")))
cat("\nAll demonstrations and reference checks completed.\n")
