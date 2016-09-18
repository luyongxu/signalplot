# 1. Load libraries.
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)

# 2. Logarithmic loss.
log_loss = function(actual, predicted, eps = 1e-15) {
  predicted <- pmax(predicted, eps)
  predicted <- pmin(predicted, 1-eps)
  ll = (-1 / length(actual)) * sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}