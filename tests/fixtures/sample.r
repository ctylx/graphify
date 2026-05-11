library(ggplot2)
require(dplyr)
source("helper.R")

analyze_data <- function(df) {
  result <- dplyr::mutate(df, total = x + y)
  class(result) <- "analyzed"
  return(result)
}

normalize = function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

plot.myclass <- function(x, ...) {
  UseMethod("plot")
}

compute_stats <- function(data) {
  if (inherits(data, "data.frame")) {
    summary(data)
  } else {
    c(mean = mean(data), sd = sd(data))
  }
}

helper_func <- function(x) {
  normalized <- normalize(x)
  compute_stats(normalized)
}

result <- analyze_data(data.frame(x = 1:10, y = rnorm(10)))
dplyr::select(result, x)
