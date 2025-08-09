library(bench)
library(tidyverse)

vectorized <- function(x){
  return(x^2)
}

square_for1 <- function(x) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    result[i] <- x[i]^2
  }
  return(result)
}

square_for2 <- function(x) {
  result <- numeric(length(x))
  for (i in 1:length(x)) {
    result[i] <- x[i]^2
  }
  return(result)
}


square_map1 <- function(x) {
  purrr::map_dbl(x, \(x) x^2)
}

square_map2 <- function(x) {
  purrr::map_dbl(x, ~.x^2)
}

# Generate a large vector of numbers to benchmark
vec <- 1:1e5

# Benchmark the for loop and purrr::map() approach
benchmarks <- bench::mark(
  vectorized = vectorized(vec),
  for_loop1 = square_for1(vec),
  for_loop2 = square_for2(vec),
  purrr_map1 = square_map1(vec),
  purrr_map2 = square_map2(vec),
  iterations = 1000
)

print(benchmarks)
plot(benchmarks)
