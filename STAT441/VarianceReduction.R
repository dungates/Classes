h <- function(x) x^2 > 0.8

library(tidyverse)
set.seed(5739)

# function of interest:
h <- function(x) x^2 > 0.8

n_sims <- 1000
# 1. Simulate many examples
x <- rbeta(n_sims, shape1 = 2, shape2 = 4)
# 2. Compute function of interest on each example
h(x)
# 3. Summarise, in this case, take sample mean
#    to estimate expected value
mean(h(x))


std <- function(x) {
  sd(x)/sqrt(length(x))
}

std(h(x))



Y <- runif(n_sims)
weights <- dbeta(Y, shape1 = 2, shape2 = 4)/dunif(Y)
weighted.mean(h(Y), w = weights)

naive_mc_estimate <- function(n_sims) {
  X <- rbeta(n_sims, shape1 = 2, shape2 = 4)
  mean(h(X))
}
naive_mc_estimate(1000)

# Function on importance sampling estimate
weighted_mean <- function(n_sims, h = function(x) x^2 > 0.8, alpha, beta) {
  Y <- runif(n_sims)
  weights <- dbeta(Y, shape1 = alpha, shape2 = beta)/dunif(Y)
  weighted.mean(h(Y), w = weights)
}
weighted_mean(1000, h, 2, 4)


# Get estimate of variance/standard deviation of the naive MC estimate
naive_estimates <- map_dbl(1:1000, ~ naive_mc_estimate(n_sims = 1000))
sd(naive_estimates)
imp_estimates <- map_dbl(1:1000, ~ weighted_mean(n_sims = 1000, h, 2, 4))
sd(naive_estimates)/sd(imp_estimates)


# Sample from proposal density: Uniform(0, 1)

# Calculate function of interest

# Take weighted average of transformed samples






