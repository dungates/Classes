abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}

abs_sets <- function(vec){
  negs <- vec < 0
  vec[negs] <- -vec[negs] 
  vec
}

x <- rnorm(1000)

bench::mark(
  abs_loop = abs_loop(x),
  abs_sets = abs_sets(x)
)

all.equal(abs_sets(x), abs_loop(x))



## New section
library(tidyverse)

n <- 10 # sample size
n_sim <- 100 # simulation size
set.seed(187)
samples_matrix <- rnorm(n = n * n_sim) %>% matrix(ncol = n_sim)
dim(samples_matrix)

sample_means_purrr <- function(n_sims = 1000, n = 10){
  samples <- map(1:n_sims, ~ rnorm(n = n))
  map_dbl(samples, mean)
}

sample_means_matrix <- function(n_sims = 1000, n = 10){
  samples_matrix <- rnorm(n = n_sims*n) %>% matrix(ncol = n_sims)
  colMeans(samples_matrix)
}

set.seed(1819)
means_matrix <- sample_means_matrix()

set.seed(1819)
means_purrr <- sample_means_purrr()

# check they give they do the same thing
all.equal(means_matrix, means_purrr)


timings <- bench::mark(
  {
    set.seed(187)
    sample_means_matrix()
  },
  {
    set.seed(187)
    sample_means_purrr()
  }
)
plot(timings)
# compare timing


## New section


nsim <- 1000
n <- 30
p <- 5
Y <- rnorm(nsim*n) %>% matrix(ncol = nsim)
X <- rnorm(n*p) %>% matrix(ncol = p)

dim(Y)
dim(X)


coefs <- matrix(nrow = p, ncol = nsim)
for (i in 1:nsim){
  coefs[, i] <- lm(Y[, i] ~ X - 1)$coefficients
}


coefs2 <- solve(t(X) %*% X) %*% t(X) %*% Y
coefs3 <- lm(Y ~ X - 1)$coefficients

all.equal(coefs, coefs2)
all.equal(coefs2, coefs3, check.attributes = FALSE)


## New Section

z <- rnorm(100)

mean(z)
mean.default(z)

timings <- bench::mark(
  mean(z),
  mean.default(z)
)
plot(timings)

