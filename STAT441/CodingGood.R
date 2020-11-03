
pick <- function(x){
  y <- x[-(1:round(sqrt(length(x))-1))]
  the_max <- max(x[1:2])
  for(i in 1:length(y)) {
    if (y[i]>the_max){
      return(y[i])
    }
  }
  x[length(x)]
}





x1 <- c(0.1, 0.2, 0.3,   0,   0, 0, 0, 0, 0)
x2 <- c(0.1, 0.2,   0,   0,   0, 0, 0, 0, 0)
x3 <- c(0.1, 0.2, 0.1, 0.3,   0, 0, 0, 0, 0)
x4 <- c(0.1, 0.2, 0.1, 0.3, 0.4, 0, 0, 0, 0)



pick(x1)
pick(x2)
pick(x3)
pick(x4)


# New Class
x1 <- c(0.1, 0.2, 0.3,   0,   0, 0, 0, 0, 0)
x2 <- c(0.1, 0.2,   0,   0,   0, 0, 0, 0, 0)
x3 <- c(0.1, 0.2, 0.1, 0.3,   0, 0, 0, 0, 0)
x4 <- c(0.1, 0.2, 0.1, 0.3, 0.4, 0, 0, 0, 0)

# New function
pick <- function(x) {
  r <- length(x)
  n_see <- round(sqrt(r) - 1)
  seen <- x[1:n_see] # keep track of numbers seen
  havent_seen <- x[-(1:n_see)]
  for (i in 1:length(havent_seen)) {
    if (havent_seen[i] > max(seen)) { # compare to max(seen)
      return(havent_seen[i])
    }
  }
  # if no numbers meet criteria, return last number
  x[length(x)]
}
all.equal(pick(x1), 0.3)
all.equal(pick(x2), 0)
all.equal(pick(x3), 0.3)
all.equal(pick(x4), 0.3)
  
r <- 25
n_sim <- 1000
numbers <- purrr::map(1:n_sim,   ~ runif(r))
got_max <- purrr::map_lgl(numbers, ~ pick(.) == max(.))
mean(got_max)

# one way to get a CI
mean(got_max) + c(-1, 1)* 1.96 * sd(got_max)/sqrt(n_sim)


library(tidyverse)
sim_coin_tosses <- function(n_sims, n, prob){
  # Simulates n_sims sequences of n coin tosses
  # with heads occurring with probability prob
  map(1:n_sims, 
      ~ rbinom(n, size = 1, prob = prob))
}

max_run_length <- function(x){
  # Finds length of longest run in x
  max(rle(x)$lengths)
}

sim_run_lengths <- function(n_sims, n, prob = 0.5){
  # Returns length of longest run in n_sims sequences
  # of coin toss sequences of  n flips, where each flip
  # has prob of being heads, fair by default
  sim_coin_tosses(n_sims, n = n, prob = prob) %>% 
    map_dbl(~ max_run_length(.))
}

sim_run_lengths(n_sims = 10, n = 4, prob = 1)

sim_run_lengths(n_sims = 10, n = 4, prob = 0.5)


















