m <- 13
a <- 5

m <- 2^31 - 1
a <- 7^5

x <- numeric(10)
x[1] <- 4

for (i in 2:length(x)){
  x[i] <- (a * x[i-1]) %% m
}

x / m

?set.seed

set.seed(1871)
x <- runif(10)
y <- runif(10)
(unif_1 <- c(x, y))
set.seed(1871)
(unif_2 <- runif(20))

nums <- rep(0, 10)
for (i in 1:10) {
  nums[i] <- (-1/5)*log(1-runif(1))
}
nums


