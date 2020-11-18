x <- read_rds(url("https://data.cwick.co.nz/15-x.rds"))

dmix <- function(x, theta){
  pi <- theta[1]
  mu_1 <- theta[2]
  mu_2 <- theta[3]
  sigma_1 <- theta[4]
  sigma_2 <- theta[5]
  pi * dnorm(x, mean = mu_1, sd = sigma_1) +
    (1 - pi) * dnorm(x, mean = mu_2, sd = sigma_2)
}
dmix(0, c(0, 1, 1, 1, 1))
dmix(0, c(0.8, 0.2, 2, 1, 0.3))
ggplot() +
  geom_histogram(aes(x = x, y = stat(density)), binwidth = 0.25) +
  stat_function(fun = dmix, args = list(theta = c(0.9360894, 0.3052268, 2.2481530, 1.2413592, 0.1165862)))

nllhood_mix <- function(theta, x){
  d <- dmix(x = x, theta = theta)
  -1 * sum(log(d))
}
nllhood_mix(theta = c(0.8, 0.2, 2, 1, 1), x = x)

optim(par = c(0.9, 0.2, 2, 1, 0.3), fn = nllhood_mix, x = x, method = "BFGS")










