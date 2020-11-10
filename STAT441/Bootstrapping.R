library(tidyverse)

n <- 11
sample_medians <- map(1:10000,
                      ~ rt(11,3)) %>%
  map_dbl(median)
sd(sample_medians)
test <- tibble(sample_medians)
ggplot(test, aes(x = sample_medians)) + geom_histogram()



x <- rt(n, df = 3)
sample_medians2 <- sample(x, replace = T, size = length(x)) %>% map_dbl(median)
sd(sample_medians2)



map(1:250, ~ sample(x, size = length(x), replace = TRUE)) %>%
  map_dbl(median) %>%
  sd()

bootstrap_medians <- function(x, n_boot = 250){
  map(1:n_boot, ~ sample(x, size = length(x), replace = TRUE)) %>%
    map_dbl(median)
}
bootstrap_medians(x)

n_sim <- 1000
many_bootstraps <- tibble(
  id = 1:n_sim,
  sample = map(1:n_sim, ~ rt(n, df = 3))
)

many_bootstraps <- many_bootstraps %>%
  mutate(bootstrapped_medians = map(sample, ~ bootstrap_medians(.)))

many_bootstraps <- many_bootstraps %>%
  mutate(est_se = map_dbl(bootstrapped_medians, sd))

mean(many_bootstraps$est_se)

many_bootstraps <- many_bootstraps %>%
  mutate(
    ci = map(bootstrapped_medians, quantile,
             probs = c(0.025, 0.975)),
    covers_true_median = map_lgl(ci, ~ between(0, .[1], .[2]))
  )
# 0 = median for population, t(df = 3)
many_bootstraps %>% pull(covers_true_median) %>% mean()


