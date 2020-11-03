library(tidyverse)

(mi_letters <- str_split("Mississippi", "")[[1]])

# 1. Randomly sampling without replacement over and over

# 2. Count number of "adjacent letters"

# 3. Take number of time no adjacent letters / number of simulations

one_reordering <- sample(mi_letters)
rle(one_reordering)
# Subtracting actual character length from length of sum of rle lengths
sum(rle(one_reordering)$lengths == 1) - length(rle(one_reordering)$lengths) == 0
# Checking if the max of lengths is 1
max(rle(one_reordering)$lengths) == 1
# Check if all lengths equal 1
all(rle(one_reordering)$lengths == 1)

# test <- function(x) {
#   y <- sample(mi_letters)
#   z <- all(rle(y)$lengths == 1)
#   while(z == FALSE) {
#     print("nope")
#   }
# }
# test(mi_letters)

my_list <- list(
  1,
  1:2,
  1:3
)

my_list
length(my_list)
one_element <- my_list[[1]]
length(one_element)

purrr::map(my_list, ~mean(.))

n_sim <- 1000

many_reorderings <- purrr::map(1:n_sim, ~sample(mi_letters))
str(many_reorderings)

adj_letters_diff <- purrr::map_lgl(many_reorderings, ~ all(rle(.)$lengths == 1))

# theta_hat <- mean(adj_letters_diff)
# 
# t.test(theta_hat, conf.level = 0.95)

adj_letters_diff %>% table()
adj_letters_diff %>% mean()


# Function for many simulations
(mi_letters <- str_split("Mississippi", "")[[1]])
mc_simulations <- function(num_sims = 1000) {
  (mi_letters <- str_split("Mississippi", "")[[1]])
  many_reorderings <- map(1:num_sims, 
                          ~ sample(mi_letters))
  adj_letters_diff <- map_lgl(many_reorderings, 
                              ~ all(rle(.x)$lengths == 1))
  mean(adj_letters_diff)
}

mc_simulations()




# Map has variants map_int, map_lgl, map_chr

paste0((factorial(11)/(factorial(4)*factorial(4)*factorial(2)))/(factorial(11))*100,"%")



# Final functions
mc_estimate_prob <- function(){
  many_reorderings <- map(1:num_sims, 
                          ~ sample(mi_letters))
  adj_letters_diff <- map_lgl(many_reorderings, 
                              ~ all(rle(.)$lengths == 1))
  mean(adj_letters_diff)
}
mc_estimate_prob()  
mc_estimate_prob <- function(letters, num_sims = 1000) {
  many_reorderings <- map(1:num_sims, 
                          ~ sample(letters))
  adj_letters_diff <- map_lgl(many_reorderings, 
                              ~ all(rle(.)$lengths == 1))
  mean(adj_letters_diff)
}
mc_estimate_prob(letters = mi_letters)  
mc_estimate_prob <- function(){
  many_reorderings <- map(1:num_sims, 
                          ~ sample(mi_letters))
  adj_letters_diff <- map_lgl(many_reorderings, 
                              ~ all(rle(.)$lengths == 1))
  mean(adj_letters_diff)
}
mc_estimate_prob()  
mc_estimate_prob <- function(letters, num_sims = 1000) {
  many_reorderings <- map(1:num_sims, 
                          ~ sample(letters))
  adj_letters_diff <- map_lgl(many_reorderings, 
                              ~ all(rle(.)$lengths == 1))
  mean(adj_letters_diff)
}
mc_estimate_prob(letters = mi_letters)  
  
  # 1.
many_mc_ests <- map(1:500, ~ mc_estimate_prob(mi_letters))
# 2.
many_mc_ests_flat <- many_mc_ests %>% flatten_dbl()
# 3. 
sd(many_mc_ests_flat)
ggplot(mapping = aes(x = many_mc_ests_flat)) + 
  geom_bar()









