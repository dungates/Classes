# 1. Name for function
# 2. Arguments
# 3. Write body

sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)


lit <- function(x){
  sd(x, na.rm = T) / mean(x, na.rm = T)
}

x <- data_frame(x = as.double(c(3,17,24,100000000)),
                y = as.double(c(1,2,3,4)))

lit(x$y)


# How many simulations do we need?

