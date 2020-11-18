f <- read_rds(url("http://stat541.cwick.co.nz/notes/10-f.rds"))

ggplot() + geom_function(aes(x = f))
plot(f)

ggplot(data.frame(x = c(-1000, 1000)), aes(x = x)) + 
  stat_function(fun = f)

optimise(f, interval = c(-2,0))

f(-1.09)
??optimise


f2 <- function(x) x^2
optimize(f2, c(-1000, 1000))

ggplot(data.frame(x = c(-1000, 1000)), aes(x = x)) + 
  stat_function(fun = f2)

rlang::env_get(f, "fun")



