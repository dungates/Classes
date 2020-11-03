# install.packages("ggvis")
library(ggvis)
library(tidyverse)
library(lubridate)
library(nycflights13)

# I shouldn't have to write a fucking function for this
getdata <- function(...)
{
  e <- new.env()
  name <- data(..., envir = e)[1]
  e[[name]]
}

# now load your data calling getdata()
df <- getdata("cocaine")
str(df)
unique(df$state) # Only 45 states idk which ones missing tho

df <- df %>% mutate(date = lubridate::make_date(year = 2007, month = month, day = 1), state = factor(state))


p <- ggplot(df, aes(x = weight, y = potency, color = state)) + geom_point() + facet_wrap( ~ month.abb[month]) +
  theme(legend.position = "none")


library(plotly)

ggplotly(p)

df %>% ggvis(~potency, ~weight) %>% layer_points()

df %>% ggvis(~month, fill := "red") %>%
  layer_densities() %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Count")

df %>% 
  group_by(month) %>%
  ggvis(~ price, ~potency, opacity := 0.6, fill = ~state) %>% 
  layer_points() %>%
  add_legend("fill", title = "State", orient = "bottom") %>%
  add_axis("x", title = "Estimated Value ($)") %>%
  add_axis("y", title = "Potency (%), 100% = pure cocaine, 0% = all filler") %>%
  add_axis("x", orient = "top", ticks = 0, title = "No Apparent Relationship Between Cocaine Price and Potency in 2007",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0)))




