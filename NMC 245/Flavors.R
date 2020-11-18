library(readr)
library(ggraph)
library(igraph)
library(tidygraph)
library(visNetwork)
library(lubridate)
library(tidyverse)
library(network)

flavors <- read_csv("/Users/dunk/Classes/NMC 245/flavors.csv", col_names = T)

# Some data cleaning first
# This is fairly clean but includes the extraneous longer inputs such as humans have 2 arms. Also fixes japapeño entries
# And adds id column
flavors2 <- flavors %>% drop_na() %>% mutate(SOURCE = str_to_title(SOURCE),
                                             TARGET = str_to_title(TARGET),
                                             VALUE = str_to_title(VALUE)) %>%
  mutate_all(funs(str_replace(., "Jala", "Jalapeño"))) %>%
  mutate_all(funs(gsub("(Jalapeño).*", "\\1", .))) %>%
  rowid_to_column()
# Funny if we include the old answers this just keeps the first word of all original answers
flavors3 <- flavors2 %>% mutate_all(funs(word(., start = 1)))
# Does same as above but first deletes anything after a space
flavors4 <- flavors2 %>% mutate_all(funs(gsub(" .*", "", .)))
arsenal::comparedf(flavors3, flavors4) # Produces the exact same thing!

# Lists of things in the network
fruits <- c("Apple","Pineapple","Pear","Orange","Banana","Raspberry","Blueberry","Blackberry","Strawberry","Huckleberry",
            "Peach", "Grapefruit","Watermelon","Cherry","Pumpkin","Grape","Mango","Plum","Guava","Habanero",
            "Jalapeño","Lime","Pomegranate","Cucumber")
meats <- c("Chicken", "Beef", "Bacon","Buffalo","Sirloin","Prosciutto","Pork","Teriyaki","Kimchi","Salmon", "Tuna")
veggies <- c("Broccoli","Onion","Beets","Cilantro")

flavors4 <- flavors4 %>% mutate(fruit = if_else(
  str_detect(SOURCE, pattern = paste(fruits,collapse = "|")),1,0),
  meat = if_else(
    str_detect(SOURCE, pattern = paste(meats,collapse = "|")),1,0),
  veggie = if_else(
    str_detect(SOURCE, pattern = paste(veggies,collapse = "|")),1,0))
write_csv(flavors4, "/Users/dunk/Classes/NMC 245/flavors.csv")
#flavors4 <- flavors4 %>% mutate(color = rowMeans(select(., 5:7)))
flavors4 <- flavors4 %>% mutate(color = floor(runif(nrow(.), 1, 7)))

# Making the network dataframe
nw <- network(flavors4, directed = T, matrix.type = "edgelist")
x <- flavors4$color
nw %v% "Color" = as.character(x)
y = RColorBrewer::brewer.pal(7, "Blues")[ c(3, 1, 6, 5, 2) ]
names(y) = levels(x)
# Colorful networking
GGally::ggnet2(nw, label = T, label.size = 3, arrow.size = 3, arrow.gap = 0.03,
               color = "Color", palette = y, alpha = 0.67)


# Now some real networking
nw2 <- network(flavors2, directed = T, matrix.type = "edgelist")
GGally::ggnet2(nw2, label = T, label.size = 3, arrow.size = 3, arrow.gap = 0.03)
