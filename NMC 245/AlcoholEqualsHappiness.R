library(readr)
library(tidyverse)
library(StandardizeText)
library(ggthemes)
library(plotly)

# https://www.kaggle.com/mathurinache/world-happiness-report?select=2020.csv
Happiness2018 <- read_csv("/Users/dunk/Classes/NMC 245/2018.csv")
# https://apps.who.int/gho/data/node.main.A1039?lang=en
AlcoholConsumption <- read_csv("/Users/dunk/Classes/NMC 245/SA_0000001400.csv", skip = 1)

# Renaming some variables in happiness
Happiness2018 <- Happiness2018 %>% reshape::rename(c(`Country or region` = "Country",
                                                     Score = "Happiness"))# Easier for join but remember some regions in there

AlcoholConsumption <- AlcoholConsumption %>% select(Country, `Beverage Types`, `2018`) %>%
  mutate(`Alcohol Consumption in the Year of our Lord 2018` = `2018`) %>% select(-`2018`)


# Need to pivot_wider beer, wine, spirits and all types
AlcoholConsumption2 <- pivot_wider(AlcoholConsumption, names_from = `Beverage Types`, values_from = `Alcohol Consumption in the Year of our Lord 2018`)

# Checking for country differences
mismatched <- anti_join(AlcoholConsumption2, Happiness2018, by = c("Country")) # Looks like there's a lot (56), too much work to deal with
StandardizeText::standardize.countrynames(mismatched$Country)

# Running country standardization through
AlcoholConsumption3 <- AlcoholConsumption2
AlcoholConsumption3$Country <- StandardizeText::standardize.countrynames(AlcoholConsumption2$Country)
AlcoholConsumption3 <- AlcoholConsumption3 %>% mutate(Country = ifelse(Country == "Dominica", "Dominican Republic", Country))

# Testing join again
mismatched2 <- anti_join(AlcoholConsumption3, Happiness2018, by = c("Country")) # Lol still 47
# One more try
Happiness20182 <- Happiness2018 
Happiness20182$Country <- StandardizeText::standardize.countrynames(Happiness2018$Country)
mismatched3 <- anti_join(AlcoholConsumption3, Happiness20182, by = c("Country")) # Sure we'll take 40

AlcoholHappy <- left_join(AlcoholConsumption3, Happiness20182, by = c("Country")) %>% relocate(Happiness, .after = "Country") %>%
  arrange(`Overall rank`)

write_csv(AlcoholHappy, "/Users/dunk/Classes/NMC 245/AlcoholHappy.csv")

p <- ggplot(AlcoholHappy, aes(Happiness, `All types`, color = Country)) + geom_point() + 
  labs(y = "Alcohol Consumption (Liters per capita for 15+)", x = "Happiness Score") +
  theme_economist() + theme(legend.position = "none") + geom_text(aes(x = 5, y = 16, label = "Looks correlated to me!"))

beer <- ggplot(AlcoholHappy, aes(Happiness, Beer, color = Country)) + geom_point() + 
  labs(y = "Alcohol Consumption (Liters per capita for 15+)", x = "Happiness Score") +
  theme_economist() + theme(legend.position = "none") + geom_text(aes(x = 5, y = 16, label = "Looks correlated to me!"))
plotly::ggplotly(beer)
wine <- ggplot(AlcoholHappy, aes(Happiness, Wine, color = Country)) + geom_point() + 
  labs(y = "Alcohol Consumption (Liters per capita for 15+)", x = "Happiness Score") +
  theme_economist() + theme(legend.position = "none") + geom_text(aes(x = 5, y = 16, label = "Looks correlated to me!"))
plotly::ggplotly(wine)
spirits <- ggplot(AlcoholHappy, aes(Happiness, Spirits, color = Country)) + geom_point() + 
  labs(y = "Alcohol Consumption (Liters per capita for 15+)", x = "Happiness Score") +
  theme_economist() + theme(legend.position = "none") + geom_text(aes(x = 5, y = 16, label = "Looks correlated to me!"))
plotly::ggplotly(spirits)


library(ggpmisc)
plotly::ggplotly(p)
ggplot(AlcoholHappy, aes(Happiness, Spirits, color = Country)) + geom_point() + 
  labs(y = "Alcohol Consumption (Liters per capita for 15+)", x = "Happiness Score") + geom_smooth(method = "lm", formula = y ~ x) + 
  theme(legend.position = "none") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), formula = y ~ x, parse = T)

AlcoholHappy %>% pivot_longer(names_to = "Alcohol", values_from = c("Beer","Spirits","Wine")) %>% 
  ggplot() + geom_bar(aes(x = Country, y = Alcohol))

GGally::ggcorr(AlcoholHappy, label = T)

lares::corr_cross(AlcoholHappy)


# BeerConsumption <- read_csv("/Users/dunk/Classes/NMC 245/beer-consumption-per-person.csv")
# WineConsumption <- read_csv("/Users/dunk/Classes/NMC 245/wine-consumption-per-person.csv")
# SpiritsConsumption <- read_csv("/Users/dunk/Classes/NMC 245/spirits-consumption-per-person.csv")

# Filtering spirit consumption for 2014 and renaming some things
# SpiritsConsumption <- SpiritsConsumption %>% 
#   reshape::rename(c(`Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Spirits` =
#                     "Spirits")) %>% filter(Year == 2013) %>% select(-Code)
# # Filtering beer consumption for 2015 and renaming
# BeerConsumption <- BeerConsumption %>% 
#   reshape::rename(c(`Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Beer` =
#                       "Beer")) %>% filter(Year == 2013) %>% select(-Code)
# # Filtering wine consumption for 2015 and renaming
# WineConsumption <- WineConsumption %>% 
#   reshape::rename(c(`Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Wine` =
#                       "Beer")) %>% filter(Year == 2013) %>% select(-Code)
