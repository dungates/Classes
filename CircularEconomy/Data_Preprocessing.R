library(tidyverse)
library(readr)
options(scipen=999)
####Read and inspect datasets####
data1 <- read_csv("coastal_population_vs_mismanaged_plastic.csv", stringsAsFactors = FALSE)
data2 <- read_csv("inadequately-managed-plastic.csv", stringsAsFactors = FALSE)
data3 <- read_csv("mismanaged-plastic-waste-by-region-2010.csv", stringsAsFactors = FALSE)
data4 <- read_csv("mismanaged-waste-global-total.csv", stringsAsFactors = FALSE)
data5 <- read_csv("per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv", stringsAsFactors = FALSE)
data6 <- read_csv("per-capita-plastic-waste-vs-gdp-per-capita.csv", stringsAsFactors = FALSE)
data7 <- read_csv("plastic-waste-generation-total.csv", stringsAsFactors = FALSE)
data8 <- read_csv("plastic-waste-per-capita.csv", stringsAsFactors = FALSE)
dev.country <- read_csv("country-codes.csv", stringsAsFactors = FALSE)
GDP <- read_csv("GDP.csv", stringsAsFactors = FALSE)
Income <- read_csv("INCOME.csv", stringsAsFactors = FALSE)
continents <- read_csv("Countries-Continents.csv", stringsAsFactors = FALSE)
####Update by filtering rows that are only in 2010####
data1 <- data1 %>% 
  filter(Year == 2010)
data3 <- data3 %>% #Greater area
  filter(Year == 2010)
data5 <- data5 %>% 
  filter(Year == 2010) %>% 
  select(-Total.population..Gapminder.,-GDP.per.capita..PPP..constant.2011.international.....Rate.)
data6 <- data6 %>% 
  filter(Year == 2010) %>% 
  select(-Total.population..Gapminder.,-Per.capita.plastic.waste..kilograms.per.person.per.day.)
dev.cty <- dev.country %>% 
  select(official_name_en, Developed...Developing.Countries) %>% 
  rename(Country = "official_name_en", Development = "Developed...Developing.Countries")
GDPg <- GDP %>% 
  select('Country.Name', 'X2010', 'X2009', 'X2008') %>% 
  mutate(avg.growth = (X2010+ X2009+ X2008)/3) %>% 
  select(Country.Name, avg.growth) %>% 
  rename(Country = "Country.Name", Avg.3Y.GDP.Growth = "avg.growth")
####Full join all the data frames except data3, which wil be dealed with later, at once####
plastic <- list(data1, data2, data4, data5, data6, data7, data8) %>% reduce(full_join, by = c("Entity", "Year", "Code"))
####Change column names####
names(plastic) <- tolower(names(plastic))
plastic <- plastic %>% 
  rename(Country = "entity", 
         Code = "code",
         Mismanaged.Plastic.Waste.Total = "mismanaged.plastic.waste..tonnes.",
         Coastal.Population = "coastal.population",
         Population = "total.population..gapminder.",
         Inadequately.Managed.Waste.Share = "share.of.plastic.inadequately.managed....",
         Mismanaged.Plastic.Waste.Share = "mismanaged.waste....global.total.....of.global.total.",  
         Mismanaged.Plastic.Waste.Per.Capita. = "per.capita.mismanaged.plastic.waste..kilograms.per.person.per.day.",
         GDP.Per.Capita = "gdp.per.capita..ppp..constant.2011.international.....constant.2011.international...",
         Plastic.Waste.Generation.Total = "plastic.waste.generation..tonnes..total...tonnes.per.year.",
         Plastic.Waste.Per.Capita = "per.capita.plastic.waste..kilograms.per.person.per.day."
  )
plastic <- left_join(plastic, dev.cty, by = "Country")
plastic <- left_join(plastic, GDPg, by = "Country")
plastic <- left_join(plastic, continents, by = "Country")
####Clean Missing Value####
plastic$Coastal.Population[is.na(plastic$Coastal.Population)] <- 0
plastic$Land <- ifelse(plastic$Coastal.Population == 0, 'Landlocked', 'Coastal')
plastic$Land <- ifelse(plastic$Coastal.Population == 0, 'Landlocked', 'Coastal')
#
Total.PW <- plastic %>% 
  summarise(sum(na.omit(Plastic.Waste.Generation.Total)))
Total.PW <- as.numeric(Total.PW)
plastic <- plastic %>% mutate(Plastic.Waste.Share = Plastic.Waste.Generation.Total/Total.PW)
####Reoder Columns####
plastic <- plastic[c(1, 2, 13, 15, 3, 6, 5, 10, 11, 12, 16, 4, 9, 8, 7, 14)]
plastic$Eco <- ifelse(plastic$Avg.3Y.GDP.Growth <0, 'Recession', ifelse(plastic$Avg.3Y.GDP.Growth >3, 'Inflation', 'Normal'))
####Output plastic.csv####
write.csv(plastic, file = "plastic.csv")
####Visualization####
head(plastic[order(-plastic$Plastic.Waste.Per.Capita),],5) %>% 
ggplot(aes(x=Country, y=Plastic.Waste.Per.Capita)) +
  geom_bar(stat = "identity")

