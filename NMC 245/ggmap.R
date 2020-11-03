library(ggmap)

#a sequence on portland

register_google(key = "AIzaSyA-GGFjnD5ljBThO3OZxtTXCDQbXQfHTSQ", write = T)

port<-geocode("portland, oregon")
portland<-get_map(port)
PDX<-ggmap(portland)
ggmap(portland)

donut <- readr::read_csv("/Users/dunk/Classes/NMC 245/donut.csv")
#a nice little mappy
PDX +
  geom_point(aes(x = lon, y = lat, colour = hip, size = flavor,stroke =5), data=donut)

#let's zoom this up a little
#call the map from google
portland<-get_map(port, zoom = 11)

#now we translate the basic plotting object into an intermediate var
PDX<-ggmap(portland)
PDX +
  geom_point(aes(x = lon, y = lat, colour = hip, size = flavor,stroke =5), data = donut)

#now a quick jitter
PDX +
  geom_jitter(aes(x = lon, y = lat, colour = hip, size = flavor), data = donut)

#with geom_text
PDX + 
  geom_text(aes(x = lon, y = lat, colour = hip, size = flavor, label = name), data = donut)

#and crime in Corvallis via Zac
#import corvallis noise and let's start cleaning
library(lubridate)
#parsed times
corvallis_noise <- readr::read_csv("/Users/dunk/Classes/NMC 245/corvallis_crime.csv")
bonkers<-lubridate::mdy_hms(corvallis_noise$DATE)
hour<-lubridate::hour(bonkers)
month_day<-lubridate::day(bonkers)
month<-lubridate::month(bonkers)
year<-lubridate::year(bonkers)
bh<-data.frame(hour, month_day, month, year)
corvallis_noise <- corvallis_noise %>% select("CFS","DATE","NATURE.CODE","street_address","XCOORD","YCOORD","DOW")
noise<-bind_cols(corvallis_noise, bh)
View(noise)

#NOW we must be frugal
noiseA<-filter(noise, year > 2017)%>%
  filter(month > 8)

# noise <- noise %>% rename(street_address = ADDRESS)

#make a dataframe with the street_addresses
df <- data.frame(street_address = noise$street_address, stringsAsFactors = FALSE)

#mutate_geocode to put that list into a new dataframe
noise <- noise %>% mutate(street_address = paste0(street_address, ", Corvallis OR"))
noise <- noise %>% distinct(DATE, .keep_all = T)
locations <- noise %>% mutate_geocode(street_address)

write_csv(locations, "/Users/dunk/Classes/NMC 245/locations.csv", row.names = FALSE)

#inner_join that to the original
colnames(noise)[4]<-"street_address"
noise2 <- inner_join(noise, locations, by = "street_address")

#take a quick look
View(noise2)

#get a road map of town
corvallis<-get_googlemap("corvallis, oregon", zoom = 14, maptype = "road")

corvallis2<-ggmap(corvallis)

corvallis2

#and the fun begins...
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = NATURE.CODE), data = noise2)

#and the police really helped us here with formatting
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = NATURE.CODE), data = noise2)+facet_wrap(. ~month)

#from the Houston crimes example
corvallis2 +
  stat_bin2d(
    aes(x = lon, y = lat, colour = NATURE.CODE, fill = mean(hour, na.rm=TRUE)),
    size = .5, bins = 30, alpha = 1/2,
    data = noise2
  )

