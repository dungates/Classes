#lets do some simple chloropleth maps

#we need to start with some mapdata - you will need to install these
library(maps)
library(mapdata)

#how do these maps work...
states <- map_data("state")
head(states)
#basically, its a list of points that would draw a state border

#before we modify the data, let's try a few quick maps to get a hang of how the code works
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend

#look at the aesthetic binidings: as long as you have values that can inner_join, you are in flavor country

#and you can get subregions pretty easily
west_coast <- filter(states, region==c("california", "oregon", "washington"))

#graph the west_coast
ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

#go ahead and find your elections dataset
#write a filter that gets you the current number of electoral votes per state
#here is my favorite example, I wrote it during some wishful thinking once...
elector <- readr::read_csv("/Users/dunk/Classes/NMC 245/elector.csv")
elector2 <- inner_join(states, elector, by = c("region" = "Votes")) %>% rename(`State` = region)
?rename_with

colnames(elector2)[1] <- "long"

#now, lets try to sub-in our new data
ggplot(elector2) + 
  geom_polygon(aes(x = long, y = lat, fill = X4, group = group), color = "white") + 
  labs(x = "stupid", y ="name") +
  coord_fixed(1.3)

