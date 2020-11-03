#join parts of NYC Flights 13

library(nycflights13)
library(dplyr)
library(ggplot2)
#so much of our fun here involves Dan just knowing stuff, that's like, not great
#dan has wasted much of his live memorizing aviation trivia

#whatelse is in NYC flights?
#find the package help
View(flights)
#so a few things? Let's exlpore the structure
long_wait<-flights%>%
  filter(dep_delay > 180)

#lets see who's fault it really is
fault<-inner_join(long_wait, airlines)
#oh thats hard to read, lets really get this tight
faulty<-fault%>%
  #like some useful categories
  select(dep_delay, origin, dest, name)%>%
  arrange(dep_delay)

View(faulty)

#so let's learn more. Why not. 
#can we like, see where all those planes were going?
inner_join(faulty, airports)

View(airports)

#It didn't work because the key was misnamed
#the following procedure is dangerous, only do it if you like to party
colnames(airports)[1]<-"dest"

#dang - but wait, look at the report it threw - THERE WERE TWO KEYS
inner_join(faulty, airports)

crummy<-inner_join(faulty, airports, by = "dest")
View(crummy)

#now we go through the looking glass to make a map of the united states from raw data
ggplot(crummy, aes(lon, lat, colour=name.x))+geom_point()

#wait wut, why don't we like party hardy
#since we already changed airports, we could
boom<-inner_join(flights, airports, by = "dest")
ggplot(boom, aes(lon, lat, colour=origin))+geom_point()

#why is it just North America? The code will tell you...

#for really delayed flights, what kinds of planes are we talking about...
#what is the key...
View(flights)
vroom<-inner_join(flights, planes, by = "tailnum")

#what happened? 
wut<-anti_join(flights, planes)
View(wut)
#so 3200 planes are responsible for 
dim(vroom)[1]/dim(flights)[1]

#84% of the flights are done by like a handful of planes

#so what day of the year had the most delays?
library(lubridate)
bad_days<-flights%>%
  group_by(date(time_hour))%>%
  summarize(pain=mean(dep_delay, na.rm=TRUE))

View(bad_days)

#so March 8, how was the weather that day...
View(weather)
frontz<-weather%>%
  filter(month==3 & day==8)

march<-inner_join(flights, frontz)
View(march)

ggplot(march, aes(time_hour, wind_gust, colour=dep_delay))+geom_point()

#OK its game go...
spots<-inner_join(flights, weather)

windsock<-spots%>%
  group_by(date(time_hour))%>%
  summarize(pain=mean(dep_delay, na.rm=TRUE), mean(wind_speed))

View(windsock)

#Now let's join some new fun stuff
#TV and Programs

#lets layout some trends
#programs over the years
#programs by network
#looking for variation

#these are filtering tasks, we also want to do selecting tasks


#your assignment: build your own dataset that requires a join. Submit three files - the complete dataset and two others
#the core and then a dataset that modifies it - let's push this back until Tuesday? 