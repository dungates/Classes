#to start, here are the libraries called in today's code
library(ggplot2)
library(dplyr)
library(lubridate)
library(nycflights13)
library(ggthemes)
library(viridis)
library(wesanderson)

#shapes and geoms - telling the stories of delay with geoms - we can mess with and fix this...
ggplot(flights, aes(time_hour, distance, colour=carrier))+geom_jitter()+facet_grid(~origin)

#tell the story with a smooth - big aggregation and simplification
ggplot(flights, aes(time_hour, dep_delay))+geom_smooth()

#manual control of the geoms
ggplot(flights, aes(time_hour, dep_delay, color=carrier)) +
  geom_line(linetype="dotted", lineend = "butt", color="green", size=2)+
  geom_point(color="blue", size=3)

#welcome to the deep dive into uglyness, but like, educational uglyness
#what else can you do with linetype?
??geom_line()

#let's get aggressively ugly here with both the points and the lines....


#ugly again wow
ggplot(flights, aes(time_hour, dep_delay, group=carrier)) +
  geom_line(linetype="dotted", color="green", size=2)+
  geom_point(flights, mapping = aes(color=carrier, size=3))


ggplot(flights, aes(time_hour, dep_delay, group=origin)) +
  geom_line(flights, mapping = aes(linetype=origin, color="green", size=2))+
  geom_point(flights, mapping = aes(color=carrier, size=3))


#now this will be really ugly. 
ggplot(flights, aes(time_hour, dep_delay))+geom_smooth(mapping = aes(flights$time_hour, flights$dep_delay, linetype = flights$carrier))

#less of a dive into the ugly
ggplot(flights, aes(time_hour, hour, colour=mean(dep_delay)))+geom_bin2d(bins=10)+facet_wrap(~carrier)

#aggregation
carrier_flights<-flights%>%
  group_by(yday(time_hour))%>%
  count(carrier)

colnames(carrier_flights)[1]<-"day"

ggplot(carrier_flights)+
  geom_bar(mapping = aes(x = day, fill=carrier))+
  scale_fill_brewer(palette = "Greens")+
  coord_polar()

#diamonds
ggplot(diamonds)+
  geom_bar(mapping = aes(x = price, fill=cut))+
  scale_fill_brewer(palette = "Greens")+
  coord_polar()

#density is a great subin for mode
plot(density(diamonds$price))
plot(density(flights$dep_delay, na.rm=TRUE))
median(diamonds$price)
IQR(diamonds$price)
summary(diamonds$price)
fivenum(diamonds$price)

#let's sub into color control more

#to make our lives easier, and our code faster, lets make a subset
#we will use all the St. Patrick's Day Flights
patty<-flights%>%
  filter(month == 3 & day == 17)

ggplot(patty, aes(time_hour, dep_delay))+geom_point()

#lets just force it to use discrete colors
ggplot(patty, aes(time_hour, dep_delay, color=carrier))+geom_point()+scale_colour_brewer(palette = "Set1")

#notice in this code we have shifted control of our aesthetics entire into the geom
ggplot(patty, aes(time_hour, dep_delay, group=carrier)) +
  geom_line(aes(linetype="dotted", color=carrier, size=2))+
  geom_point(aes(color=carrier, size=3))+
  scale_colour_brewer(palette = "Blues") 


#there are two things happening here... this is discrete discrete so we need to keep the points from overlapping
ggplot(patty, aes(carrier, origin))+geom_point()

#why we jitter
ggplot(patty, aes(carrier, origin))+geom_jitter()

#discrete discrete discrete
ggplot(patty, aes(carrier, origin))+
  geom_jitter(patty, mapping=aes(color=dest))+
  scale_colour_brewer(palette = "Set2") +
  theme(legend.position = "none")

#now lets manipulate the color there
ggplot(patty, aes(carrier, origin, color=dep_delay))+
  scale_fill_gradient()+
  geom_jitter()+
  scale_colour_gradient2_tableau(palette = "Orange-Blue Diverging", na.value = "grey50", guide = "colourbar")

ggplot(patty, aes(carrier, origin, color=dep_delay))+
  geom_jitter()+
  scale_colour_gradient2()

#now some TV story telling
setwd("/Users/dunk/NMC 245")
TV <- read_csv("TV.csv")
ggplot(TV, aes(Year, Rating, colour=Type))+geom_point()+geom_smooth()+scale_color_economist()

#with some intense colors
ggplot(TV, aes(Year, Rating))+
  geom_jitter(aes(color = Rating))+
  scale_color_viridis(discrete = FALSE, option = "D")+
  scale_fill_viridis(discrete = FALSE)

#its groovy baby
ggplot(TV,aes(Year, Rating, colour=Network))+
  geom_point(size=6)+ scale_color_brewer(palette = "PuOr")

#because we are playing with geoms...
ggplot(TV, aes(Year, Rank, fill = Rating)) +
  geom_tile()+scale_fill_continuous(type = "viridis")

#or from some expensive trainning wheels like tableau
ggplot(TV, aes(Year, Rank, color = Rating)) +
  geom_jitter()+scale_colour_gradient2_tableau(palette = "Orange-Blue Diverging", na.value = "grey50", guide = "colourbar")

#or from a movie like Royal Tennenbaums
ggplot(TV, aes(Year, Rank, colour=Rating)) +
  geom_jitter(aes(color = Rating))+scale_colour_gradientn(colours=wes_palette("Royal1", type=c("continuous")))

#or if you like to party, manually define the colors
ggplot(TV, aes(Year, Rank, colour=Rating)) +
  geom_jitter(aes(color = Rating))+scale_color_gradient2(low = "white", mid = "red", high="purple")

#make it look like your fav wevbsite
ggplot(TV, aes(Year, Rank, color = Type))+geom_jitter()+theme_fivethirtyeight()+scale_color_fivethirtyeight()
