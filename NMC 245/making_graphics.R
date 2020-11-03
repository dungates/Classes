#Explore Our Dataset
TV <- read_csv("/Users/dunk/NMC 245/TV.csv")
dim(TV)

#where do we store our cases?
TV[,]

#where do we store our variables?
TV[,3]

head(TV)

#prove it yourself with NYCFlights
dim(flights)
flights[20000,4]

#lets expore some basic filtering
val<-filter(flights, month==2 & day==14)

library(ggplot2)
qplot(val$dep_time, val$dep_delay)

#what is the relationship between departure time and delay?
cor.test(val$dep_time, val$dep_delay)

#is it stonger if we look at 
cor.test(val$arr_time, val$arr_delay)

#play around - what seems reasonable...

#perhaps we can learn more if we make a plot
#so the first thing is to call our data

ggplot(val)

#why was that blank?

ggplot(val, aes(dep_time, dep_delay))

#what changed?

ggplot(val, aes(dep_time, dep_delay))+geom_point()

#and lets take the next step... add color
ggplot(val, aes(dep_time, dep_delay, colour=carrier))+geom_point()

#and one more element, what if we want a few separate graphs
ggplot(val, aes(dep_time, dep_delay, colour=carrier))+geom_point()+facet_grid(~origin)

#now lets add labels and stuff
ggplot(val, aes(dep_time, dep_delay, colour=carrier))+geom_point()+facet_grid(~origin)+
  ggtitle("Departures and Delays around NYC", "or why is JFK a mess?")+xlab("When did the flight take off?")+ylab("How long did you sit?")

#do you like to party? Let's expand beyond Valentine's Day...
#now lets add labels and stuff
ggplot(flights, aes(dep_time, dep_delay, colour=carrier))+geom_point()+facet_grid(~origin)+
  ggtitle("Departures and Delays around NYC", "or why is JFK a mess?")+xlab("When did the flight take off?")+ylab("How long did you sit?")

#Let's learn a little more about these flights that are really delayed
View(val)

#perhaps we should check the weather...

#Let's Check the Cheetsheet
#let's try a discrete continuous
#I'll keep using val because I like speed
ggplot(val, aes(carrier, dep_delay))+geom_boxplot()

#if you want to know more...
nycflights13::airlines
#or just
airlines

#and again, if you want to party...
ggplot(flights, aes(carrier, dep_delay)) + geom_violin()

#let's play with some other options...

#ok, shifting gears, go ahead and load that TV dataset from github...

#what is TV like? how big, how many dimensions
#what is continuous, what is discrete

#telling stories with
ggplot(TV, aes(Year, Rating, colour=Type))+geom_jitter()

#what could tell the story by type with beauty by network?
ggplot(TV, aes(Year, Rating, colour=Type))+geom_jitter()+facet_grid(~Network)

#another powerful graphic
ggplot(TV, aes(Network, Rating))+geom_boxplot()
with(TV, Program[Rating == min(Rating)]) # Worst programs of all time are Frasier and the King of Queens

#what eles can we do here, what if Fall 1990-Spring 2003 is too long a time frame...
TV %>%
  filter(Year <= 2003 & Year >= 2001)
flights %>% 
corr_cross(flights, max_pvalue = 0.05, top = 10, ignore = c("day","month","hour")) # exclude characters to run this?

#this is where it will get scary (but for like a week)...
Leaders<-TV %>%
  group_by(Year, Network) %>%
  summarize("Z"=mean(Rating))

View(Leaders)

#another way to look at it: Fox America's No 1 Network?
ggplot(Leaders, aes(Year, Z, colour=Network))+geom_point() + geom_smooth(method = "lm")

#Now start thinking about your homework...

