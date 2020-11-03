library(tidyr)
library(dplyr)
library(ggplot2)

#TV example
#YOU GOTTA WRITE THIS LINE 8:JOIN RATINGS4 and PROGS; 
#Be sure to remove duplicates from progs first
progs<-distinct(progs)
TV<-inner_join(Ratings4, progs)

#filters and joins

#super popular
TV%>%
  filter(Rating>20)

TV%>%
  filter(Rank > 5 & Rank < 7)

TV %>%
  top_frac(20)

#all boolean operators are possible
TV%>%
  filter(Rank == 4 | Rank == 5)

TV%>%
  filter(Rank==4 & Network == "ABC")


#Summary functions
TV%>%
  group_by(Type)%>%
  summarize(Ratings = mean(Rating, na.rm=TRUE), Deviation = var(Rating, na.rm=TRUE))

#windowing functions - notice I pass a 1/ to get the rankings to go descending
ABS<-TV%>%
  mutate("Absolute" = dense_rank(1/Rating))
View(ABS)

#between function 
between<-TV%>%
  mutate(Beyonce=between(Rating, 10, 11))

head(between)

#rename that
between %>%
  rename("Sweet Spot" = Beyonce)

#you can do basically any function
TV%>%
  mutate(cumprod("Viewership"=Rating*1.21))

#mutate is great for cooking new columns   

#we have focused on joins
#anti_join() is a powerful cleaning tool if you are working with inner_join()

#really easy, bind_rows and bind_cols

#tidy formatting - espeically separations

#cleaning - removing unwanted materials, exporting for spell check
write.csv(dataframe, "name your file.csv", row.names = FALSE)

#elite level - REGEX
str_extract(elect$State, "^Al")
str_extract(elect$State, "^Mi")
str_extract(elect$State, "ta$")
sum(str_count(elect$State, "ta$"))

#you can replace thigns tht arent working for you too
str_replace(elect$State, "^North", "")
str_replace(elect$State, "o$", "os")

#what if we want to make this longer 
elect_long<-pivot_longer(elect, -c(Joined, State), 
                         values_to = "Votes", names_to="Years", )

View(elect_long)


#now we can easily make a graph

ggplot(elect_long, aes(Joined, Votes))+geom_jitter()
View(elect_long)

ggplot(elect_long, aes(Years, Votes))+geom_jitter()

solort<-separate(elect_long, Years, into = "Elections", sep=".", fill="warn")

library(stringr)
range<-str_split(elect_long$Years, "[[:punct:]]+")
ranges<-data.frame(range)
View(longish)

longish<-separate(elect_long, col=Years, into = c("start","end"), sep = "[[:punct:]]+")


start<-str_replace(longish$start, "X", " ")

real_long<-longish%>%
  select(Joined, State, end, Votes)%>%
  mutate("start"=as.numeric(start))

real_long<-pivot_longer(longish, col=-c(Joined, State, Votes))
View(real_long)

year<-str_replace(real_long$value, "X", " ")
easyplot<-real_long%>%
  select(Joined, Votes, State, name)%>%
  mutate("Election"=as.numeric(year))%>%
  drop_na(Votes)%>%
  drop_na(Election)


ggplot(easyplot, aes(Election, Votes))+geom_jitter()

long_bea<-pivot_longer(bea_regions, col=-State )

bea_long<-select(long_bea, -State)%>%
  rename("Region"=name)%>%
  rename("State"=value)

#oh no extra values, just filter them out

wut<-bea_long%>%
  filter(State != "")

anti_join(elect, wut)

regions<-inner_join(easyplot, bea_long)

ggplot(regions, aes(Election, Votes, colour=Region, size=Votes))+geom_jitter()

regions%>%
  filter(State=="Missouri")%>%
  ggplot(aes(Election, Votes, colour=Region, size=Votes))+geom_jitter()

#where is the most power
ggplot(regions, aes(Election, Votes, colour=Region, size=(Votes/mean(Votes))))+geom_jitter()

regions%>%
  filter(Region=="Mideast" | Region == "Great.Lakes")

regions%>%
  filter(State== "New York" | State == "Florida")%>%
  ggplot(aes(Election, Votes, colour=Region, size=Votes))+geom_jitter()

Viewing<-select(TV, Program, Network, Type)
B<-pivot_wider(Viewing, names_from = Type, values_from = Program)
View(B)
