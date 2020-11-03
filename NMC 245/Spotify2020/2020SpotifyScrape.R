ls()
rm(list = ls())
library(rvest)
library(stringr)
library(tidyverse)
library(tidyr)
library(reshape2)
library(magrittr)
library(scales)
library(lubridate)


# Spotify scraping
url <- "https://spotifycharts.com/regional/us/daily/"

timevalues <- seq(as.Date("2020/01/01"), as.Date("2020/10/27"), by = "day")

head(timevalues);tail(timevalues)

# Function to fix constant, sequence of dates
concat.url<- function(x){
  full_url <- paste0(url, x)
  full_url
}
# Run the function
finalurl <- concat.url(timevalues)
head(finalurl, n=2);tail(finalurl, n=2)

# Function that reads HTML and extracts HTML nodes
SpotifyScrape <- function(x){
  page <- x
  rank <- page %>%
    read_html() %>% #Reads an HTML page
    html_nodes('.chart-table-position') %>% #RVEST.PKG: extract pieces out of HTML docs. using XPath & css selectors.
    html_text() %>% #RVEST.PKG:Extract attributes, text and tag name from html
    as.data.frame()
  track <- page %>% 
    read_html() %>% 
    html_nodes('strong') %>% 
    html_text() %>% 
    as.data.frame()
  artist <- page %>% 
    read_html() %>% 
    html_nodes('.chart-table-track span') %>% 
    html_text() %>% 
    as.data.frame()
  streams <- page %>% 
    read_html() %>% 
    html_nodes('td.chart-table-streams') %>% 
    html_text() %>% 
    as.data.frame()
  dates <- page %>% 
    read_html() %>% 
    html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>%
    html_text() %>% 
    as.data.frame()
  
  #combine, name, and make it a tibble
  chart <- cbind(rank, track, artist, streams, dates) #Combine R Objects by Columns
  names(chart) <- c("Rank", "Track", "Artist", "Streams", "Date") #Functions to get or set the names of an object
  chart <- as.tibble(chart)#TIBBLE.PKG:turns an existing object into a so-called tibble
  return(chart) #Final tibble 5 columns & (200 rows * 365 days) = 73,000
}

# Save as an RDS after mapping the dataframe
spotify <- map_df(finalurl, purrr::possibly(SpotifyScrape, NULL)) #PURR.PGK:The map functions transform their input by applying a function to each element and returning a vector the same length as the input.
saveRDS(spotify, "/Users/dunk/Classes/NMC 245/Spotify2020/spotifyData.rds")


spotify <- readRDS("/Users/dunk/Classes/NMC 245/Spotify2020/spotifyData.rds")
dim(spotify)

head(spotify)

# Some quick data cleaning
spotify %<>% 
  mutate( Artist = gsub("by ", "", Artist), #gsub perform replacement of the first and all matches respectively
          Streams = gsub(",", "", Streams), 
          Streams = as.numeric(Streams), 
          Date = as.Date(spotify$Date, "%m/%d/%Y"),
          WeekDay = wday(Date, label = TRUE),#LUBRIDATE.PKG:Get days component of a date-time
          Month = month(Date, label = TRUE)
  ) %>% 
  print()

# Find top 20 most streamed tracks
by_streams <- spotify %>% 
  group_by(Track) %>%
  summarise(TotalStreams = sum(Streams)) %>% 
  arrange(desc(TotalStreams)) %>%
  top_n(20) 

theme_set(theme_classic())
by_streams %>%
  ggplot(aes(reorder(Track, TotalStreams), y = TotalStreams, color = TotalStreams)) +
  geom_col(fill = "tomato2") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_continuous(type = "viridis") +
  coord_flip() +
  #geom_label_repel(aes(label = total), size = 3) +
  labs(title = 'US 2020 | Most Streamed Songs',
       x = "Track Name",
       y = "Total Streams") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 0.6),
        legend.position = "none")

# Plot of top 20 artists
by_artist <- spotify %>% 
  group_by(Artist) %>%
  summarise(TotalStreams = sum(Streams)) %>% 
  arrange(desc(TotalStreams)) %>% 
  top_n(20)
library(leaflet)
pal <- colorNumeric(
  palette = "Blues",
  domain = by_artist$TotalStreams)

by_artist %>%
  ggplot(aes(reorder(Artist, TotalStreams), y = TotalStreams, fill = TotalStreams)) +
  geom_col() + scale_fill_continuous(type = "gradient") + scale_y_continuous(labels = scales::comma) +
  #geom_label_repel(aes(label = TotalStreams), size = 3) +
  coord_flip() +
  labs(title = 'US 2020 | Most Streamed Artist So Far',
       x = "Artist Name",
       y = "Total Streams") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 0.6),
        legend.position = "none")

# Most streamed day of the week
by_WeekDay <- spotify %>% 
  group_by(WeekDay) %>%
  summarise(TotalStreams = sum(Streams)) %>% 
  arrange(desc(TotalStreams)) %>% 
  print()

ggplot(data=by_WeekDay, aes(x=WeekDay, y=TotalStreams, group=1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'US 2020 | Most Streamed Day of the Week | Friday Reached ~ 4 Billion', #3,928,599,112
       x = "Day of the Week",
       y = "Total Streams") + theme(plot.title = element_text(hjust = 0.5))

# Most streamed month
by_Month <- spotify %>% 
  group_by(Month) %>%
  summarise(TotalStreams = sum(Streams)) %>% 
  arrange(desc(TotalStreams)) %>% 
  print()

ggplot(data=by_Month, aes(x=Month, y=TotalStreams, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'US 2020 | Most streamed Month | This Data is Wack!!!', # 2,718,329,831
       x = "Month",
       y = "Total Streams")

#Group by track and sum Total Streams
by_streams2 <- spotify %>% 
  group_by(Track) %>%
  summarise(TotalStreams = sum(Streams)) %>% 
  arrange(desc(TotalStreams)) %>%
  top_n(100)

#Create a df with unique tracks and artists
spotify2 <- spotify %>% 
  select(Track, Artist) %>% 
  distinct(Track, Artist)

#Left join to prep our data and get the lyrics
top100songs <- left_join(by_streams2, spotify2, by = "Track") %>% 
  arrange(desc(TotalStreams)) %>% 
  select(Artist, Track, TotalStreams) %>%
  filter (! duplicated(TotalStreams)) %>% 
  print()

saveRDS(top100songs, file = "/Users/dunk/Classes/NMC 245/Spotify2020/top100.rds")

write_csv(spotify, "/Users/dunk/Classes/NMC 245/Spotify2020Example.csv")

