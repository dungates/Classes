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
library(ggrepel)

# Spotify scraping
url <- "https://spotifycharts.com/regional/us/daily/"

timevalues <- seq(as.Date("2012/01/01"), as.Date("2012/12/31"), by = "day")

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




