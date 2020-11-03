library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(reshape2)
library(magrittr)

#this is how you load a website
#read_html is the function from library xml2 - so you dont need to get that separately
#if you are reading some other tutorial

#scrape a wikitable
Russia<-read_html("https://en.wikipedia.org/wiki/Federal_subjects_of_Russia")

Russian_States<-Russia %>%
  #I used escape keying on the quotes
  html_node("#mw-content-text > div.mw-parser-output > table:nth-child(21)")%>%
  html_table() 

#I'm going to put you in breakout rooms to do this again with another wikipedia page...

story<-read_html("https://www.nytimes.com/2020/10/22/us/politics/trump-campaign-money.html")

#using a CSS selector
story%>%
  #this is a style based selector
  html_node(css = "#story > section > div:nth-child(1) > div > p:nth-child(1)")%>%
  html_text()

#using xpath
story%>%
  #I used escape keying on the quotes
  html_node(xpath="//*[@id=\"story\"]/section")%>%
  html_text()


story%>%
  #I used escape keying on the quotes
  html_node(xpath="//*[@id=\"story\"]/section/div[1]/div/p[2]/text()")%>%
  html_text()



#Let's grab that whole story
Trump<-story%>%
  #I used escape keying on the quotes
  html_node(css = "#story > section")%>%
  html_text()
halloween<-read_html("https://www.nytimes.com/2020/10/22/arts/celebrate-halloween-horror-movies.html")
Spooky<-halloween%>%
  #I used escape keying on the quotes
  html_node(css = "#story > section")%>%
  html_text()

#scrape a fancier looking website
#take a look at this https://rpubs.com/Mayank7j_2020/billboard_2020
#the key detail here is scraping an XML node rather than an HTML node
billboard<-read_html("https://www.billboard.com/charts/hot-100/2000-10-28")

billboard %>%
  html_node("body")%>%
  #for when it is fancy
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__artist')]")%>%
  rvest::html_text()

billboard%>%
  html_node("body")%>%
  xml_find_all("//span[contains(@class, 'chart-element__information__artist')]")%>%
  html_text()

billboard%>%
  html_node("body")%>%
  xml_find_all("//span[contains(@class, 'chart-element__rank__number')]")%>%
  html_text()

#lets get some more...

#scraping some headlines
google <- read_html("https://news.google.com/")
vehicle_all <- google %>% 
  html_nodes("body > c-wiz > div > div.FVeGwb.CVnAc.Haq2Hf.bWfURe > div.ajwQHc.BL5WZb.RELBvb.zLBZs > div > main") %>% 
  html_text()

View(vehicle_all)
news<-str_split(vehicle_all, "agobookmark_bordersharemore_vert")

melted_news<-melt(news)

newsy_news<-str_split(melted_news$value, "bookmark_bordersharemore_vert")
huey_lewis<-melt(newsy_news)

sprocket<-str_split(melted_news$value, "...ampvideo_youtube")
toad<-melt(sprocket)
View(toad)

#now cleaning up that times column
thenews<-str_split(toad$value, regex("[:digit:]+ hours", multiline = TRUE))
timez<-melt(thenews)
View(timez)
