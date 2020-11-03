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
USA <-read_html("https://en.wikipedia.org/wiki/Terrorism_in_the_United_States")

USA_Terrorism2010 <-USA %>%
  #I used escape keying on the quotes
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[11]')%>%
  html_table()

USA_Terrorism2000 <- USA %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[10]') %>%
  html_table()

TwentyFirstCenturyTerrorism <- rbind(USA_Terrorism2000, USA_Terrorism2010)
# 10, 38, 71, 81 are extended dates, write manually
TwentyFirstCenturyTerrorism <- TwentyFirstCenturyTerrorism %>% mutate(Date = mdy(Date)) %>% 
  select(-`#`)
TwentyFirstCenturyTerrorism[10,1] <- "2002-10-24"
TwentyFirstCenturyTerrorism[38,1] <- "2010-11-02"
TwentyFirstCenturyTerrorism[71,1] <- "2016-09-19"
TwentyFirstCenturyTerrorism[81,1] <- "2018-11-01"

TwentyFirstCenturyTerrorism <- TwentyFirstCenturyTerrorism %>% mutate(Dead = gsub("\\(.*", "", Dead), 
                                                                      Injured = gsub("\\(.*", "", Injured),
                                                                      Injured = gsub("\\+.*", "", Injured)) 
TwentyFirstCenturyTerrorism$Dead <- as.numeric(gsub(",", "", TwentyFirstCenturyTerrorism$Dead))
TwentyFirstCenturyTerrorism$Injured <- as.numeric(gsub(",", "", TwentyFirstCenturyTerrorism$Injured))
TwentyFirstCenturyTerrorism$Image <- NA
TwentyFirstCenturyTerrorism[4,8] <- "https://cdn.theatlantic.com/thumbor/kSTukUk_Bv1a2-8CyVHHgtrbDeM=/1200x869/media/img/photo/2011/09/911-the-day-of-the-attacks/a03_0RTRMNQW/original.jpg"

library(ggimage)
ggplot(data = TwentyFirstCenturyTerrorism, aes(x = Date, y = Dead)) + geom_line() + ggtitle("9/11 bad") + 
  geom_image(aes(image = Image), size = 0.3)


readr::write_csv(TwentyFirstCenturyTerrorism, "/Users/dunk/Classes/NMC 245/USTerrorism.csv")
