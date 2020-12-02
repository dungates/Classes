library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(textdata)
library(ggrepel)

kanye <- readr::read_csv("/Users/dunk/Classes/NMC 245/kanye_unfiltered_lyrics.csv") %>% 
  mutate(track_number = 1) %>%
  dplyr::group_by(album_name) %>% dplyr::mutate(track_number = cumsum(track_number))

# kanye_lyrics <- kanye_lyrics %>% mutate(lyric_text = gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text)) %>%
#   mutate(lyric_text = gsub("\n", " ", lyric_text)) %>%
#   mutate(lyric_text = gsub("\\[.*?\\]", " ", lyric_text)) %>%
#   mutate(lyric_text = gsub(" {2,}", " ", lyric_text))
#subset the text 
new_kanye_lines <- kanye %>% unnest_tokens(Lines, lyric_text, token = "regex", 
                                           pattern = "\n", to_lower = F) # Make
#the length of your chosen data as a vector
new_kanye_lines <- new_kanye_lines %>% mutate(Lines = gsub("([a-z])([A-Z])", "\\1 \\2", Lines)) %>% # Space in front of capital letters
  mutate(Lines = gsub("\\[.*?\\]", " ", Lines))  %>% # Gets rid of verse and chorus tags
  mutate(Lines = gsub(" {2,}", " ", Lines))

new_kanye_lines <- data.frame(new_kanye_lines, "reference"=1:nrow(new_kanye_lines))

#word count per line
kanye_lines_counted <- new_kanye_lines %>%
  unnest_tokens(word, Lines) %>%
  dplyr::count(reference, word, sort = TRUE) %>%
  dplyr::rename(per_line = n) %>% 
  arrange(desc(per_line))

#get scores
afinn<-get_sentiments("afinn")

#attach the scores
with_scores <- kanye_lines_counted %>%
  inner_join(afinn, by="word")


#create a line score
scores_per_line <- with_scores %>%
  dplyr::group_by(reference) %>%
  #notice our per line strategy is SUM
  dplyr::summarize(line_value=sum(value), line_var=sd(value))


#attach your sentiments
kanye_sentiments <- inner_join(new_kanye_lines, scores_per_line, by="reference")

#visualize the albums
albums <- kanye_sentiments %>%
  dplyr::group_by(track_name)%>%
  #adds up the scores for each line, adds the track number, and then album name
  dplyr::summarize(song_value=sum(line_value), song_sd=sd(line_value),track=mean(track_number), album=album_name)%>%
  dplyr::ungroup()

#this is an example of varience

low<-c(1,-1,1,-1)
high<-c(5,-2,6,11)
sd(low)
sd(high)

#we can plot this
var<-data.frame(X=1:4, low, high)
#with linear method argument
ggplot(var, aes(X,high))+geom_smooth(method="lm")
#this is just some overfitting nonsense
ggplot(var, aes(X,high))+geom_smooth()


#removes dupes created by the album function
albums <- distinct(albums)
#take a look
View(albums)

#X is track number, Y is song value, color is album, this repel code makes it so the labels don't overlap when zoomed out

ggplot(albums, aes(track, song_value, colour=album)) + geom_label_repel(aes(label=track_name)) +
  facet_wrap( ~ factor(album, levels = c("The College Dropout", "Late Registration", "Graduation",
                                         "808s & Heartbreak", "My Beautiful Dark Twisted Fantasy",
                                         "Yeezus", "The Life Of Pablo", "ye"))) + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Track Number", y = "Song Value", title = "Kanye Albums Largely Negative According to AFINN",
       caption = "808s should definitely have a lower score, sentiment analysis doesn't work!!!") + 
  theme_bw() + theme(legend.position = "none",
                     strip.background = element_rect(fill = "slategray2", color = "white"),
                     plot.title = element_text(hjust = 0.5))

#now inclues song varience calcualtion
ggplot(albums, aes(track, song_value, colour=album))+
  geom_label_repel(aes(size=song_sd, label=track_name))

#explore the causes
kanye_sentiments %>%
  filter(track_name=="So Appalled")

#take a look inside the song
kanye_sentiments %>%
  filter(track_name=="So Appalled")%>%
  ggplot(aes(reference, line_value, colour=line_var))+geom_jitter()

# New analysis
library("tm")
library("wordcloud")
library("maptpx")
library("igraph")
library(maptpx)
library(LDAvis)
library(lda)

kanye_words <- kanye %>% mutate(lyric_text = gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text)) %>%
    mutate(lyric_text = gsub("\n", " ", lyric_text)) %>%
    mutate(lyric_text = gsub("\\[.*?\\]", " ", lyric_text)) %>%
    mutate(lyric_text = gsub(" {2,}", " ", lyric_text)) %>% 
  unnest_tokens(word, lyric_text, token = "words")

library(rsconnect)
library(dplyr)
library(knitr) # for dynamic reporting
library(stringr)
library(lubridate)
library(ggrepel)
library(wordcloud)
library(gridExtra) #viewing multiple plots together
library(tidyverse)
library(tidytext)
library(feather)
library(glue)
library(rprojroot)
library(purrr) #reduce and map functions
library(qdap)
library(ggplot2) #visualizations
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function



#METHOD 1: USING TIDY TEXT
SongSentiment_TidyText_NRC <- kanye_words %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  dplyr::group_by(album_name, sentiment) %>%
  dplyr::count(word, album_name, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% #consider top_n() from dplyr also
  ungroup()

`%mypipe%` <- magrittr::`%>%` # Some library fucked with pipes
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
  {
    theme(plot.title = element_text(hjust = 0.5), #Center the title
          axis.ticks = aticks, #Set axis ticks to on or off
          panel.grid.minor = pgminor, #Turn the minor grid lines on or off
          legend.title = lt, #Turn the legend title on or off
          legend.position = lp) #Turn the legend on or off
  }


#PLOT NRC SENTIMENT BY ARTST & ALBUM (ALL WORDS)
SongSentiment_TidyText_NRC %mypipe%
  #Set `y = 1` to just plot one variable and use word as the label
  ggplot(aes(word, 1, label = word, fill = sentiment)) +
  #You want the words, not the points
  geom_point(color = "transparent") +
  #Make sure the labels don't overlap
  geom_label_repel(force = 1,nudge_y = .5, nudge_x = .5,  
                   direction = "both",
                   box.padding = 0.04,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(album_name ~ sentiment) +
  theme_lyrics() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("NRC Sentiment by Album") +
  coord_flip()









