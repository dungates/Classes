today <- readr::read_csv("/Users/dunk/Downloads/music.csv")

features<-textfeatures(today$lyric, sentiment=FALSE)
featured_today<-bind_cols(today, features)

library(ggplot2)
ggplot(featured_today, aes(n_uq_words, n_prepositions, size=n_commas, colour=album_name))+geom_jitter() + theme(legend.position = "none")

#we can run a few tests 
cor.test(featured_today$n_uq_words, featured_today$n_prepositions)
cor.test(featured_today$n_uq_words, featured_today$n_commas)

#what can we infer....
library(tidyverse)
library(tidytext)
#let's do some counting...
today_words <- today %>%
  unnest_tokens(word, Text) %>%
  count(Source, word, sort = TRUE)

total_words <- today_words %>% 
  group_by(Source) %>% 
  summarize(total = sum(n))

today_words <- inner_join(today_words, total_words, by="Source")

beep<-today_words%>%
  mutate(index=n/total)

ggplot(today_words, aes(n, n/total, colour=Source))+geom_text(aes(label=word), check_overlap = TRUE)+facet_wrap(~Source)


library(tidytext)
today_bigrams<-today%>%
  #name the colmn, where do you get the text from, what token type is it, how many in the ngram
  unnest_tokens(bigram, Text, token="ngrams", n=2)


#this is when we just do normal tidy stufff
today_bigrams%>%
  count(bigram, sort=TRUE)

#this is a godo chance ot check for negations
today_sep<-today_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#we can remove what are called stopwords
today_nostop<- today_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

View(today_nostop)

#we then can flag and mange negations, qualifiers, and modifiers
today_nostop %>%
  filter(word1 == "partially" | word1=="falsely" | word1== "no" | word1 == "never") %>%
  count(word1, word2, sort = TRUE)

#Zipf's law
#term frequency
freq_by_rank <- today_words %>% 
  group_by(Source) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

#fairly clear plotting - notice the early truncation of the distrivution
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Source)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

#and because we like to party
library(network)
library(ggnetwork)

edgelist<-select(today_nostop, c(word1,word2))
news<-network(edgelist, directed = FALSE)

#detect the modularities
pulse<-as.matrix.network.adjacency(news)
louvain_net<-modMax::greedy(pulse) 

#grab the detected modularities from 
news %v% "louvain" <- louvain_net[3]

#lay it out
plot<-ggnetwork(news, layout="fruchtermanreingold")

#now render that bad boy
GGally::ggnet2(plot, label = T, label.size = 3, arrow.size = 3, arrow.gap = 0.02, alpha = 0.5)
ggplot(plot, aes(x, y, xend = xend, yend = yend)) +
  #just make the lines orange
  geom_edges(aes(color="pink"))+
  #set the other aesthetics as static values
  geom_nodetext(aes(colour = as.factor(louvain), label = vertex.names))+
  theme_blank()
















