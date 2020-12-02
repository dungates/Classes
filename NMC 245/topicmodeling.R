#simplified topic model
library(topicmodels)

#standard tidy text analysis library
library(tidytext)

#helpful manipulation tools
library(dplyr)

#charts and graphs and stuff
library(ggplot2)

#various tidy commands
library(tidyr)

#this works with the today data

#STEP ONE: standard addition of document reference number
today <- readr::read_csv("/Users/dunk/Classes/NMC 245/Cultural20/unit_three/today.csv")
reference<-1:dim(today)[1]
today2 <- today%>%
  mutate(reference = reference)

#STEP TWO - count stuff
today3<-today2 %>%
  unnest_tokens(word, Text) %>%
  count(reference, word, sort = TRUE)%>%
  rename(per_line = n)

#STEP THREE - convert data structure from TIDY to DOCUMENT TERM MATRIX
today_dtm<-today3%>%
  cast_dtm(reference, word, per_line)

#STEP FOUR - run LDA
#this version of LDA does not allow control of alpha or beta assumptions
#these control the rate at which the model changes during the learning cycles
today_lda <- LDA(today_dtm, k = 3, control = list(seed = 1234))

#STEP 5 - look at beta values - word distribution per topic
today_topics <- tidy(today_lda, matrix = "beta")

#5B get the top ten per topic
today_top_terms <- today_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#5C visualize those words
today_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#STEP 6: look at the topic to topic spread (assumes a two topic comp)
beta_spread <- today_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#6B visualize that
beta_spread%>%
  filter(topic2/topic1 > 5)%>%
  ggplot(aes(topic2/topic1, term, colour=topic3))+geom_jitter()

#STEP 7 - assign topic guess values...
bob<-today_documents <- tidy(today_lda, matrix = "gamma")

#7B plot the results 
ggplot(bob, aes(document, topic, colour=gamma, size=gamma))+geom_point()

