# install.packages("babynames")
library(readr)
library(babynames)
library(dplyr)
# Reading in old data
data(babynames)
# Reading in new data
boys18 <- read_csv("/Users/dunk/Classes/NMC 245/boys_18.csv")
boys19 <- read_csv("/Users/dunk/Classes/NMC 245/boys_19.csv")
girls18 <- read_csv("/Users/dunk/Classes/NMC 245/girls_18.csv")
girls19 <- read_csv("/Users/dunk/Classes/NMC 245/girls_19.csv")

# Adding year, sex, and proportion columns
boys18 <- boys18 %>% mutate(year = 2018, sex = "M", prop = n/sum(n))
boys19 <- boys19 %>% mutate(year = 2019, sex = "M", prop = n/sum(n))
girls18 <- girls18 %>% mutate(year = 2018, sex = "F", prop = n/sum(n))
girls19 <- girls19 %>% mutate(year = 2019, sex = "F", prop = n/sum(n))

# Binding all new data together
newdf <- do.call("rbind", list(boys18, boys19, girls18, girls19))

# Joining dataframes together
lit <- babynames %>% full_join(newdf)



test <- lit %>% group_by(year, sex) %>% top_n(n=5)

test <- lit %>% dplyr::filter(name == "Olivia" | name == "Linda" | name == "Jennifer")
scale_y_continuous(labels = scales::dollar, 
                   sec.axis = sec_axis(~.*coeff, name = "Consumer Loans ($)", labels = scales::dollar))
ggplot(data = test, aes(as.Date(year), n, color = name)) + geom_line(lwd = 1) + 
  scale_color_manual(values = c("firebrick1", "dodgerblue", "forestgreen")) + 
  scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~(.)/sum(.), name = "Proportion", labels = scales::percent)) +
  theme_bw() + 
  labs(y = "Number", x = "Year", color = "Name")



baby_ranks<- lit %>%
  group_by(year, sex)%>%
  mutate(rank=dense_rank(1/n))

baby_ranks %>% filter(rank <= 5) %>% ggplot(aes(x = year, y = n, color = name)) + geom_line(lwd = 1) + facet_wrap(~ rank, scales = "free") +
  labs(x = "Year", y = "Number of Occurrences", color = "Name") + theme_bw() + ggtitle("Top 5 Ranks of Baby Names") 

baby_ranks %>% filter(rank <= 10) %>% ggplot(aes(x = year, y = prop, color = sex)) + geom_line(lwd = 1) + 
  scale_color_manual(values = c("darkorange2", "black")) + theme_bw() + labs(x = "Year", y = "Proportion", color = "Sex") + 
  ggtitle("Name Conformity Over Time") + geom_text(aes(label = "Go Beavs!", x = 2000, y = 0.08), color = "magenta4") +
  theme(plot.title = element_text(hjust = 0.5))

