library(tidyverse)
library(wordcloud2)
library(googlesheets4)
rm(list=ls())
setwd("~/Documents/DavidBellProject")

s <- read_sheet("https://docs.google.com/spreadsheets/d/10bntXIZk2xCw7DyDWO34Uw02NGWsCCSvhshniyCsv7M/edit#gid=0",
                col_names = c("Game","Date","Player","Word","Score","Bingo","Bonus"),
                range = "A2:G")

words <- s %>% count(Player, Word, sort = T) %>%
  filter(!is.na(Word))


#### GOSIA ####
words_gosia <- words %>% 
  filter(Player =="gosia") %>%
  select(-Player) %>%
  filter(n >1)

set.seed(1234)
wordcloud2(words_gosia,
           size = 1.5,
           rotateRatio = 0,
           gridSize = 1,
           fontFamily = "Georgia",
           fontWeight = "normal",
           color = "black",
           backgroundColor = "white",
           figPath = "./scrabble-G.jpg")

set.seed(1234)
wordcloud2(words_gosia,
           size = 2.1,
           ellipticity = 1,
           rotateRatio = 0,
           gridSize = 1,
           fontFamily = "Georgia",
           fontWeight = "normal",
           color = "black",
           backgroundColor = "white",
           shape = "circle")

#### DAVID ####
words_david <- words %>% 
  filter(Player =="david") %>%
  select(-Player) %>%
  filter(n>1)
set.seed(1234)
wordcloud2(words_david,
           size = 1.5,
           # minRotation = 0,
           # maxRotation = 0,
           rotateRatio = 0,
           gridSize = 1,
           fontFamily = "Georgia",
           fontWeight = "normal",
           color = "black",
           backgroundColor = "white",
           figPath = "./scrabble-D.jpg")

set.seed(1234)
wordcloud2(words_david,
           size = 2.1,
           ellipticity = 1,
           rotateRatio = 0,
           gridSize = 1,
           fontFamily = "Georgia",
           fontWeight = "normal",
           color = "black",
           backgroundColor = "white",
           shape = "circle")


##### BEST WORD SCORE #####
s %>% group_by(Player) %>%
  slice(which.max(Score)) %>%
  select(Game, Player, Word, Score)


s %>% filter(!is.na(Word)) %>% group_by(Player) %>%
  summarise(total_words = n(),
            words_per_game = n()/n_distinct(Game),
            score_per_word = mean(Score),
            score_match = sum(Score)/n_distinct(Game))


aux <- s %>% filter(!is.na(Word)) %>% 
  mutate(score_bin = cut(Score, c(0,30,40,50,Inf),
                         c("<30","30+","40+","50+"))) %>%
  count(Player,score_bin) %>%
  mutate(prop = n/sum(n)*100)

aux %>%
  ggplot(aes(x = score_bin,
             y = prop, 
             fill = Player)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent)
  
score_per_game <- s %>% 
  filter(!is.na(Word)) %>%
  dplyr::group_by(Player, Game) %>%
  dplyr::mutate(total = sum(Score,Bonus, na.rm=T)) %>%
  dplyr::summarise(points = mean(Score, na.rm=T),
                   total = mean(total, na.rm=T),
                   maxim = max(Score),
                   minim = min(Score),
                   words = n()) %>% 
  group_by(Game) %>%
  mutate(winner = if_else(total == max(total),1,0)) %>%
  group_by(Player) %>%
  mutate(cumwinner = cumsum(winner))

score_per_game %>%
  slice(which.max(total))


ggplot(score_per_game, aes(x = Game, 
                           y = total, 
                           color = Player)) +
  geom_line() +
  labs( x = "Game", y = "Total Score",
        title = "Total Score per game")

ggplot(score_per_game, aes(x = Game, 
                           y = words, 
                           color = Player)) +
  geom_smooth() + 
  geom_line() +
  labs( x = "Game", y = "Total Score",
        title = "Average points / word")

score_per_game %>%
  ggplot(aes(x = Game,
             y = cumwinner,
             color  =Player)) + 
  geom_line() +
  theme_minimal()

score_per_game %>% 
  filter(winner == 1) %>%
  mutate(winner = if_else(Player == "gosia", winner, winner*-1)) %>%
  ggplot(aes(x = Game,
             y = winner,
             fill  = Player)) + 
  geom_col(color  = "black")

# Highest streak -- Gosia = 4. David = 5


top5_words <- s %>%
  filter(!is.na(Word)) %>%
  group_by(Player, Word) %>%
  summarise(total = sum(Score,Bingo,Bonus,na.rm=T),
            times_used = n()) %>%
  arrange(-total) %>% 
  slice_head(n = 5)

ggplot(top5_words,
       aes(x = Word,
           y = total)) +
  geom_col() + 
  facet_wrap("Player", scales = "free_x")
 
