library(rtweet)
library(cowplot)
library(tidytext)
library(lubridate)
library(gganimate)
library(tidyverse)
source("R_rainclouds.R")

names <- read_rds("cleaned_data/cleaned_names.rds")
stop_words <- read_rds("cleaned_data/clean_stop_words.rds")

hockey <- get_timeline("NCAAIceHockey", n = 1000) %>% 
  mutate(stripped_text = gsub("http.*","", text)) %>% 
  mutate(stripped_text = gsub("https.*", "", stripped_text)) %>% 
  select(status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words)


joined_names_tweets <- hockey %>% 
  left_join(names, by = c("word" = "name")) %>% 
  drop_na() %>% 
  select(-year,-prop, -word, -n, -avg_prop, -word) %>% 
  group_by(status_id) %>% 
  count(sex) %>% 
  spread(key = sex, value = n) %>%
  replace_na(list(F = 0, M = 0)) %>% 
  mutate(tweet_female = ifelse(M == 0, 1,0)) %>% 
  mutate(tweet_male = ifelse(tweet_female == 0, 1,0)) %>% 
  left_join(hockey, by = "status_id") %>% 
  select(-word) %>% 
  mutate(sex_id = ifelse(tweet_female == 1, "F", "M")) %>% 
  mutate(month = month(ymd_hms(created_at), label = TRUE, abbr = TRUE))


  
  #line plot - do it by month
ggplot(joined_names_tweets, aes( x = tweet_female)) +
  geom_density()
  

  #raindrop plot copied

  ggplot(joined_names_tweets, aes(x=sex_id,y=created_at, fill = sex_id)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 4) +
    geom_point(position = position_jitter(width = .15), size = .25, alpha = .5) +
    ylab('Month')+
    xlab('Gender')+
    coord_flip()+
    theme_cowplot()+
    guides(fill = FALSE) +
    scale_fill_manual(values = c("snow1", "steelblue"))
  

