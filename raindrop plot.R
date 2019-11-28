library(rtweet)
library(cowplot)
library(tidyverse)

names <- read_rds("cleaned_data/cleaned_names.rds")
stop_words <- read_rds("cleaned_data/clean_stop_words.rds")

hockey <- get_timeline("NCAAIceHockey", n = 100) %>% 
  mutate(stripped_text = gsub("http.*","", text)) %>% 
  mutate(stripped_text = gsub("https.*", "", stripped_text)) %>% 
  select(status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words)


joined_names_tweets <- hockey %>% 
  left_join(names, by = c("word" = "name")) %>% 
  drop_na() %>% 
  select(-year,-prop, -n, -avg_prop) %>% 
  group_by(status_id) %>% 
  add_count(sex)
  
  


  replace_na(list(F = 0, M = 0)) %>% 
  mutate(tweet_female = ifelse(M == 0, 1,0))
  
  #raindrop plot copied

  ggplot(joined_names_tweets, aes(x=sex,y=created_at, fill = sex)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2) +
    geom_point(position = position_jitter(width = .15), size = .25) +
    ylab('date')+
    xlab('sex')+
    coord_flip()+
    theme_cowplot()+
    guides(fill = FALSE)
  

