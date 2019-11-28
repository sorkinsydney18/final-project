library(rtweet)
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
  group_by(status_id) %>% 
  count(sex)

