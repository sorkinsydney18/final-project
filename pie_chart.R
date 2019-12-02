library(rtweet)
library(cowplot)
library(tidytext)
library(tidyverse)
get_token()

names <- read_rds("cleaned_data/cleaned_names.rds")
stop_words <- read_rds("cleaned_data/clean_stop_words.rds")

ncaa_accounts <- c("NCAA", "NCAAIceHockey", "NCAATrackField", "NCAALAX", "NCAASoccer")

tweet_timeline <- get_timelines(ncaa_accounts, n = 1000, parse = TRUE)

#clean text from tweets before I join the data with the babynames dataset

cleaned_tweet_timeline <- tweet_timeline %>% 
  mutate(stripped_text = gsub("http.*","", text)) %>% 
  mutate(stripped_text = gsub("https.*", "", stripped_text)) %>% 
  select(screen_name, status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words)

#join babynames set with cleaned tweets

joined_names_tweets <- cleaned_tweet_timeline %>% 
  left_join(names, by = c("word" = "name"))