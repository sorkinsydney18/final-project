library(rtweet)
library(cowplot)
library(tidytext)
library(tidyverse)
source("R_rainclouds.R")
get_token()

#read in cleaned babynames dataset and cleaned stop words

names <- read_rds("cleaned_data/cleaned_names.rds")
stop_words <- read_rds("cleaned_data/clean_stop_words.rds")

#character vector containing the twitter accounts I am examining

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

#save data for pie charts 

write_rds(joined_names_tweets, "cleaned_data/joined_names_tweets.rds") 
file.copy("cleaned_data/joined_names_tweets.rds", "shiny_files/joined_names_tweets.rds", overwrite = TRUE)
 

#data used for raincloud plots

raincloud <- joined_names_tweets %>%  
  #drop values that are not matched with names in babynames dataset
  
  drop_na() %>% 
  group_by(status_id) %>% 
  
  #I use count to see the frequency of gender flags per tweet
  
  count(sex) %>% 
  spread(key = sex, value = n) %>%
  replace_na(list(F = 0, M = 0)) %>% 
  
  #tweet_female is a dummy variable used to id a tweet as either "female" or "male"
  #a tweet is considered "female" if there were only female names mentioned in the tweet ('M' == 0)
  #tweets that mentioned both male and female names are not included 
  
  mutate(tweet_female = ifelse(M == 0, 1,0)) %>% 
  mutate(sex_id = ifelse(tweet_female == 1, "F", "M")) %>% 
  
  #I rejoined the original dataset to include the data variable
  
  left_join(cleaned_tweet_timeline, by = "status_id") %>% 
  select(-word)

write_rds(raincloud, "cleaned_data/raincloud.rds")
file.copy("cleaned_data/raincloud.rds", "shiny_files/raincloud.rds")

ggplot(raincloud, aes(x=sex_id,y=created_at, fill = sex_id)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 4) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = .5) +
  ylab('Date')+
  xlab('Gender')+
  coord_flip()+
  theme_cowplot()+
  guides(fill = FALSE) +
  scale_fill_manual(values = c("snow1", "steelblue"))



###### pie chart cleaning

pie_chart <- joined_names_tweets %>% 
  group_by(screen_name) %>% 
  count(sex)
  