library(rtweet)
library(cowplot)
library(tidytext)
library(scales)
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
  left_join(names, by = c("word" = "name")) %>% 
  mutate(account_names1 = screen_name)

#save data for pie charts 

write_rds(joined_names_tweets, "cleaned_data/joined_names_tweets.rds") 
file.copy("cleaned_data/joined_names_tweets.rds", "shiny_files/joined_names_tweets.rds", overwrite = TRUE)

############################# 
#clean data for raincloud plots

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
  
  #I rejoined the original dataset to include the date variable
  
  left_join(cleaned_tweet_timeline, by = "status_id") %>% 
  select(-word) %>% 
  mutate(account_name = screen_name)

write_rds(raincloud, "cleaned_data/raincloud.rds")
file.copy("cleaned_data/raincloud.rds", "shiny_files/raincloud.rds", overwrite = TRUE)

#########################
#UNFILTERED RAINCLOUD PLOT 

ggplot(raincloud, aes(x=sex_id,y=created_at, fill = screen_name, alpha = .5)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 4,scale="count") +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = .5) +
  ylab('Date')+
  xlab('Gender')+
  coord_flip()+
  theme_cowplot()
  guides(fill = FALSE) +
  scale_y_datetime(limits = as.POSIXct(c("2019-04-01", "2019-12-01")))
  
  
  #scale_fill_manual(values = c("snow1", "steelblue")) +
  #cale_color_brewer("Accounts")


##################
#pie chart cleaning

pie_chart <- joined_names_tweets %>% 
  group_by(status_id) %>%
  count(sex) %>% 
  spread(key = sex, value = n) %>%
  left_join(joined_names_tweets, by = "status_id") %>% 
  select(-word) %>%
  replace_na(list(F = 0, M = 0)) %>% 
  
  #This variable labels each individual tweet as either a Female, Male, or Neither - this is similar
  #to the coding from above, but this time I kept tweets that are neither purely male or female 
  #to get a whole sense of each account's tweeting tendencies
  
  mutate(tweet_id = case_when(M == F ~ "Neither",
                              F == 0 ~ "Male",
                              M == 0 ~ "Female",
                              TRUE ~ "Neither")) %>% 
    mutate(account_names1 = screen_name) %>% 
  group_by(account_names1) %>% 
  count(tweet_id) %>% 
  mutate(prop = n/sum(n)) 

#Pie chart format 

pie_chart %>%
  filter(screen_name == "NCAA") %>% 

  ggplot(aes(x = "", y = prop, fill = tweet_id)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_brewer("") +
    theme(axis.text.x=element_blank()) +
    theme_void() +
  geom_text(aes(label = percent(prop, accuracy = .1)), 
            position = position_stack(vjust = 0.5), 
            color = "gray18")
    
   
  
         
  