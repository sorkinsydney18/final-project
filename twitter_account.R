library(rtweet)
library(lubridate)
library(tidyverse)

get_token()

##Create combined tibble of most recent tweets from the listed NCAA affiliate accounts

ncaa <- get_timeline("NCAA", n = 100, parse = TRUE) %>% 
  filter(is_retweet == "FALSE") %>% 
  select(screen_name, text, created_at, favorite_count, retweet_count)

hockey <- get_timeline("NCAAIceHockey", n = 100, parse = TRUE) %>%
  filter(is_retweet == "FALSE") %>% 
  select(screen_name, text, created_at, favorite_count, retweet_count)

track_field <- get_timeline("NCAATrackField", n = 100, parse = TRUE) %>%
  filter(is_retweet == "FALSE") %>% 
  select(screen_name, text, created_at, favorite_count, retweet_count)

lax <- get_timeline("NCAALAX", n = 100, parse = TRUE) %>% 
  filter(is_retweet == "FALSE") %>% 
  select(screen_name, text, created_at, favorite_count, retweet_count)

soccer <- get_timeline("NCAASoccer", n = 100, parse = TRUE) %>% 
  filter(is_retweet == "FALSE") %>% 
  select(screen_name, text, created_at, favorite_count, retweet_count)
  
tweets <- rbind(ncaa, hockey, track_field, lax, soccer) %>% 
  mutate(created_at = as.Date(ymd_hms(created_at)))

write_rds(tweets, "cleaned_data/tweets.rds")
file.copy("cleaned_data/tweets.rds", "shiny_files/tweets.rds", overwrite = TRUE)
