
library(babynames)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)

get_token()

hockey <- get_timeline("NCAAIceHockey", n = 100) %>% 
  mutate(stripped_text = gsub("http.*","", text)) %>% 
  mutate(stripped_text = gsub("https.*", "", stripped_text)) %>% 
  select(status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text)

#remove stop words
data("stop_words")

cleaned_hockey <- hockey %>% 
  anti_join(stop_words)

#babynames dataset
data("babynames")

#change case of names in babynames data set
test <- babynames %>% 
  distinct(name, .keep_all = TRUE) %>% 
  mutate(name = str_to_lower(name))

#joining names and tweets
joined_babynames_tweets <- cleaned_hockey %>% 
  left_join(test, by = c("word" = "name")) %>% 
  drop_na() %>% 
  group_by(status_id) %>% 
  count(sex, sort = TRUE) %>% 
  ungroup(status_id) %>% 
  mutate(status_id = reorder(status_id, n))

#graph 

joined_babynames_tweets %>% 
  ggplot(aes(x=status_id, y = n)) +
  geom_col() +
  facet_wrap(~sex) +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  labs(title = "Gender Mentions in Last 100 Tweets of @NCAAIceHockey",
       subtitle = "Male names are favored",
       caption = "Gender assumed by names mentioned in individual Tweet",
       y = "Count of names mentioned per Tweet",
       x = "Distinct Tweets")

#cleaned data directory
dir.create("cleaned_data")
write_rds(joined_babynames_tweets, "cleaned_data/joined_babynames_hckytweets.rds")
file.copy("cleaned_data/joined_babynames_hckytweets.rds", "shiny_files/joined_babynames_hckytweets.rds")








#notes from meeting 
#how to visualize?
##tweets by month - over time tweet frequency - by season? fall spring winter
##alluvial plot
#raindrop plots (animation)
#story tab - just raindrop plot that shows story
#exlpore data tab - DT table, allow person to click sport, date, other stuff 

#how to incorporate statistical modeling into tweet data - frequency of tweets ~ gender? 
#no stat modeling
