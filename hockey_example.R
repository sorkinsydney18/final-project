library(babynames)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)
library(fs)

get_token()

data("babynames")

hockey <- get_timeline("NCAAIceHockey", n = 100) %>% 
  mutate(stripped_text = gsub("http.*","", text)) %>% 
  mutate(stripped_text = gsub("https.*", "", stripped_text)) %>% 
  select(status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text) 


#gsub says pattern, replacement, x 
#use mutate
hockey$stripped_text <- gsub("http.*","",  hockey$text)
hockey$stripped_text <- gsub("https.*","", hockey$stripped_text)

hockey_cleaned <- hockey %>% 
  select(status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text)
  
#group_by(status_id) use this as id for each distinct tweet

#remove stop words
data("stop_words")

cleaned_hockey <- hockey_cleaned %>% 
  anti_join(stop_words)

#change case of names in babynames data set
test <- babynames %>% 
  distinct(name, .keep_all = TRUE) %>% 
  mutate(name = str_to_lower(name))

#join baby names and words

joined_babynames_tweets <- cleaned_hockey %>% 
  left_join(test, by = c("word" = "name")) %>% 
  drop_na() %>% 
  group_by(status_id) %>% 
  count(sex, sort = TRUE) %>% 
  ungroup(status_id) %>% 
  mutate(status_id = reorder(status_id, n))

joined_babynames_tweets %>% 
  ggplot(aes(x=status_id, y = n)) +
  geom_col() +
  facet_wrap(~sex) +
  coord_flip() +
  theme(axis.text.y=element_blank())


#save joined_babynames_tweets as rds
dir.create("cleaned_data")
write_rds(joined_babynames_tweets, "cleaned_data/joined_babynames_hckytweets.rds")



#check out dillon's shiny, do page of models? - model for each NCAA twitter, does gender predict tweet frequency? 



