library(babynames)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)

get_token()

data("babynames")

hockey <- get_timeline("NCAAIceHockey", n = 100) 
  
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
  distinct(name, .keep_all = TRUE)

test$name <- str_to_lower(test$name)

#join baby names and words

joined_babynames_tweets <- cleaned_hockey %>% 
  left_join(test, by = c("word" = "name")) %>% 
  drop_na() %>% 
  group_by(status_id) %>% 
  summarise(total_male = sum(sex == "M"))





#grepl returns logical vector of pattern 
grepl("eric", hockey_cleaned$word)



#check out dillon's shiny, do page of models? - model for each NCAA twitter, does gender predict tweet frequency? 



