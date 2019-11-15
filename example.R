library(babynames)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)

get_token()

data("babynames")

hockey <- get_timeline("NCAAIceHockey", n = 10) 
  
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





grep(names[i], hockey_cleaned$word)

for(i in 1:length(names)){storage[[i]]} <- (grep(names[i],hockey_cleaned$word))

[is.element(df$id,grep(unis[i],g)),"index"] <- df[is.element(df$id,grep(unis[i],g)),"index"]+1


#grepl returns logical vector of pattern 
grepl("eric", hockey_cleaned$word)



example <- babynames %>% 
  grep("Anna", name)



