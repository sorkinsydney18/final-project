library(babynames)
library(rtweet)
library(tidytext)
library(tidyverse)

get_token()

data("babynames")

hockey <- get_timeline("NCAAIceHockey", n = 10) 
  
#gsub says pattern, replacement, x 
hockey$stripped_text <- gsub("http.*","",  hockey$text) 

hockey_cleaned <- hockey %>% 
  unnest_tokens(word, stripped_text) 

#load baby names

data("babynames")

example <- babynames %>% 
  grep("Anna", name)



