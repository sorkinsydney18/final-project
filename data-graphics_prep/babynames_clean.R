library(babynames)
library(UScensus2010)
library(tidytext)
library(tidyverse)
data("babynames")
data("stop_words")

#The purpose of this script is to eliminate uncommon names in babynames dataset
#This will make left_join with tweets more accurate

#I used 1995 as a cutoff year since most college students are born before 1995
#I then calculated the average proportion of a name per year and used that to eliminate uncommon names

cleaned_names <- babynames %>% 
  filter(year >= 1995) %>% 
  group_by(year) %>% 
  mutate(avg_prop = mean(prop)) %>% 
  filter(prop >= avg_prop) %>% 
  ungroup() %>% 
  distinct(name, .keep_all = TRUE) %>% 
  mutate(name = str_to_lower(name))

#I created a gender variable since some tweets will include gendered pronouns instead of names
#When I merge the names with tweets it can identify gender pronouns as well

gender_id <- data.frame(year = "N/A",
                        sex = c("F","F","F","M", "M", "M", "M", "M", "M", "F","F","F"),
                        name = c("she","her","hers","he", "him", "his", "man", 
                                 "men", "men's", "woman",  "women", "women's"),
                        n = "N/A",
                        prop = "N/A",
                        avg_prop = "N/A")

cleaned_names <- rbind(cleaned_names, gender_id)

clean_stop_words <- stop_words %>% 
  filter(!word %in% c("he", "him", "his", "she", "her", "hers"))

write_rds(cleaned_names, "cleaned_data/cleaned_names.rds")
write_rds(clean_stop_words, "cleaned_data/clean_stop_words.rds")
