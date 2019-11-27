library(babynames)
library(UScensus2010)
library(tidyverse)
data("babynames")
data("state.names")

#The purpose of this script is to eliminate uncommon names in babynames dataset
#This will make left_join with tweets more accurate

cleaned_names <- babynames %>% 
  filter(year >= 1995) %>% 
  group_by(year) %>% 
  mutate(avg_prop = mean(prop)) %>% 
  filter(prop >= avg_prop) %>% 
  ungroup() %>% 
  distinct(name, .keep_all = TRUE) %>% 
  mutate(name = str_to_lower(name))

gender_id <- data.frame(year = "N/A",
                        sex = c("F","F","F","M", "M", "M", "M", "M", "M", "F","F","F"),
                        name = c("she","her","hers","he", "him", "his", "man", 
                                 "men", "men's", "woman",  "women", "women's"),
                        n = "N/A",
                        prop = "N/A",
                        avg_prop = "N/A")

cleaned_names <- rbind(cleaned_names, gender_id)

write_rds(cleaned_names, "cleaned_data/cleaned_names.rds")
