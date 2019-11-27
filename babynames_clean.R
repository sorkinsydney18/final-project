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
  distinct(name, .keep_all = TRUE) %>% 
  mutate(name = str_to_lower(name))

write_rds(cleaned_names, "cleaned_data/cleaned_names.rds")
