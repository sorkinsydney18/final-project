library(rtweet)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)
library(fs)
get_token()
data("stop_words")

names <- read_rds("cleaned_data/cleaned_names.rds")

ncaa <- get_timeline("NCAA", n = 100) %>% 
  mutate(stripped_text = gsub("http.*","", text)) %>% 
  mutate(stripped_text = gsub("https.*", "", stripped_text)) %>% 
  select(status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text) %>% 
  anti_join(stop_words)


joined_ncaa_names <- ncaa %>% 
  left_join(names, by = c("word" = "name")) %>% 
  drop_na() %>% 
  group_by(status_id) %>% 
  count(sex, sort = TRUE) %>% 
  ungroup(status_id) %>% 
  mutate(status_id = reorder(status_id, n))

joined_ncaa_names %>% 
  ggplot(aes(x=status_id, y = n)) +
  geom_col() +
  facet_wrap(~sex) +
  coord_flip() +
  theme(axis.text.y=element_blank())
