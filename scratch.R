library(rtweet)
library(httpuv)
library(tidyverse)
library(tidytext)
library(gender)

get_token()


#look up users with "harvard" in the profile
users <- search_users("Harvard crimson", n = 100)

#lookup users
harvard <- lookup_users("Harvard")


#save user I want to target
harvard_twtrs <- c("Harvard", "thecrimson", "harvardcrimson", "HVClub", "THCSports", "HarvardAlumni") 
  
stuff <- lookup_users(harvard_twtrs) %>% 
  select(screen_name, text)
  
head(stuff$text)


#get_timeline finds up to 3,200 tweets posted by target account

harvard_timeline <- get_timeline("Harvard", n = 3200)

#built in ggplot function that creates frequency of tweets over specified interval of time
ts_plot(harvard_timeline)

harvard1 <- search_fullarchive(q = "Harvard", 
                               n = 100,
                               fromDate = "201701012315",
                               toDate = "201812312315", 
                               env_name = "production")

ncaa <- c("NCAA", "NCAATrackField", "NCAALAX", "NCAAVolleyball", "NCAASoccer", "NCAAIceHockey")

hockey <- get_timeline("NCAAIceHockey") %>% 
  select(created_at, text) 

head(hockey$text)

##get r to recognize names - use census data? 


## what i need to do:
#if Harvard: find way to narrow down to just sports tweets
#both: extract names from text, if name not in text find way to code for gender, then use gender package 
##to code for gender