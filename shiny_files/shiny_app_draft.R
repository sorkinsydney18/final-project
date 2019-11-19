#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("draft_graph.R")
library(babynames)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)
library(shiny)
library(markdown)
library(shinythemes)

data("babynames")
data("stop_words")

#figure out how to copy token to shiny

consumer_key <- "1G8amEhpNj0qkbU34l9oa1FFa"
consumer_secret <- "y14jSk4QQFZOXLcU4ogH3w0bYfRtTlNJlguUKDYfYRH36YvAAW"
app_name <- "gov_1005_proj"

token <- create_token(app_name, consumer_key, consumer_secret, set_renv = TRUE)


#create variables for ggplot
hockey <- get_timeline("NCAAIceHockey", n = 100) %>% 
  mutate(stripped_text = gsub("http.*","", text)) %>% 
  mutate(stripped_text = gsub("https.*", "", stripped_text)) %>% 
  select(status_id, created_at, stripped_text) %>% 
  unnest_tokens(word, stripped_text)

cleaned_hockey <- hockey %>% 
  anti_join(stop_words)

test <- babynames %>% 
  distinct(name, .keep_all = TRUE) %>% 
  mutate(name = str_to_lower(name))

joined_babynames_tweets <- cleaned_hockey %>% 
  left_join(test, by = c("word" = "name")) %>% 
  drop_na() %>% 
  group_by(status_id) %>% 
  count(sex, sort = TRUE) %>% 
  ungroup(status_id) %>% 
  mutate(status_id = reorder(status_id, n))


ui <- navbarPage("Project",
                 theme = shinytheme("united"),
    
           tabPanel("About",
                    fluidRow(
                        column(8,
                               includeMarkdown("about.Rmd"))
                    )),
           tabPanel("Data",
                    mainPanel(plotOutput("plot")),
                    sidebarPanel()),
           tabPanel("Explanation"))
                     
server <- function(input, output) {
  output$plot <- renderPlot({
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
  })
    
}

        
                               

# Run the application 
shinyApp(ui = ui, server = server)
