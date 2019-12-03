#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)
library(shiny)
library(cowplot)
library(DT)
library(markdown)
library(shinythemes)

source("R_rainclouds.R")


#create variables for ggplot
joined_names_tweets <- read_rds("joined_names_tweets.rds")
tweets <- read_rds("tweets.rds")
raincloud <- read_rds("raincloud.rds")


ui <- navbarPage("Project",
                 theme = shinytheme("flatly"),
    
     ###########
     ###DATA###
     ##########
 
           tabPanel("Graphics",
                    tabsetPanel(
                      tabPanel("Frequency",
                               
                               h3("When is the NCAA tweeting the most? And about who?"),
                               
                               br(),
                               
                               h4("Full Sample"),
                               
                               br(),
                               
                               plotOutput("full_raincloud"),
                               
                               br(),
                               
                               h4("Are different accounts tweeting more about men or women?"),
                               
                              sidebarPanel(
                                helpText("The raincloud graphs show the distribution of gendered tweets during a time intervals"),
                                 selectInput("account_name", "NCAA Twitter Accounts:",
                                             choices = list("@NCAA" = "NCAA",
                                                            "@NCAAIceHockey" = "NCAAIceHockey",
                                                            "@NCAATrackField" = "NCAATrackField",
                                                            "@NCAALLAX" = "NCAALAX",
                                                            "@NCAASoccer" = "NCAASoccer"),
                                             selected = "NCAA")),
                               
                               mainPanel(plotOutput("raincloud"))),
                      
                      tabPanel("Content",
                               
                               h3("How often is the NCAA tweeting about Men or Women? A look at tweet distributions"),
                               
                               br(),
                               
                               h4("Different Accounts show an imbalance of gendered tweets"),
                               
                               sidebarPanel(
                                 selectInput("account_names1", "NCAA Twitter Accounts:",
                                             choices = list("@NCAA" = "NCAA",
                                                            "@NCAAIceHockey" = "NCAAIceHockey",
                                                            "@NCAATrackField" = "NCAATrackField",
                                                            "@NCAALLAX" = "NCAALAX",
                                                            "@NCAASoccer" = "NCAASoccer"),
                                             selected = "NCAA")),
                               
                               mainPanel(plotOutput("pie_chart"))
                               ))),
                    
    #############
    ##EXPLORE###
    ############
    
           tabPanel("Explore",
                    
                    fluidPage(
                      titlePanel("Explore the data"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Pick an NCAA Twitter Account to view recent tweets"),
                          h3("Tweet Search"),
                          selectInput("screen_name", NULL,
                                      choices = list("@NCAA" = "NCAA",
                                                     "@NCAAIceHockey" = "NCAAIceHockey",
                                                     "@NCAATrackField" = "NCAATrackField",
                                                     "@NCAALLAX" = "NCAALAX",
                                                     "@NCAASoccer" = "NCAASoccer"),
                                      selected = "NCAA")),
                        mainPanel(
                          DTOutput("word_table"))))),
    #########
    ##ABOUT##
    #########
    
    tabPanel("About",
                    fluidRow(
                        column(8,
                               includeMarkdown("about.Rmd")))))
                     
server <- function(input, output, session) {
 
  ########
  ##DATA##
  ########
  
  output$full_raincloud <- renderPlot({
    
    ggplot(raincloud, aes(x=sex_id,y=created_at, fill = screen_name)) +
      geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 4,scale="count") +
      geom_point(position = position_jitter(width = .15), size = .25, alpha = .5) +
      ylab('Date')+
      xlab('Gender')+
      coord_flip()+
      theme_cowplot()+
      guides(fill = FALSE)
      #scale_fill_manual(values = c("snow1", "steelblue")) +
      #scale_color_brewer("Accounts")
    
  })
  
  
  #raincloud plot with reactive data
  
  output$raincloud <- renderPlot({
      
    raincloud %>% 
      filter(account_name == input$account_name) %>% 
      ggplot(aes(x=sex_id,y=created_at, fill = sex_id)) +
      geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 4, scale = "count") +
      geom_point(position = position_jitter(width = .15), size = .25, alpha = .5) +
      ylab('Date')+
      xlab('Gender')+
      coord_flip()+
      theme_cowplot()+
      guides(fill = FALSE) +
      scale_fill_manual(values = c("snow1", "steelblue"))
  })

  #pie chart with reactive data  
  
output$pie_chart <- renderPlot({
  
  pie_chart_reac <- 
    
    joined_names_tweets %>% 
    group_by(status_id) %>%
    count(sex) %>% 
    spread(key = sex, value = n) %>%
    left_join(joined_names_tweets, by = "status_id") %>% 
    select(-word) %>%
    replace_na(list(F = 0, M = 0)) %>% 
    mutate(tweet_id = case_when(M == F ~ "Neither",
                                F == 0 ~ "Male",
                                M == 0 ~ "Female",
                                TRUE ~ "Neither")) %>%
    mutate(account_names1 = screen_name) %>% 
    group_by(account_names1) %>% 
    count(tweet_id) %>% 
    mutate(prop = n/sum(n)) %>% 
    filter(account_names1 == input$account_names1) 
  
  
    ggplot(pie_chart_reac, aes(x = "", y = prop, fill = tweet_id)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_brewer("") +
    theme(axis.text.x=element_blank()) +
    theme_void() +
    geom_text(aes(label = percent(prop, accuracy = .1)), 
              position = position_stack(vjust = 0.5), 
              color = "gray18")
  
})
  
  
  ############
  ##EXPLORE##
  ###########
  
  output$word_table <- renderDT({
    
    tweets_reac <- tweets %>% 
      filter(screen_name == input$screen_name) %>% 
      select(-screen_name)
    
    datatable(tweets_reac,
              class = 'display',
              rownames = FALSE,
              selection = 'single',
              colnames = c("Tweet Text", "Date", "Favorites", "Retweets"),
              options = list(dom = 'tip'))
  })
      
}

      
# Run the application 
shinyApp(ui = ui, server = server)
