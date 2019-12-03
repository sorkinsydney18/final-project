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


ui <- navbarPage("NCAA on Twitter",
                 theme = shinytheme("flatly"),
     
    ##########
    ##ABOUT##
    #########
                             
                 tabPanel("About",
                          
                          #load first image 
                          
                          imageOutput("ncaa_logo", width = "100%", height = "100%"),
                          br(),
                          
                          #title and subtitle
                          
                          h2("Does the NCAA promote its Men's and Women's teams differently?", align = "center"),
                          h4(em("An Analysis of how the NCAA uses Twitter to promote teams and athletes"), align = "center"),
                          br(),
                          div(),
                          
  
                
                          br(),
                          
                          fluidRow(column(2), column(8,
                                                     
                                          h4(strong("About this Project")),          
                                                     
                                          #text to introduce project
                                          
                                          p("The purpose of the project is to analyze and display how different NCAA Twitter accounts use their platform relay news to followers. This project is tasked with answering the question of 
                                                       whether there is an imbalance of tweets dedicated to Men's or Women's team and athletes.
                                                       Women's sports have been historically underrepresented in the media in turn damaging the
                                                       visibilty and growth of the women's game. Recent campaigns from top-tier brands like Nike or
                                                       Addidas have recognized the imbalance of media coverage and sought to equalize. Twitter 
                                                       represents just one platform in which the NCAA can promote its sports teams. Is there a 
                                                       difference for how the NCAA reports Men's and Women's teams? If so, does this highlight a
                                                       larger gender bias?"),
                                          
                                          br(),
                                          
                                          #text to explain how I selected the accounts to analyze and how I coded for gender
                                          
                                          h4(strong("How this project works")),
                                          
                                          
                                          
                                          p("I chose to analyze Division I NCAA Twitter accounts that included both Men's and Women's 
                                            coverage as some sports have seperate Twitters for the men and women divisions. I chose 
                                            these five sports and their Twitter accounts: Ice Hockey (@NCAAIceHockey), Track and Field
                                            (@NCAATrackField), Lacrosse (@NCAALAX), Soccer (@NCAASoccer), and the general NCAA account 
                                            (@NCAA). Most sports are covered by the general NCAA account."),
                                          
                                          span(),
                                          
                                          p("I coded for gender by flagging names or other gendered words and pronouns mentioned in
                                          a tweet. In order to guess gender from names I used Social Security data that includes 
                                          the most commonly associated gender with a name. Tweets that were flagged as 'Female'
                                          contained at least one mention of a typically female name or pronoun with no mention
                                          of a male name or pronoun. The same is true for 'Male' tweets.")
                                                     ))),
    
                 
     ###########
     ###DATA###
     ##########
 
           tabPanel("Graphics",
                    tabsetPanel(
                      
                      #this page includes the raincloud plots that show the density of male and female tweets
                      #by account
                      
                      tabPanel("Tweet Distribution",
                               
                               h3("NCAA Twitter Account Activity in 2019"),
 
                               br(),
                               
                               h4("subtitle explaining main graph"),
                               
                               plotOutput("full_raincloud"),
                               
                               br(),
                               br(),
                               br(),
                               br(),
                               
                               h4("Is there a difference in Twitter activity between accounts?"),
                               
                              sidebarPanel(
                                helpText("Choose an account to get a closer look at the division of tweets by gender"),
                                 selectInput("account_name", "NCAA Twitter Accounts:",
                                             choices = list("@NCAA" = "NCAA",
                                                            "@NCAAIceHockey" = "NCAAIceHockey",
                                                            "@NCAATrackField" = "NCAATrackField",
                                                            "@NCAALLAX" = "NCAALAX",
                                                            "@NCAASoccer" = "NCAASoccer"),
                                             selected = "NCAA")),
                               
                               mainPanel(plotOutput("raincloud"))),
                      
                      tabPanel("Content",
                               
                               h3("What percentage of tweets are devoted to Females or Males?"),
                               
                               br(),
                               
                               h4("Different Accounts show an imbalance of gendered tweets"),
                               
                               sidebarPanel(
                                 helpText("Choose an account to get a full picture of the percentage of tweets each account devotes
                                          to men, women, or neither."),
                                 span(),
                                 helpText(em("'Female' and 'Male' tweets include tweets that only mention the specified gender. 
                                             'Neither' includes tweets that mention both or no genders.")),
                                 selectInput("account_names1", "NCAA Twitter Accounts:",
                                             choices = list("@NCAA" = "NCAA",
                                                            "@NCAAIceHockey" = "NCAAIceHockey",
                                                            "@NCAATrackField" = "NCAATrackField",
                                                            "@NCAALLAX" = "NCAALAX",
                                                            "@NCAASoccer" = "NCAASoccer"),
                                             selected = "NCAA")),
                               
                               mainPanel(plotOutput("pie_chart"))
                               ))),
                    
    ###################
    ##EXPLORE TWEETS###
    ###################
    
           tabPanel("Explore Tweets",
                    
                    fluidPage(
                      titlePanel("What is the NCAA tweeting about?"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Pick an NCAA Twitter Account to view its recent tweets"),
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
    
    #tab to explain where I got my data and reference resources used
    
    tabPanel("Footnotes",
             
             h3("References"),
             br(),
             
             p("I obtained twitter data through the rtweet package (created by Michael Kearney) 
               and relevant API's."),
             p("I used name data from the babynames package. This is a collection of names from Social Security
               data."),
             p("I used the the geom_flat_violin function to create my raincoud plots. I used the raincloud script
               built by Allen Poggiali and Kristen Whitaker. More about raincloud plots at ther github found here:
               https://github.com/RainCloudPlots/RainCloudPlots"),
             br(),
             
             h3("Contact Me"),
             
             p("My name is Sydney Sorkin and I am an undgraduate at Harvard. I'm majoring in Government with a
               minor in Psychology. I'm interested in applying data science to explain different facets of human
               nature, especially as it relates to American politics and sports (namely hockey!). Email me at
               ssorkin@college.harvard.edu. The code for this project can be found here: ")))
                     
server <- function(input, output, session) {
 
  ##########
  ##ABOUT##
  ########
  
  output$ncaa_logo <- renderImage({
    
    list(src = 'www./ncaa_sports.png',
         height = 300,
         width = 700,
         style = "display: block; margin-left: auto; margin-right: auto;")},
    deleteFile = FALSE
    )
  
  
  ########
  ##DATA##
  ########
  
  output$full_raincloud <- renderPlot({
    
    ggplot(raincloud, aes(x=sex_id,y=created_at, fill = screen_name, alpha = .5)) +
      geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 4,scale="count") +
      geom_point(position = position_jitter(width = .15), size = .25, alpha = .5) +
      ylab('Date')+
      xlab('Gender')+
      coord_flip()+
      theme_cowplot() +
      scale_fill_manual("Accounts", values = wes_palette("Darjeeling2")) +
      guides(alpha = FALSE) +
      scale_y_datetime(limits = as.POSIXct(c("2019-01-01", "2019-12-01")))

  })
  
  
  #raincloud plot with reactive data
  
  output$raincloud <- renderPlot({
      
    raincloud %>% 
      filter(account_name == input$account_name) %>% 
      ggplot(aes(x=sex_id ,y=created_at, fill = sex_id)) +
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
