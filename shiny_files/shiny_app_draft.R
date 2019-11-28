#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(babynames)
library(rtweet)
library(tidytext)
library(tidyverse)
library(stringr)
library(shiny)
library(DT)
library(markdown)
library(shinythemes)

data("babynames")
data("stop_words")


#create variables for ggplot
joined_babynames_tweets <- read_rds("joined_babynames_hckytweets.rds")
tweets <- read_rds("tweets.rds")

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
                                      choices = tweets$screen_name,
                                      selected = "@NCAA")),
                        mainPanel(
                          DTOutput("word_table"))
                    
                    ))))
                     
server <- function(input, output, session) {
  
  ############
  ##EXPLORE##
  ###########
  
  output$word_table <- renderDT({
    
    datatable(tweets %>% filter(screen_name == input$screen_name) %>% select(-screen_name),
              class = 'display',
              rownames = FALSE,
              selection = 'single',
              colnames = c("Tweet Text", "Date", "Favorites", "Retweets"),
              options = list(dom = 'tip'))
  })
  
  
  
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
