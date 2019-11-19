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
library(markdown)
library(shinythemes)

data("babynames")
data("stop_words")


#create variables for ggplot
joined_babynames_tweets <- read_rds("joined_babynames_hckytweets.rds")


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
