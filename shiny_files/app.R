#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(shinythemes)

ui <- navbarPage("Project",
                 theme = shinytheme("united"),
    
           tabPanel("About",
                    fluidRow(
                        column(8,
                               includeMarkdown("about.Rmd"))
                    )),
           tabPanel("Data"),
           tabPanel("Explanation"))
                     
server <- function(input, output) {
    
}

        
                               

# Run the application 
shinyApp(ui = ui, server = server)
