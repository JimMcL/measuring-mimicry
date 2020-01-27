#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "eatup.css",
  
  # Application title
  titlePanel("Eat Up Trial Results"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

      selectInput("cmpTime", h3("Comparison time"), 
                  choices = list("Plot all" = 0,
                                 "5 min ago" = 1,
                                 "10 min ago" = 2,
                                 "30 min ago" = 3,
                                 "1 hour ago" = 4,
                                 "6 hours ago" = 5,
                                 "1 day ago" = 6,
                                 "1 week ago" = 7,
                                 "2 weeks ago" = 8),
      selected = 0),
      
      actionButton("downloadData", "Download data"),
      p(textOutput("download_result"))
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Responses", fluidRow(
          htmlOutput("scoreSummary"),
          plotOutput("plotResponses"))),
        tabPanel("Success rates", plotOutput("ratesPlot")),
        tabPanel("Mimic scores", htmlOutput("scoreMimicImages")),
        tabPanel("Non-mimic scores", htmlOutput("scoreNonMimicImages")),
        tabPanel("Ant scores", htmlOutput("scoreAntImages")),
        tabPanel("Image stats", htmlOutput("imageStats"))
      )
    )
  )
))
