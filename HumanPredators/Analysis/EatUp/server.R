#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("scripts/db.R", local = TRUE)
source("scripts/download-firebase.R", local = TRUE)
source("scripts/session-info.R", local = TRUE)
source("scripts/derive-stats.R", local = TRUE)
source("scripts/strings.R", local = TRUE)

BASE_IMG_URL <- "https://jimmcl.github.io/EatUp/"

# Load up the database with Firebase data
tmp <- MDbBringUpToDate(QueryFirebase)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  .nowPlusMins <- function(mins) Sys.time() + mins * 60
  
  .getFrom <- function(cmpTime) {
    switch(cmpTime, 
           "0" = list(NULL, NULL),
           "1" = list(.nowPlusMins(-5), c("> 5 mins", "< 5 mins")),
           "2" = list(.nowPlusMins(-10), c("> 10 mins", "< 10 mins")),
           "3" = list(.nowPlusMins(-30), c("> 30 mins", "< 30 mins")),
           "4" = list(.nowPlusMins(-60), c("> 1 hour", "< 1 hour")),
           "5" = list(.nowPlusMins(-60 * 6), c("> 6 hour", "< 6 hour")),
           "6" = list(.nowPlusMins(-60 * 24), c("> 24 hours", "< 24 hours")),
           "7" = list(.nowPlusMins(-60 * 24 * 7), c("> 7 days", "< 7 days")),
           "8" = list(.nowPlusMins(-60 * 24 * 14), c("> 14 days", "< 14 days")))
  }
  
  observeEvent(input$downloadData, {
    result <- MDbBringUpToDate(QueryFirebase)
    output$download_result <- renderText({
      paste("Downloaded", JToSentence(sapply(names(result), function (nm) sprintf("%d %s(s)", length(result[[nm]]), nm))))
    })
  })
  
  renderMimicScoresSummary <- eventReactive(c(input$downloadData, input$cmpTime), {
    # Get comparison time, i.e. compare stats from before and after this time
    from <- .getFrom(input$cmpTime)
    summary <- MDbRun(ScoreSummary, from = from[[1]])
    line1 <- ""
    rowFmt <- "<tr><th>%s</th><td>%d</td><td>%d</td><td>%d</td></tr>"
    if (summary[2, 1] > 0 && summary[1, 2] != summary[2, 2]) {
      line1 <- sprintf(rowFmt, from[[2]][2], summary[2, 1], summary[2, 2], summary[2, 3])
    }
    paste(
      "<table class='summary pretty'>",
      "<tr><th></th><th>Sessions</th><th>Decisions</th><th>Mimic decisions</th></tr>",
      line1,
      sprintf(rowFmt, "Total", summary[1, 1], summary[1, 2], summary[1, 3]), 
      "</table>")
  })

  # Redraw plot after download button pressed as well as change in cmpTime
  plotIdRates <- eventReactive(c(input$downloadData, input$cmpTime), {
    # Get comparison time, i.e. compare stats from before and after this time
    from <- .getFrom(input$cmpTime)
    summary <- MDbRun(PlotCompareSession, legendLabels = from[[2]], from = from[[1]], addSampleSize = TRUE)
  })
  
  # Redraw photo scores table after download button pressed as well as change in cmpTime
  renderMimicScoresTable <- eventReactive(c(input$downloadData, input$cmpTime), {
    # Get comparison time, i.e. compare stats from before and after this time
    from <- .getFrom(input$cmpTime)
    HTMLBuildImageScoresTable(mimicType = "mimic", from = from[[1]])
  })
  
  # Redraw photo scores table after download button pressed as well as change in cmpTime
  renderAntScoresTable <- eventReactive(c(input$downloadData, input$cmpTime), {
    # Get comparison time, i.e. compare stats from before and after this time
    from <- .getFrom(input$cmpTime)
    HTMLBuildImageScoresTable(mimicType = "model", from = from[[1]])
  })
  
  # Redraw photo scores table after download button pressed as well as change in cmpTime
  renderNonMimicScoresTable <- eventReactive(c(input$downloadData, input$cmpTime), {
    # Get comparison time, i.e. compare stats from before and after this time
    from <- .getFrom(input$cmpTime)
    HTMLBuildImageScoresTable(mimicType = "non-mimic", from = from[[1]])
  })
  
  renderImageStatsTable <- eventReactive(c(input$downloadData, input$cmpTime), {
    HTMLBuildImageStatsTable()
  })
  
  # Redraw responses histogram after download button pressed as well as change in cmpTime
  plotResponseHist <- eventReactive(c(input$downloadData, input$cmpTime), {
    # Get comparison time, i.e. compare stats from before and after this time
    from <- .getFrom(input$cmpTime)
    # TODO? I could use renderImage to re-render every time the user interface plot size changes
    MDbRun(PlotResponsesByTime, cutoffTime = from[[1]])
  })
  
  output$scoreSummary <- renderText({ renderMimicScoresSummary() })
  
  output$ratesPlot <- renderPlot({ plotIdRates() })
  
  output$scoreMimicImages <- renderText({ renderMimicScoresTable() })
  
  output$scoreNonMimicImages <- renderText({ renderNonMimicScoresTable() })
  
  output$scoreAntImages <- renderText({ renderAntScoresTable() })
  
  output$plotResponses <- renderPlot({ plotResponseHist() })

  output$imageStats <- renderText({ renderImageStatsTable() })
})

