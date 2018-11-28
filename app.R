#!/usr/bin/env Rscript

# rSPRITE - An implementation of SPRITE, from an idea by James Heathers.
# Written by Nick Brown (nicholasjlbrown@gmail.com), 2018.
# Thanks to Cédric Batailler for help with the X-axis.

source('libraries.R')
source('parameters.R')
source('stat_functions.R')
source('messages.R')
source('charts.R')

server <- function (input, output, session) {
  fixedCount <- reactive({
    result <- 0
    sn <- gsub(" ", "", input$fixedCount)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^[0-9]+$", sn)) {
        f <- as.numeric(sn)
        if ((f > 0) && (f < input$N)) {
          result <- f
        }
      }
    }

    if (result == rSprite.huge) {
      s <- paste("Fixed count must be an integer from 1 to ", (input$N - 1)
               , "; input |", input$fixedCount
               , "| ignored"
               , sep=""
      )
      rSprite.message(s, shinyType="warning")
      result <- 0
    }

    result
  })

  fixedResponse <- reactive({
    result <- 0
    sn <- gsub(" ", "", input$fixedResponse)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^-?[0-9]+$", sn)) {
        result <- as.numeric(sn)
      }
    }

    if (result == rSprite.huge) {
      s <- paste("Fixed value must be an integer from ", input$scaleMin
               , " to ", input$scaleMax
               , "; input |", input$fixedResponse
               , "| ignored"
               , sep=""
      )
      rSprite.message(s, shinyType="warning")
      result <- 0
    }

    result
  })

  reactiveSample <- eventReactive(input$go, {
    rSprite.message("Calculating...", shinyNow=TRUE)
    set.seed(if (input$fixedSeed) 1 else as.numeric(Sys.time()))
    fixed <- rep(fixedResponse(), fixedCount())
    gridSize <- sqrt(as.numeric(input$gridSize))

    rSprite.getSample(
      gridSize ^ 2
    , input$N
    , input$tMean
    , input$tSD
    , input$scaleMin
    , input$scaleMax
    , input$dp
    , fixed
    )
  })

# This element is just a place to catch and handle changes in the input controls and their relations to each other.
# We never actually output anything to a text box.
  output$dummy <- renderText({
    N <- input$N
    tMean <- input$tMean
    tSD <- input$tSD
    scaleMin <- input$scaleMin
    scaleMax <- input$scaleMax
    dp <- input$dp
    dstep <- c(0.1, 0.01, 0.001)[dp]

    updateNumericInput(session, inputId="scaleMin", max=(scaleMax - 1))
    updateNumericInput(session, inputId="scaleMax", min=(scaleMin + 1))
    updateNumericInput(session, inputId="tMean", min=scaleMin, max=scaleMax, step=dstep)
    updateNumericInput(session, inputId="tMean", min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)

# It is tempting to force the mean value to a GRIM-consistent one here
#  (cf. what we do for the SD below), but this would be an error,
#  as we would be unable to "scroll" from one valid mean to another using the
#  input spinners if there were any invalid intermediate values
#  (we would constantly be forced back).
# However, we do force the mean to be between scaleMin and scaleMax.
    if (!is.na(tMean)) {
      newMean <- max(min(round(tMean, dp), scaleMax), scaleMin)
      if (newMean != tMean) {
        updateNumericInput(session, inputId="tMean", value=newMean)
      }
    }

# Similarly, it would be nice to have the range for the SD limited by the current mean,
#  but this leads to all sorts of complications. So we allow the user to enter an SD
#  that is too small or large, and tell them later.
    if (!is.na(tSD)) {
      newSD <- max(min(round(tSD, dp), scaleMax), 0)
      if (newSD != tSD) {
        updateNumericInput(session, inputId="tSD", value=newSD)
      }
    }

    return()      # just falling out at the end gives an Shiny error message the first time we come here
  })

  output$plotDisplayed <- reactive({
    input$go
    input$help

    (length(rSprite.plotData) > 0)
  })
  outputOptions(output, "plotDisplayed", suspendWhenHidden=FALSE, priority=-1)

  output$downloadData <- downloadHandler(
    filename=function () {
      "spritedata.csv"
    }
  , content=function (file) {
      write.table(rSprite.plotData, file, row.names=FALSE, col.names=FALSE, sep=",")
    }
  )

  output$help <- renderUI({
    input$go                  # creates a dependency on the Go button

    helpText <- ""            # Unless the user clicked Help, we will clear any existing help text.
    if (input$help > rSprite.prevHelp) {    # user clicked the Help button
      rSprite.prevHelp <<- input$help
      helpText <- HTML(paste(rSprite.helpText, collapse=""))
    }

    isolate({
      helpText
    })
  })

  output$plot <- renderPlot({
    input$help                          # creates a dependency on the Help button
    rSprite.plotData <<- c()

    if (input$go > rSprite.prevGo) {    # user clicked the Go button
      rSprite.prevGo <<- input$go
    }
    else {
      return()           # this clears the plot area (which conveniently allows the help text to show)
    }

    isolate({
      N <- input$N
      tMean <- input$tMean
      tSD <- input$tSD
      scaleMin <- input$scaleMin
      scaleMax <- input$scaleMax
      dp <- input$dp
      gridSize <- sqrt(as.numeric(input$gridSize))

      sample <- reactiveSample()
      if (length(sample$rows) > 0) {
        if (    (gridSize == 10)
             && (session$clientData$url_hostname == "127.0.0.1")        # On developer's local screen...
           ) {                                                          # ... don't show 10x10 grid...
          rSprite.message("Skipping rendering", shinyNow=TRUE)          # ... to speed up generation of test data.
        }
        else {
          gridSize <- floor(sqrt(nrow(sample$rows)) + 0.999)
          rSprite.buildCharts(sample, scaleMin, scaleMax, gridSize)
          rSprite.message("Rendering...", shinyNow=TRUE)
        }

        rSprite.plotData <<- sample$rows
      }

      rSprite.shinyMessages(rSprite.messageList)
      rSprite.messageList <<- list()
    })
  }, height=function () {
    min(session$clientData$output_plot_width, 780)
  }, width=1200)
}
ui <- fluidPage(
  titlePanel("rSPRITE beta 0.14")
, sidebarLayout(
    position="left"
  , sidebarPanel(
      width=2
    , numericInput(inputId="scaleMin", label="Minimum scale value", value=scaleMin, min=-20, max=1, step=1)
    , numericInput(inputId="scaleMax", label="Maximum scale value", value=scaleMax, min=2, max=50, step=1)
    , numericInput(inputId="N", label="Sample size", value=N, min=2, max=1000, step=1)
    , numericInput(inputId="tMean", label="Target mean", value=round(tMean, dp), min=scaleMin, max=scaleMax, step=dstep)
    , numericInput(inputId="tSD", label="Target SD", value=round(tSD, dp), min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)
    , numericInput(inputId="dp", label="Decimal places", value=dp, min=1, max=3, step=1)
    , selectInput(inputId="gridSize", label="Number of results", choices=(c(1:10) ^ 2), selected=9)
    , fluidRow(
        column(
          6
        , textInput(inputId="fixedResponse", label="Fixed value", value=fixedValue)
      )
      , column(
          6
        , textInput(inputId="fixedCount", label="Fixed count", value=fixedCount)
      )
    )
    , checkboxInput(inputId="fixedSeed", label="Use fixed seed", value=fixedSeed)
    , fluidRow(
        column(
          6
        , actionButton(inputId="go", label="Go!")
        )
      , column(
          6
        , actionButton(inputId="help", label="Help")
        )
      )
    , conditionalPanel(
        condition="output.plotDisplayed"
      , br()
      , downloadLink(outputId="downloadData", label="Download data")
    )
  )
  , mainPanel(
      withSpinner(
        plotOutput(outputId="plot", width="100%"), type=5
      )
    , absolutePanel(        # allows help panel to overlay plot panel
        top="50"
      , textOutput(outputId="dummy")
      , htmlOutput(outputId="help")
      )
    )
  )
, tags$head(
    tags$style(
      HTML(".shiny-notification { position:relative; bottom:2px; left:-200px; width:125% }")
    )
  )
)

if (1) {
  shinyApp(ui=ui, server=server)
}
