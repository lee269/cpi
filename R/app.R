library(shiny)
library(dplyr)
library(ggplot2)
library(here)

source(here("R", "cdid_chart.R"))
mm23 <- readRDS(here("data", "tidy", "mm23.rds"))
data <- mm23$data

ui <- fluidPage(
  # Global options on top row
  fluidRow(
   column(2,
          selectInput("period",
                      "Frequency:",
                      choices = list(Month =  "M", Quarter = "Q", Year =  "Y"),
                      selected = "M")
          ),
   column(2,
          dateInput(inputId = "startdate",
                    label = "Start date:",
                    value = "2020-01-01")
          )
  ),
  fluidRow(
    # Local options on left column
    column(4,
           selectInput("series",
                       "Choose series",
                       choices = setNames(mm23$cpih_ann_rate_cdids$cdid, mm23$cpih_ann_rate_cdids$title),multiple = TRUE)
    ),
    # Chart area in right column
    column(8,
           plotOutput("chart"))
    
  )
)

server <- function(input, output, session) {

  output$chart <- renderPlot({
    x <- cdid_chart(data, 
                    cdids = input$series, 
                    freq = input$period, 
                    start_date = input$startdate)
    x
  })
}

shinyApp(ui, server)