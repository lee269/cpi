library(shiny)
library(dplyr)
library(ggplot2)
library(here)

source(here("R", "cdid_chart.R"))
# mm23 <- readRDS(here("data", "tidy", "mm23.rds"))
# data <- mm23$data

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data

cpih_ann_rate_cdids <- data %>% 
  filter(category == "CPIH Annual rate (%)") %>% 
  select(cdid, title, level) %>%
  unique() %>%
  arrange(title, level)


ui <- navbarPage(title = "Inflation Explorer",
  tabPanel("CPIH Annual rate",
# Global options on top row-----------------------------------------------------
  fluidRow(
   column(1,
          selectInput("period",
                      "Frequency:",
                      choices = list(Month =  "M", Quarter = "Q", Year =  "Y"),
                      selected = "M")
          ),
   column(2,
          dateInput(inputId = "startdate",
                    label = "Start date:",
                    value = "2020-01-01")
          ),
   column(2,
          "|Facet box|"
          )
  ),
# main body---------------------------------------------------------------------
  fluidRow(
    # Local options on left column----------------------------------------------
    column(4,
           selectInput("series",
                       "Choose series",
                       choices = setNames(cpih_ann_rate_cdids$cdid,
                                          cpih_ann_rate_cdids$title), 
                       multiple = TRUE),
           "|Download button|"
    ),
    # Chart area in right column------------------------------------------------
    # column(4,
    #        plotOutput("chart")),
    column(8, tabsetPanel(type = "pills", tabPanel("text1", plotOutput("chart")),tabPanel("text2")))
    
  )
  )
)

server <- function(input, output, session) {

  output$chart <- renderPlot({
    x <- cdid_chart(data, 
                    cdids = input$series, 
                    freq = input$period, 
                    start_date = input$startdate)
    x$chart
  })
}

shinyApp(ui, server)