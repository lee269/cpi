library(shiny)
library(dplyr)
library(ggplot2)
library(here)

source(here("R", "chartmod.R"))
source(here("R", "helpers.R"))

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data

ann_rate <- cdid_list(data, "CPIH Annual rate (%)")
mth_rate <- cdid_list(data, "CPIH Monthly rate (%)")
rpi_price <- cdid_list(data, "RPI Average price (pence)")



ui <- navbarPage(title = "Inflation Explorer",
                 theme = bslib::bs_theme(bootswatch = "minty"),
# Global options on top row-----------------------------------------------------
   fluidRow(
     column(2,
            selectInput("period",
                        "Frequency:",
                        choices = list(Month =  "M",
                                       Quarter = "Q",
                                       Year =  "Y"),
                        selected = "M")
     ),
     column(2,
            dateInput(inputId = "startdate",
                      label = "Start date:",
                      value = "2020-01-01")
     ),
     column(2,
            checkboxInput(inputId = "facet",
                          label = "Separate charts",
                          value = FALSE)
     ),
     column(3,
            paste("Next data:", format(appdata$next_release, "%d %b %Y")))
   ),

# Pages---------------------------------------------------------------------
  navbarMenu(title = "CPIH",
  tabPanel("CPIH Annual rate",
    chartUI("annrate", ann_rate)
  ),
  tabPanel("CPIH Monthly rate",
    chartUI("mthrate", mth_rate)         
  ),
  tabPanel("RPI prices",
           chartUI("rpiprice", rpi_price)
  )
  
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  chartServer("annrate",
              rawdata = data, 
              period = reactive(input$period),
              date = reactive(input$startdate),
              facet = reactive(input$facet))
  chartServer("mthrate",
              rawdata = data,
              period = reactive("M"),
              date = reactive(input$startdate),
              facet = reactive(input$facet))
  chartServer("rpiprice", 
              rawdata = data, 
              period = reactive(input$period),
              date = reactive(input$startdate),
              facet = reactive(input$facet))
}

shinyApp(ui, server)