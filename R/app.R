library(shiny)
library(dplyr)
library(ggplot2)
library(here)

source(here("R", "chartmod.R"))
source(here("R", "mod_dataset.R"))
source(here("R", "helpers.R"))

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data

ann_rate <- cdid_list(data, "CPIH Annual rate (%)")
mth_rate <- cdid_list(data, "CPIH Monthly rate (%)")
rpi_price <- cdid_list(data, "RPI Average price (pence)")
cont_rate_cdids <- cdid_list(data, "CPIH contribution to all items annual rate")

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
     column(2,
            paste("Next data:", format(appdata$next_release, "%d %b %Y")))
   ),

# Pages---------------------------------------------------------------------
  tabPanel("CPIH Annual rate",
    chartUI("annrate", ann_rate)
  ),
  tabPanel("RPI prices",
           chartUI("rpiprice", rpi_price)
  ),
  navbarMenu(title = "Other",
             tabPanel("Contribution to annual rate",
                      fluidRow(
                      column(4,datasetInput("cont_rate_sel", cont_rate_cdids)),
                      column(8, plotOutput("cont_rate_cht"))
                      )
                      ),
             tabPanel("CPIH Monthly rate",
                chartUI("mthrate", mth_rate)         
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
  
  cont_rate_data <- datasetServer("cont_rate_sel", data)
  
  output$cont_rate_cht <- renderPlot({
   pct_line_chart(cont_rate_data(), facet = input$facet) 
               })
  
}

shinyApp(ui, server)