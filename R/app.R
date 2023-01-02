library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(here)

# source(here("R", "chartmod.R"))
source(here("R", "mod_dataset.R"))
source(here("R", "helpers.R"))

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data

ann_rate_cdids <- cdid_list(data, "CPIH Annual rate (%)")
mth_rate_cdids <- cdid_list(data, "CPIH Monthly rate (%)")
rpi_price_cdids <- cdid_list(data, "RPI Average price (pence)")
cont_rate_cdids <- cdid_list(data, "CPIH contribution to all items annual rate")

ui <- navbarPage(title = "Inflation Explorer",
                 theme = bslib::bs_theme(bootswatch = "minty"),
# Global options on top row-----------------------------------------------------
   fluidRow(
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
           fluidRow(
             column(3,datasetInput("ann_rate_sel", ann_rate_cdids)),
             column(9, plotOutput("ann_rate_cht"))
           )
  ),
  tabPanel("RPI prices",
           fluidRow(
             column(3,datasetInput("rpi_price_sel", rpi_price_cdids)),
             column(9, plotOutput("rpi_price_cht"))
           )
  ),
  navbarMenu(title = "Other",
             tabPanel("Contribution to annual rate",
                      fluidRow(
                      column(3,datasetInput("cont_rate_sel", cont_rate_cdids)),
                      column(9, plotOutput("cont_rate_cht"))
                      )
                      ),
             tabPanel("CPIH Monthly rate",
                      fluidRow(
                        column(3,datasetInput("mth_rate_sel", mth_rate_cdids)),
                        column(9, plotOutput("mth_rate_cht"))
                      )
                     )
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()

  ann_rate_data <- datasetServer("ann_rate_sel", rawdata = data)
  rpi_price_data <- datasetServer("rpi_price_sel", rawdata = data)
  cont_rate_data <- datasetServer("cont_rate_sel", rawdata = data)
  mth_rate_data <- datasetServer("mth_rate_sel", rawdata = data)
  
  output$ann_rate_cht <- renderPlot(
    pct_line_chart(ann_rate_data(), facet = input$facet)
  )

  output$rpi_price_cht <- renderPlot(
    pct_line_chart(rpi_price_data(), facet = input$facet)
  )

  output$cont_rate_cht <- renderPlot({
   pct_line_chart(cont_rate_data(), facet = input$facet) 
               })

  output$mth_rate_cht <- renderPlot({
    pct_line_chart(mth_rate_data(), facet = input$facet) 
  })
    
}

shinyApp(ui, server)