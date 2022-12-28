library(shiny)
library(dplyr)
library(ggplot2)
library(here)

source(here("R", "cdid_chart.R"))
source(here("R", "chartmod.R"))
# mm23 <- readRDS(here("data", "tidy", "mm23.rds"))
# data <- mm23$data

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data

cpih_ann_rate_cdids <- data %>% 
  filter(category == "CPIH Annual rate (%)") %>% 
  select(cdid, title, level) %>%
  unique() %>%
  arrange(title, level) 

ann_rate <- setNames(cpih_ann_rate_cdids$cdid,
                     cpih_ann_rate_cdids$title)

cpih_mth_rate_cdids <- data %>% 
  filter(category == "CPIH Monthly rate (%)") %>% 
  select(cdid, title, level) %>%
  unique() %>%
  arrange(title, level)
  
mth_rate <- setNames(cpih_mth_rate_cdids$cdid,
                     cpih_mth_rate_cdids$title)


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
            "|Facet box|"
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
  )
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  chartServer("annrate", period = reactive(input$period), date = reactive(input$startdate))
  chartServer("mthrate", period = reactive("M"), date = reactive(input$startdate))
}

shinyApp(ui, server)