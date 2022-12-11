library(shiny)
library(dplyr)
library(ggplot2)

mm23 <- readRDS(here("data", "tidy", "mm23.rds"))
data <- mm23$data

ui <- fluidPage(
  fluidRow(
   column(2,
          selectInput("period",
                      "Choose period",
                      choices = c("M", "Q", "Y"),
                      selected = "M"),
          selectInput("series",
                      "Choose series",
                      choices = setNames(mm23$cpih_ann_rate_cdids$cdid, mm23$cpih_ann_rate_cdids$title)),
          ),
   column(8,
          plotOutput("chart"))
  ),
)

server <- function(input, output, session) {
  output$chart <- renderPlot({
    data %>% 
      filter(cdid == input$series & period == input$period) %>% 
      ggplot() +
      geom_line(aes(x = date, y = value))
  })
}

shinyApp(ui, server)