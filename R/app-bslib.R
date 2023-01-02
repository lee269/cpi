library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(here)
library(htmltools)

source(here("R", "mod_dataset.R"))
source(here("R", "helpers.R"))

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data

ann_rate_cdids <- cdid_list(data, "CPIH Annual rate (%)")
rpi_price_cdids <- cdid_list(data, "RPI Average price (pence)")

hdr <- tagList(checkboxInput("facet", "Facet:"),
               appdata$latest_data)


ui <- page_navbar(title = "Inflation explorer",
                  theme = bs_theme(bootswatch = "minty"),
                  header = hdr,
                  nav("CPIH Annual Rate",
                    layout_column_wrap(width = 1/2,
                      style = css(grid_template_columns = "1fr 3fr"),
                      card(datasetInput("ann_rate_sel", ann_rate_cdids)),
                        navs_pill_card(full_screen = TRUE,
                                       nav("Chart", plotOutput("ann_rate_cht")),
                                       nav("Data", tableOutput("ann_rate_table")))
                        )
                  ),
                  nav("RPI Prices",
                      layout_column_wrap(width = 1/2,
                                         style = css(grid_template_columns = "1fr 3fr"),
                                         card(datasetInput("rpi_price_sel", rpi_price_cdids)),
                                         navs_pill_card(full_screen = TRUE,
                                                        nav("Chart", plotOutput("rpi_price_cht")),
                                                        nav("Data", tableOutput("rpi_price_table")))
                                         )
                      ),
                  nav_menu("Menu",
                           nav("Item"),
                           nav("Item2")),
                  nav_spacer(),
                  nav_item(checkboxInput("f", "facet",width = "100px"))


)



server <- function(input, output, session) {
  thematic::thematic_shiny()
  # CPIH annual rate
  ann_rate_data <- datasetServer("ann_rate_sel", rawdata = data)
  output$ann_rate_table <- renderTable(ann_rate_data())
  output$ann_rate_cht <- renderPlot(
    pct_line_chart(ann_rate_data(), facet = input$facet)
  )

  
  # CPIH annual rate
  rpi_price_data <- datasetServer("rpi_price_sel", rawdata = data)
  output$rpi_price_table <- renderTable(rpi_price_data())
  output$rpi_price_cht <- renderPlot(
    pct_line_chart(rpi_price_data(), facet = input$facet)
  )
  
  
}

shinyApp(ui, server)