library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(here)
library(htmltools)
library(ggbump)

source(here("R", "mod_dataset.R"))
source(here("R", "helpers.R"))

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data %>% filter(period == "M")

ann_rate_cdids <- cdid_list(data, "CPIH Annual rate (%)")
rpi_price_cdids <- cdid_list(data, "RPI Average price (pence)")
cont_rate_cdids <- cdid_list(data, "CPIH contribution to all items annual rate")

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
                           nav("CPIH contribution to annual rate",
                               layout_column_wrap(width = 1/2,
                                                  style = css(grid_template_columns = "1fr 3fr"),
                                                  card(datasetInput("cont_rate_sel", cont_rate_cdids)),
                                                  navs_pill_card(full_screen = TRUE,
                                                                 nav("Chart", plotOutput("cont_rate_cht")),
                                                                 nav("Data", tableOutput("cont_rate_table")),
                                                                 nav("Rank", plotOutput("cont_rate_rank")))
                               )
                               ),
                           nav("Item2")),
                  nav_spacer(),
                  nav_item("sdfkjsd")


)



server <- function(input, output, session) {
  thematic::thematic_shiny()
  # CPIH annual rate
  ann_rate_data <- datasetServer("ann_rate_sel", rawdata = data)
  output$ann_rate_table <- renderTable(ann_rate_data())
  output$ann_rate_cht <- renderPlot(
    pct_line_chart(ann_rate_data(), facet = input$facet) +
      theme(legend.position = "bottom")
  )

  
  # RPI price rate
  rpi_price_data <- datasetServer("rpi_price_sel", rawdata = data)
  output$rpi_price_table <- renderTable(rpi_price_data())
  output$rpi_price_cht <- renderPlot(
    pct_line_chart(rpi_price_data(), facet = input$facet)
  )
  
  cont_rate_data <- datasetServer("cont_rate_sel", rawdata = data)
  
                    
  output$cont_rate_table <- renderTable(cont_rate_data()) 
  output$cont_rate_cht <- renderPlot(
    pct_line_chart(cont_rate_data(), facet = input$facet)
  )

  output$cont_rate_rank <- renderPlot({
    cont_rate_data() %>% 
      group_by(date) %>% 
      mutate(rank = rank(-value),
             rank_text = scales::ordinal(rank(-value))) %>% 
      ggplot() +
      geom_bump(aes(x = date, y = rank, colour = title)) +
      geom_point(aes(x = date, y = rank, colour = title), size = 3) +
      scale_y_reverse(limits = c(12,1), n.breaks = 12) +
      chart_theme
      
    })



}

shinyApp(ui, server)