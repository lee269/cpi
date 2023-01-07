library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(here)
library(htmltools)
library(ggbump)
library(reactable)

source(here("R", "mod_dataset.R"))
source(here("R", "helpers.R"))

appdata <- readRDS(here("data", "tidy", "appdata.rds"))
data <- appdata$data %>% filter(period == "M")
rank_data <- data %>% 
  filter(category == "CPIH contribution to all items annual rate") %>% 
  group_by(date) %>% 
  mutate(rank = rank(-value, ties.method = "random"),
         rank_text = scales::ordinal(rank(-value)),
         date = as.Date(date)) %>% 
  select(date, cdid, rank, rank_text)


ann_rate_cdids <- cdid_list(data, "CPIH Annual rate (%)")
rpi_price_cdids <- cdid_list(data, "RPI Average price (pence)")
cont_rate_cdids <- cdid_list(data, "CPIH contribution to all items annual rate")

hdr <- fluidRow(
                checkboxInput("facet", "Separate charts")
               # radioButtons("period", "Frequency (if needed):",
               #             choices = list(Month =  "M",
               #                            Quarter = "Q",
               #                            Year =  "Y"),
               #             selected = "M",
               #             inline = TRUE)
                )


ui <- page_navbar(title = "Inflation explorer",
                  theme = bs_theme(bootswatch = "minty"),
                  header = hdr,
                  nav("CPIH Annual Rate",
                    layout_column_wrap(width = 1/2,
                      style = css(grid_template_columns = "1fr 3fr"),
                      card(height = "100%",
                           card_body_fill(datasetInput("ann_rate_sel", ann_rate_cdids))
                           ),
                        navs_pill_card(full_screen = TRUE,
                                       card_header(class = "bg-primary", "CPIH Annual Rate"),
                                       nav("Chart", plotOutput("ann_rate_cht")),
                                       nav("Data", reactableOutput("ann_rate_table")))
                        )
                  ),
                  nav("RPI Prices",
                      layout_column_wrap(width = 1/2,
                                         style = css(grid_template_columns = "1fr 3fr"),
                                         card(height = "100%",
                                              card_body_fill(datasetInput("rpi_price_sel", rpi_price_cdids))
                                              ),
                                         navs_pill_card(full_screen = TRUE,
                                                        card_header(class = "bg-primary", "RPI Average Prices"),
                                                        nav("Chart", plotOutput("rpi_price_cht")),
                                                        nav("Data", reactableOutput("rpi_price_table")))
                                         )
                      ),
                  nav_menu("Menu",
                           nav("CPIH contribution to annual rate",
                               layout_column_wrap(width = 1/2,
                                                  style = css(grid_template_columns = "1fr 3fr"),
                                                  card(height = "100%",
                                                       card_body_fill(datasetInput("cont_rate_sel", cont_rate_cdids))
                                                       ),
                                                  navs_pill_card(full_screen = TRUE,
                                                                 card_header(class = "bg-primary", "CPIH Contribution to annual rate"),
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
  output$ann_rate_table <- renderReactable({ann_rate_data() %>% 
                                          select(date, title, value, unit, cdid) %>%
                                           reactable(columns = list(
                                             date = colDef(name = "Date", minWidth = 35),
                                             title = colDef(name = "Series", minWidth = 140),
                                             value = colDef(name = "Value", minWidth = 25),
                                             unit = colDef(name = "Unit", minWidth = 25),
                                             cdid = colDef(name = "CDID", minWidth = 25)
                                           ),
                                                     showPageSizeOptions = TRUE,
                                                     pageSizeOptions = c(5,10,25),
                                                     defaultPageSize = 5,
                                                     filterable = TRUE)
    })
  output$ann_rate_cht <- renderPlot(
    pct_line_chart(ann_rate_data(), facet = input$facet) +
      theme(legend.position = "bottom")
  )

  
  # RPI price rate
  rpi_price_data <- datasetServer("rpi_price_sel", rawdata = data)
  output$rpi_price_table <- renderReactable({rpi_price_data() %>% 
      select(date, title, value, unit, cdid) %>%
      reactable(columns = list(
        date = colDef(name = "Date", minWidth = 35),
        title = colDef(name = "Series", minWidth = 140),
        value = colDef(name = "Value", minWidth = 25),
        unit = colDef(name = "Unit", minWidth = 25),
        cdid = colDef(name = "CDID", minWidth = 25)
      ),
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5,10,25),
      defaultPageSize = 5,
      filterable = TRUE)
})
  
  output$rpi_price_cht <- renderPlot(
    pct_line_chart(rpi_price_data(), facet = input$facet)
  )
  
  
  # CPI  contribution to annual rate
  cont_rate_data <- datasetServer("cont_rate_sel", rawdata = data)
  
  output$cont_rate_table <- renderTable({
    cont_rate_data() %>% 
      left_join(rank_data, by = c("date", "cdid"))
    }) 
  output$cont_rate_cht <- renderPlot({
    # pct_line_chart(cont_rate_data(), facet = input$facet)
    cht <- cont_rate_data() %>% 
      ggplot() +
      geom_col(aes(x = date, y = value, fill = title)) +
      chart_theme
    
    if(input$facet){
    cht +
        facet_wrap(vars(title)) +
        theme(legend.position = "none")
    } else {
      cht
    }
    
  })

  output$cont_rate_rank <- renderPlot({
    df <- cont_rate_data() %>% 
      left_join(rank_data, by = c("date", "cdid")) %>% 
      mutate(date = as.Date(date))
    
      ggplot(df, aes(x = date, y = rank, colour = title)) +
      geom_bump() +
      geom_point(size = 3) +
      geom_text(data = df %>% filter(date == max(date)),
                aes(x = date + 10, label = title),
                hjust = 0) +
      geom_text(data = df %>% filter(date == min(date)),
                aes(x = date - 10, label = title),
                hjust = 1,) +
      scale_y_reverse(limits = c(12,1), n.breaks = 12) +
      scale_x_date(expand = expansion(mult = c(0.3, 0.3))) +
      chart_theme +
      theme(legend.position = "none")
      
    })



}

shinyApp(ui, server)