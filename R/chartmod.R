chartUI <- function(id, cdids) {
  fluidRow(
      column(4,
         selectInput(NS(id, "cdid"), label = "Select series:", choices = cdids, multiple = TRUE),
         downloadButton(NS(id, "downloaddata"), "Download")
             ),
      column(8,
         tabsetPanel(
               tabPanel("Chart", plotOutput(NS(id, "chart"))),
               tabPanel("Data", tableOutput(NS(id, "data")))
                    )
        
             )
           )
}


# parameters are period in the form M Q or Y
# start date for charts
# both reactive
chartServer <- function(id, period, date) {
  stopifnot(is.reactive(period))
  stopifnot(is.reactive(date))
  
  moduleServer(id, function(input, output, session){
    
    data <- reactive({
      cdid_chart(appdata$data, 
                      cdids = input$cdid, 
                      freq = period(),
                      start_date = date())
      
    })
    
    output$chart <- renderPlot({
      data()$chart
    })

    output$data <- renderTable({
      data()$data
    })
    
    output$downloaddata <- downloadHandler(
      filename = function() {
        paste("data-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data()$data, file)
      }
    )
        
  })
}


chartApp <- function() {
  x <- sample_n(appdata$data,10)
  cdids <- setNames(x$cdid, x$title)
  
  ui <- fluidPage(
    selectInput("time",
                "Frequency:",
                choices = list(Month =  "M", Quarter = "Q", Year =  "Y"),
                selected = "M"),
    dateInput(inputId = "startdate",
              label = "Start date:",
              value = "2020-01-01"),
    chartUI("chart1", cdids)
  )
  server <- function(input, output, session) {
    chartServer("chart1", period = reactive(input$time), date = reactive(input$startdate))
  }
  
  shinyApp(ui, server)
  
  
}
