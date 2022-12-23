chartUI <- function(id) {
  fluidRow(
  column(4,
    selectInput(NS(id, "cdid"), label = "cdid", choices = c("L55P", "L55O"), multiple = TRUE)
    ),
  column(8,
    plotOutput(NS(id, "chart"))
  )
  )
}



chartServer <- function(id) {
  moduleServer(id, function(input, output, session){
    output$chart <- renderPlot({
      x <- cdid_chart(appdata$data, 
                      cdids = input$cdid, 
                      freq = "M")
      x$chart
    })
    
  })
}


chartApp <- function() {
  ui <- fluidPage(
    chartUI("chart1")
  )
  server <- function(input, output, session) {
    chartServer("chart1")
  }
  
  shinyApp(ui, server)
  
  
}
