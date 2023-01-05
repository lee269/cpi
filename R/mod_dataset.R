#' Title
#'
#' @param id 
#' @param cdids 
#'
#' @return
#' @export
#'
#' @examples
datasetInput <- function(id, cdids) {
  # possibly useful
  # https://stackoverflow.com/questions/20637248/shiny-4-small-textinput-boxes-side-by-side
  tagList(
    selectInput(NS(id, "cdid"), label = "Select series:", choices = cdids, multiple = TRUE),
    # selectInput(NS(id, "period"), "Frequency:",
    #             choices = list(Month =  "M",
    #                            Quarter = "Q",
    #                            Year =  "Y"),
    #             selected = "M"),
    dateInput(NS(id, "startdate"), label = "Start date:", value = "2020-01-01"),
    downloadButton(NS(id, "downloaddata"), label = "Download!")
  )
}


#' Title
#'
#' @param id 
#' @param rawdata 
#'
#' @return
#' @export
#'
#' @examples
datasetServer <- function(id, rawdata) {
  moduleServer(id, function(input, output, session) {
    data <-reactive({
       rawdata %>% 
        filter(cdid %in% input$cdid & date >= input$startdate)
    })
    
    output$downloaddata <- downloadHandler(
      filename = function() {
        paste("data-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )
    
    return(data)
  })
}


datasetApp <- function() {
  
  l <- sample_n(appdata$data,10)
  cdids <- setNames(l$cdid, l$title)
  rawdata <- appdata$data
  
  ui <- fluidPage(
    fluidRow(
      column(4, datasetInput("stuff", cdids = cdids)),
      column(8, tableOutput("table"))
    )
  )
  
  server <- function(input, output, session) {
    data <- datasetServer("stuff", rawdata)
    output$table <- renderTable(data())
  }
  
  shinyApp(ui, server)
}