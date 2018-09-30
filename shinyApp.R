#!/usr/bin/env Rscript
#shinyApp

library(shiny)

ui <- fluidPage(
  
  #uploading file
  fileInput(inputId = "sales_dat", label = "Upload the sales data csv file"),
  
  dateInput("start_date", label = "the date of the first observation"),
  
  #plotOutput("graph"),
  textOutput("contents")
  #tableOutput("table")
)

server <- function(input, output) {
  read_file <- reactive(
              req(input$sales_dat),
              SalesData <- as.data.frame(read.csv(input$sales_dat$datapath)),
              start_date <- input$start_date
              )
  
  output$contents <- renderPrint({
                 print(summary(SalesData))
              })
  
  
  'output$graph <- renderPlot({
    #all code goes here
    autoplot(mstsVal) 
  })'
  #output$text <- renderText({
    
  #})

  'output$table <- renderTable({
    
  #})'
  
  
  
}

shinyApp(ui, server)