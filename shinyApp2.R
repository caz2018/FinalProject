
#!/usr/bin/env Rscript
#shinyApp

library(readr)
library(stats)
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(forecast)
library(BBmisc)
library(zoo)
library(shiny)

dat_proc <- function(dat){
  #function to transform a vector into a time series object. Need the day of the year as a number from 1 to 365/366.
  #freq is the data interval, for daily data use yearly.
  ts.transform <- function(univariateseries, YYYY,DDD, freq){
    x = ts(univariateseries, start = c(YYYY,DDD) , frequency = freq)
    return(x)
  }
  
  #function for transforming a time series object into a msts, which is a multiple-series time series
  
  msts.transform <- function(tsobj, YYYY,DDD,s1=NULL,s2=NULL,s3=NULL){
    require(forecast)
    a = msts(tsobj, start = c(2014,001), seasonal.periods=c(s1,s2,s3))
    return(a)
  }
  
  #function to decompose both single-seasonal and multi-seasonal time-series
  decomp.msts <- function(mstsobj){
    require(forecast)
    xy1 = mstl(mstsobj, iterate = 3)
    return(xy1)
  }
  
  SalesData <-  as.data.frame(dat)
  require(lubridate)
  date <- ymd(SalesData$Date)
  SalesData$Date <- date
  SalesData$DoW <- as.factor(weekdays.Date(date))
  SalesData$WeekNum <- recode(SalesData$DoW,'Monday'=1,'Tuesday'=2, 'Wednesday'=3,'Thursday'=4,
                              'Friday'=5,'Saturday'=6,'Sunday'=7)
  
  ### add the number of the month
  SalesData$MonthNum <- month(SalesData$Date)
  SalesData$MonthFactor <- as.factor(recode(SalesData$MonthNum, '1'="Jan", '2'= "Feb", '3'= "Mar", '4'= "Apr", '5'= "May",
                                            '6'= "Jun", '7'="Jul",'8' = "Aug", '9'= "Sep", '10' = "Oct", '11'= "Nov", '12' = "Dec"))
  
  #setting the seasonality variables
  weekly <- 7
  monthly <- 30.5
  yearly <- 364.25
  
  #adding a normalized version of daily sales values to the dataframe, using a 1 to 10 range to avoid zero values
  SalesData$ValNorm <- normalize(SalesData$Total_Daily_Value , method="range", range=c(1,10))

  tsVal <- ts.transform(SalesData$Total_Daily_Value, 2014,001, yearly)
  
  #By default the Full Daily Sales Value will be used for ease of visualisation, with yearly only yearly and weekly pattern.
  mstsVal <- msts.transform(tsVal, 2014,001, weekly, yearly)
  decVals <- decomp.msts(mstsVal)
  
  
  #generating a decomposition of the normalised Daily sales values for use in the analysis in conjunction with other variables.
  salesNormmsts_mwy <- msts(tsValNorm, seasonal.periods=c(7,30.5,365.25))
  xnorm_wmy <- mstl(salesNormmsts_mwy, lambda = "auto", iterate = 3)
  autoplot(xnorm_wmy)
  summary(xnorm_wmy)
  #generating a dataset with decomposed values to use with GAM
  dat_decomp <- as.data.frame(xnorm_wmy)
  dat_decomp$Date <- date
  
  #mergin the decomposition to the original data frame
  SalesData<- merge(SalesData, dat_decomp, by.x = "Date", by.y = "Date", all.x = TRUE)
  
  
  autoplot.zoo(decVals) +ylab("Daily Value")+ xlab("Year")
  summary(SalesData)
  
}



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tableOutput("contents"),
      plotOutput("sales"),
      textOutput("summary")
    )
  )
)




server <- function(input, output, session) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    a <-read.csv(inFile$datapath, header = input$header)
  })
  
  processing_data   <- reactive({ dat_proc(as.data.frame(a)) })
  
  
}

shinyApp(ui, server)