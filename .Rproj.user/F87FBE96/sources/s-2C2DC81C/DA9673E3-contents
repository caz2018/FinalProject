#clean up env
#rm(list = ls())

#loading sales data with dates and formatting the date field as date in R
library(readr)
SalesData <-  as.data.frame(read_csv("AllHistoricalData.csv")) 
dates <- as.Date(SalesData$Date)
SalesData$Date <- dates

library(readr)
WeatherData <- read_csv("LondonWeather2013-2018.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)
WeatherData$DATE <- weatherdates

#need to transform sales data into a ts (time series) object

library(xts)
#DailySales <- xts(SalesData$Date, order.by = SalesData$Date) didn't work as expected
test <- ts(SalesData$`Tickets Sold`, start = 1, frequency = 7)
is.ts(test)
test


#fitting a STL decomposition to the sales data
require(graphics)

fit <- stl(test, s.window=12, robust=TRUE)
plot(fit)


ts(1:40, frequency = 1, start = 1) 
print( ts(1:40, frequency = 7, start = c(1959)), calendar = TRUE)
