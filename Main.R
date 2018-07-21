#clean up env
rm(list = ls())

#loading sales data with dates and formatting the date field as date in R
library(readr)
SalesData <- read_csv("AllHistoricalData.csv")
dates <- as.Date(SalesData$Date)
SalesData$Date <- dates

library(readr)
WeatherData <- read_csv("LondonWeather2013-2018.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)

