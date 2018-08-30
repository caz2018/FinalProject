#clean up env
rm(list = ls())

#loading sales data with dates and formatting the date field as date in R
library(readr)
SalesData <-  as.data.frame(read_csv("AllHistoricalData.csv")) 
dates <- as.Date(SalesData$Date)
SalesData$Date <- dates
library(dplyr)
SalesData <- `row.names<-`(SalesData, SalesData$Date)

library(DescTools)
#reversed the order of the tickets sold because the time series kept using the original 
#order despite using the sort function, don't know why.
#TO DO - check the order of the dates on the given dataset (if min does not equal the value in row_number 1, then reverse)
reversed <- rev(SalesData$`Tickets Sold`)
#removed the first year
reversedsubset <- reversed[365:1864 ]
min(reversedsubset)

#arrange(newdata, newdata$Date)


#need to transform sales data into a ts (time series) object and a msts (multi-seasonal time-series)

#install the forecast package if not yet installed
#install.packages('forecast', dependencies = TRUE)
library(forecast)

timeseries <- ts(reversedsubset , start = c(2013,06) , frequency = 7)
timeseries

salesmsts <- msts(timeseries, seasonal.periods=c(48,336))

#fitting a STL decomposition to the sales data
require(graphics)
x <- mstl(salesmsts)
plot(x)

salesmsts %>%  stlf() %>% autoplot() 



#stlfit <- stl(timeseries, t.window=12, s.window='periodic', robust=TRUE) #s.window > results in a smoother trend
#plot(stlfit)

library(readr)
WeatherData <- read_csv("LondonWeather2013-2018.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)
WeatherData$DATE <- weatherdates


