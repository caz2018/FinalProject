library(stats)
library(readr)
library(lubridate)
library(ggplot2)
library(forecast)

WeatherData <- read_csv("WeatherDaily.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)
WeatherData$DATE <- weatherdates
WeatherData$PRCP[is.na(WeatherData$PRCP)] <- 0

CurrencyData <- read.csv("CurrencyExchangeDaily.csv")
currencydates <- mdy(CurrencyData$Date)
CurrencyData$Date <- currencydates 


#merging the 2 dataframes
newdf <- merge(SalesData, WeatherData, by.x = "Date", by.y = "DATE")
summary(newdf)
newdf <- merge(newdf, CurrencyData, by.x = "Date", by.y = "Date")
summary(newdf)

#standardizing and normalising the weather data
newdf$PRCP <- normalize(newdf$PRCP, method="range", range = c(1,10))
newdf$TAVG <- normalize(newdf$TAVG, method="range", range = c(1,10))
newdf$PRCP <- normalize(newdf$PRCP, method="range", range = c(1,10))

summary(newdf)


















