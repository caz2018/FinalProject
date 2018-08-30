
#clean up env
rm(list = ls())

library(readr)
WeatherData <- read_csv("LondonWeather2013-2018.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)
WeatherData$DATE <- weatherdates

#merging the 2 dataframes
newdf <- merge(SalesData, WeatherData, by.x = "Date", by.y = "DATE" , all = TRUE)
#removing columns we do not weed
colnames(newdf)

newdf <- newdf[c("Date","Tickets Sold", "PRCP", "TAVG","TMAX","TMIN") ]
