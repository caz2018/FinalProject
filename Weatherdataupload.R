library(stats)
library(readr)

WeatherData <- read_csv("WeatherDaily.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)
WeatherData$DATE <- weatherdates

#merging the 2 dataframes
newdf <- merge(SalesData, WeatherData, by.x = "Date", by.y = "DATE")
summary(newdf)

#standardizing and normalising the weather data
newdf$PRCP <- normalize(newdf$PRCP, method="range", range = c(1,10))
newdf$TAVG <- normalize(newdf$TAVG, method="range", range = c(1,10))
newdf$PRCP <- normalize(newdf$PRCP, method="range", range = c(1,10))

summary(newdf)


















