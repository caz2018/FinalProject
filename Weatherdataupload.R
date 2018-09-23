library(stats)
library(readr)
library(lubridate)
library(ggplot2)
library(forecast)
library(zoo)

WeatherData <- read_csv("WeatherDaily.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)
WeatherData$DATE <- weatherdates
#remove NA's
WeatherData$PRCP[is.na(WeatherData$PRCP)] <- 0


CurrencyData <- read.csv("CurrencyExchangeDaily.csv")
currencydates <- mdy(CurrencyData$Date)
CurrencyData$Date <- currencydates
#trading stops on weekends, so NA's will carry the latest value

#removing data we don't need
CurrencyData$Open <- NULL
CurrencyData$High <- NULL
CurrencyData$Low <- NULL
CurrencyData$Change.. <- NULL


#merging weather and sales dataframes
newdf <- merge(SalesData, WeatherData, by.x = "Date", by.y = "DATE", all.x = TRUE)
summary(newdf)
#adding currency data
newdf <- merge(newdf, CurrencyData, by.x = "Date", by.y = "Date", all.x = TRUE)
summary(newdf)
#dealing with NA's in currency data 
newdf$Price <- na.locf(newdf$Price)
#adding decomposition data
newdf <- merge(newdf, dat_decomp, by.x = "Date", by.y = "Date", all.x = TRUE)
summary(newdf)


#standardizing and normalising the weather and currency data
newdf$PRCP[newdf$PRCP > 0] <- 1
newdf$PRCP <- as.factor(newdf$PRCP)
#change temp as factor according to the weekly average

newdf$TAVG <- normalize(newdf$TAVG, method="range", range = c(1,10))

#normalise price
newdf$Price <- normalize(newdf$Price, method="range", range = c(1,10))

#change days of week and month as factors

summary(newdf)


















