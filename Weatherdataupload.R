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
#trading stops on weekends, so weekends will carry the latest value after the merge of datasets

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
#replacing NA's in currency data with the last value 
newdf$Price <- na.locf(newdf$Price)

#transforming precipitation into factors - 'rain' and 'dry'
newdf$PRCP[newdf$PRCP > 0] <- 1
newdf$PRCP <- as.factor(newdf$PRCP)


#running a seasonal decomposition on weather as it is a highly seasonal feature
tavg.ts <- ts(newdf$TAVG)
tavg.msts <- msts.transform(tavg.ts,2014,001,yearly)
tavg.decomp <- decomp.msts(tavg.msts)

autoplot(tavg.decomp) + ylab("Daily Value") + xlab("Year")

Temperature  <- as.data.frame(tavg.decomp)
Temperature$Date <- newdf$Date
#merge datasets again
newdf <- merge(newdf, Temperature, by.x = "Date", by.y = "Date", all.x = TRUE )
#not sure if this data should be normalised

#normalise currency price
newdf$currNorm <- normalize(newdf$Price, method="range", range = c(1,10))

#change days of week and month as factors
newdf$DoW <- as.factor(newdf$DoW)
newdf$MonthFactor <- as.factor(newdf$MonthFactor)

summary(newdf)


















