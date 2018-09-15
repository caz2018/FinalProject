

library(readr)
WeatherData <- read_csv("LondonWeather2013-2018.csv")
weatherdates <- as.Date(WeatherData$DATE, "%d/%m/%Y")
#View(weatherdates)
WeatherData$DATE <- weatherdates

#merging the 2 dataframes
newdf <- merge(SalesData, WeatherData, by.x = "Date", by.y = "DATE")
#removing columns we do not weed
newdf <- newdf[c("Date","Norm", "PRCP", "TAVG", "TMAX") ]
newdf$Norm <- normalize(newdf$Norm, method="range", range = c(0,4))
newdf$PRCP <- normalize(newdf$PRCP, method="range", range = c(0,4))
newdf$TAVG <- normalize(newdf$TAVG, method="range", range = c(0,4))
newdf$TMAX <- normalize(newdf$TMAX, method="range", range = c(0,4))
summary(newdf)
newdfSummer2018 <- newdf[1770:1825, ]
newdfSummer2017 <- newdf[1410:1465, ]

plot(newdfSummer2017)
plot(newdfSummer2018)

#newdf$TMAX <- normalize(newdf$TMAX, method="standardize")
#newdf$TMIN <- normalize(newdf$TMIN, method="standardize")
require(graphics)

plot(newdfSummer2017)
(z <- line(newdfSummer2017))
abline(coef(z))
## Tukey-Anscombe Plot :
plot(residuals(z) ~ fitted(z), main = deparse(z$call))


l <- line(newdfSummer2017$Norm, newdfSummer2017$PRCP)
library(stats)
#linear model of sales vs precipitation
l2 <- lm(newdfSummer2017$Norm~newdfSummer2017$PRCP)
plot(l2)
plot(residuals(l2)~ fitted(l2, main = deparse(l2$call))) + abline(coef(l2))

#linear model of sales vs averga temperature
l3 <- lm(newdfSummer2017$Norm~newdfSummer2017$TAVG)
plot(l3)
plot(residuals(l3)~ fitted(l3, main = deparse(l3$call))) + abline(coef(l3))

l4 <- lm(newdfSummer2017$Norm~newdfSummer2017$TMAX)
plot(l4)
plot(residuals(l4)~ fitted(l4, main = deparse(l4$call))) + abline(coef(l4))















