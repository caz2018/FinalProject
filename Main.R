#clean up env
rm(list = ls())

#loading sales data with dates and formatting the date field as date in R
library(readr)
SalesData <-  as.data.frame(read_csv("AllHistoricalData.csv")) 
summary(SalesData)
dates <- as.character(SalesData$Date)
dates <- as.Date(SalesData$Date, "%d/%m/%Y")
SalesData$Date <- dates
#adding a log version of tickets sold to the dataframe
SalesData$Norm <- normalize(SalesData$TicketsSold, method="standardize")


library(DescTools)
#reversed the order of the tickets sold because the time series kept using the original 
#order despite using the sort function.
#TO DO - check the order of the dates on the given dataset (if min does not equal the value in row_number 1, then reverse)
reversed <- rev(SalesData$`TicketsSold`)
#removed the first year because this year was too low in comparison to others
reversedlast <- reversed[365:1827 ]
reversedfirst <- reversed[1:365 ]

#normalizing and scaling the data

library(BBmisc)
norm.reversed <- normalize(reversed, method = "standardize")
summary(norm.reversed)
log.reversed <- log(reversed)
summary(log.reversed)

#removing infinites from data
log.reversed[is.infinite(log.reversed)] <- 0


#need to transform sales data into a ts (time series) object and a msts (multi-seasonal time-series)

#install the forecast package if not yet installed
#install.packages('forecast', dependencies = TRUE)
library(forecast)

timeseries1 <- ts(reversedfirst, frequency=365, start=c(2014, 182)) 
timeseries2 <- ts(reversedlast , start = c(2014, 182) , frequency = 365)
timeseriesall <- ts( reversed, start = c(2013,182) , frequency = 365)

salesmsts1 <- msts(timeseries1, seasonal.periods=c(7))
salesmsts2 <- msts(timeseries2, seasonal.periods=c(365.25))
salesmsts_wy <- msts(timeseries2, seasonal.periods=c(7,365.25))
salesmsts_wmy <- msts(timeseries2, seasonal.periods=c(7,30.5,365.25))

#fitting a STL decomposition to the sales data

require(graphics)

require(ggplot2)
x <- mstl(salesmsts1)
xp <- autoplot(x) +
  ylab("Sales volume") + xlab("Week") +
  scale_x_continuous(minor_breaks = seq(1,30))
p2 <- autoplot(window(x, start=20, end=50)) +
  ylab("Sales volume") + xlab("Week") 
gridExtra::grid.arrange(xp,p2)

#plot for yearly trend only
x1 <- mstl(salesmsts2)
xp <- autoplot(x1) +
  ylab("Sales volume") + xlab("Year") +
  scale_x_continuous(minor_breaks = seq(1,30))
p2 <- autoplot(window(x1, start=2016, end=2018)) +
  ylab("Sales volume") + xlab("Quarter") 
gridExtra::grid.arrange(xp,p2)

#plot for year and weekly trends
x2 <- mstl(salesmsts_wy)
xp <- autoplot(x2) +
  ylab("Sales volume") + xlab("Year") +
  scale_x_continuous(minor_breaks = seq(1,30))
p2 <- autoplot(window(x2, start=2016, end=2018)) +
  ylab("Sales volume") + xlab("Quarter") 
gridExtra::grid.arrange(xp,p2)

#plot for weekly, monthly and yearly trends
x3 <- mstl(salesmsts_wmy)
xp <- autoplot(x3) +
  ylab("Sales volume") + xlab("Year") +
  scale_x_continuous(minor_breaks = seq(1,30))
p2 <- autoplot(window(x3, start=2016, end=2018)) +
  ylab("Sales volume") + xlab("Quarter") 
gridExtra::grid.arrange(xp,p2)


#forecasting based on sales data and seasonality only


salesmsts2 %>%  stlf() %>% autoplot() 
salesmsts_wy %>%  stlf() %>% autoplot()
salesmsts_wmy %>%  stlf() %>% autoplot()

#When x is a msts object, then K should be a vector of integers specifying the
# number of sine and cosine terms for each of the seasonal periods.
fityear <- auto.arima(salesmsts2, seasonal=FALSE,
                  xreg=fourier(salesmsts2, K=c(12)))
fityear %>% forecast(xreg=fourier(salesmsts2, K=c(12), h=2*336)) %>% autoplot(include=7*336)


#arima fcast with wmy didn't improve accuracy dramatically, but it is a good model and more likely to be replicable with other data
fitwmy <- auto.arima(salesmsts_wmy, seasonal=FALSE,
                      xreg=fourier(salesmsts_wmy, K=c(3,15,180)))
fitwmy %>% forecast(xreg=fourier(salesmsts_wmy, K=c(3,15,180), h=2*336)) %>% autoplot(include=7*336)


