#clean up env
rm(list = ls())

library(readr)
library(DescTools)
library(stats)
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(BBmisc)
library(forecast)

#read data from csv and loading sales data with dates and formatting the date field as date in R
#Note: file must be a csv and the date order must be oldest to newest in the file.

SalesData <-  as.data.frame(read_csv("SalesReportDaily.csv"))
print(summary(SalesData))
dates <- as.character(SalesData$Date)
dates <- as.Date(SalesData$Date, "%Y/%m/%d")
SalesData$Date <- dates

#adding a normalized version of tickets sold to the dataframe, using a 1 to 10 range to avoid zero values
SalesData$Norm <- normalize(SalesData$Daily_Items_Sold, method="range", range=c(1,10))
print(summary(SalesData$Norm))


#need to transform sales data into a ts (time series) object and then into a msts (multi-seasonal time-series)

timeseriesItems <- ts( SalesData$Daily_Items_Sold, start = c(2014,001) , frequency = 365.25)
autoplot(timeserieItems) + ylab("Sales volume") + xlab("Year")

timeseriesValue <- ts( SalesData$Total_Daily_Value, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesValue) + ylab("Sales Value") + xlab("Year")

timeseriesNorm <- ts( SalesData$Norm, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesNorm) + ylab("Sales Volume Normalised") + xlab("Year")

#decomposing the data for different seasonal periods


#yearly seasonality period of sales volumes with plots of full period and year one
salesmsts_y <- msts(timeseriesItems, seasonal.periods=c(364.25))
xy <- mstl(salesmsts_y, iterate = 3)
autoplot(xy)
xp <- autoplot(xy) +
  ylab("Sales volume") + xlab("Year")
p2 <- autoplot(window(xy, start=1, end=2)) +
  ylab("Sales volume") + xlab("Year 1 Close-up") + 
  scale_x_continuous(breaks = c(seq(1, 1.90, length.out=12)), labels=month.abb)
gridExtra::grid.arrange(xp,p2)


#Combine weekly, monthly, and yearly seasonality period of sales volumes
salesmsts_wmy <- msts(timeseriesItems, seasonal.periods=c(7,30.5,365.25))
salesmsts_wy <- msts(timeseriesItems, seasonal.periods=c(7,365.25))
xwmy <- mstl(salesmsts_wmy, iterate = 3)
xwy <- mstl(salesmsts_wy, iterate = 3)
autoplot(xwmy)
xp <- autoplot(xwmy) +
  ylab("Sales volume") + xlab("Year")
p2 <- autoplot(window(xwmy, start=2, end=3)) +
  ylab("Sales volume") + xlab("Year 2 Close-up") + 
  scale_x_continuous(breaks = c(seq(2, 2.90, length.out=12)), labels=month.abb)
p3 <- autoplot(window(xwmy, start=2.50, end=3)) +
  ylab("Sales volume") + xlab("Year 2, 2nd half, Close-up") + 
  scale_x_continuous(breaks = c(seq(2, 2.90, length.out=12)), labels=month.abb)
p4 <- autoplot(window(xwy, start=2.75, end=3)) +
  ylab("Sales volume") + xlab("Year 2, 3rd Q, Close-up") + 
  scale_x_continuous(breaks = c(seq(2, 2.90, length.out=12)), labels=month.abb)

gridExtra::grid.arrange(p2, p3, p4)


#using normalised set, no plot generated
salesNormmsts_mwy <- msts(timeseriesNorm, seasonal.periods=c(7,30.5,365.25))
xnorm_wmy <- mstl(salesNormmsts_mwy, lambda = "auto", iterate = 3)
autoplot(xnorm_wmy)
xdat <- as.data.frame(xnorm_wmy)


#forecasting based on sales data and seasonality only

salesmsts_wmy %>%  stlf() %>% autoplot()

#When x is a msts object, then K should be a vector of integers specifying the
# number of sine and cosine terms for each of the seasonal periods. 
#To find the best number of Fourier terms by computing the lowest AICc
bestfit <- list(aicc=Inf)
for(K in seq(182)) {
  fit <- auto.arima(salesmsts_y, xreg=fourier(salesmsts_y, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}


fityear <- auto.arima(salesmsts_m, seasonal=FALSE,
                  xreg=fourier(salesmsts_m, K=c(14)))
fityear %>% forecast(xreg=fourier(salesmsts_m, K=c(12), h=1*336)) %>% autoplot(include=6*336)


#arima fcast with wmy didn't improve accuracy dramatically, but it is a good model and more likely to be replicable with other data
fitwmy <- auto.arima(salesmsts_wmy, seasonal=FALSE,
                      xreg=fourier(salesmsts_wmy, K=c(3,15,180)))
fitwmy %>% forecast(xreg=fourier(salesmsts_wmy, K=c(3,15,180), h=2*336)) %>% autoplot(include=7*336)


