#clean up env
rm(list = ls())

library(readr)
library(stats)
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(forecast)
library(BBmisc)
library(zoo)



#read data from csv and loading sales data with dates and formatting the date field as date in R
#Note: file must be a csv and the date order must be oldest to newest in the file.

SalesData <-  as.data.frame(read_csv("SalesReportDaily.csv"))
print(summary(SalesData))
dates <- as.character(SalesData$Date)
dates <- as.Date(SalesData$Date, "%Y/%m/%d")
SalesData$Date <- dates
SalesData$DoW <- weekdays.Date(dates)
SalesData$WeekNum <- recode(SalesData$DoW,'Monday'=1,'Tuesday'=2, 'Wednesday'=3,'Thursday'=4,
                                        'Friday'=5,'Saturday'=6,'Sunday'=7)
## Make the date an EoM
require("lubridate")
SalesData$Month <-NULL
### add the number of the month
SalesData$MonthNum <- month(SalesData$Date)
SalesData$MonthFactor <- recode(SalesData$MonthNum, '1'="Jan", '2'= "Feb", '3'= "Mar", '4'= "Apr", '5'= "May",
                                '6'= "Jun", '7'="Jul",'8' = "Aug", '9'= "Sep", '10' = "Oct", '11'= "Nov", '12' = "Dec")

#adding a normalized version of tickets sold to the dataframe, using a 1 to 10 range to avoid zero values
SalesData$ValNorm <- normalize(SalesData$Total_Daily_Value , method="range", range=c(1,100))
print(summary(SalesData$ValNorm))


#need to transform sales data into a ts (time series) object and then into a msts (multi-seasonal time-series)

timeseriesItems <- ts( SalesData$Daily_Items_Sold, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesItems) + ylab("Sales Volume") + xlab("Year")

timeseriesValue <- ts( SalesData$Total_Daily_Value, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesValue) + ylab("Sales Value") + xlab("Year")

timeseriesATV <- ts( SalesData$Avg_Trans_Value_GBP, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesATV) + ylab("Average Transaction Value") + xlab("Year")

timeseriesValNorm <- ts( SalesData$ValNorm, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesValNorm) + ylab("Sales Volume Normalised") + xlab("Year")


#function to see decomposed data for yearly seasonality only 

weekly <- 7
monthly <- 30.5
yearly <- 364.25
#custom <- user_input - if time allows for the development of custom values

salesmsts_d <- function(timeseries_obj,s1=NULL,s2=NULL,s3=NULL) {
  salesmsts <- msts(timeseries_obj, start = c(2014,001), seasonal.periods=c(s1,s2,s3))
  xy1 <- mstl(salesmsts, iterate = 3)
  return(xy1) 
}
  
dV <- salesmsts_d(timeseriesValue, yearly,weekly)

autoplot(dV) + ylab("Daily Sales Value") + xlab("Year")


p1 <- autoplot(window(dV, start=1, end=2)) +
  ylab("Sales Value") + xlab("First Year Close-up") + 
  scale_x_continuous(breaks = c(seq(1, 1.90, length.out=12)), labels=month.abb)
p2 <- autoplot(window(dV, start=2, end=3)) +
  ylab("Sales") + xlab("Year 2 Close-up") + 
  scale_x_continuous(breaks = c(seq(2, 2.90, length.out=12)), labels=month.abb)
p3 <- autoplot(window(dV, start=2.50, end=3)) +
  ylab("Sales") + xlab("Year 2, 2nd half, Close-up") + 
  scale_x_continuous(breaks = c(seq(2, 2.90, length.out=12)), labels=month.abb)
p4 <- autoplot(window(dV, start=2.75, end=3)) +
  ylab("Sales volume") + xlab("Year 2, 3rd Q, Close-up") + 
  scale_x_continuous(breaks = c(seq(2, 2.90, length.out=12)), labels=month.abb)

gridExtra::grid.arrange(p1, p2, p3, p4)


#using normalised set, no plot generated
salesNormmsts_mwy <- msts(timeseriesValNorm, seasonal.periods=c(7,30.5,365.25))
xnorm_wmy <- mstl(salesNormmsts_mwy, lambda = "auto", iterate = 3)
autoplot(xnorm_wmy)
xdat <- as.data.frame(xnorm_wmy)
xdat$Date <- dates


#forecasting based on sales data and seasonality only

salesmsts_wmy %>%  stlf() %>% autoplot()

#When x is a msts object, then K should be a vector of integers specifying the
# number of sine and cosine terms for each of the seasonal periods. 
#To find the best number of Fourier terms by computing the lowest AICc - the below code has taken over 
#12 hours to run without going past 57, therefore an inpractical addition. The max will be used instead.

fit <- auto.arima(salesmsts_y, xreg=fourier(salesmsts_y, K=57),
                   seasonal=FALSE)
fit[["aicc"]]



#arima fcast with week, month and year seasonalities takes over 5 min to run
#so only wekly and yearly will be used
#it also didn't improve accuracy dramatically.
fitwy <- auto.arima(salesmsts_wy, seasonal=FALSE,
                      xreg=fourier(salesmsts_wy, K=c(3,52), seasonal=FALSE))
fitwy %>% forecast(xreg=fourier(salesmsts_wmy, K=c(3,52), h=1*336)) %>% autoplot(include=6*336)


