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
#ask - do those values seem correct? Ensure minimum is the oldest date and that the format matches the example 2014-01-01
#y/n
#if y then continue
#if not then request to re-upload the file
date <- ymd(SalesData$Date)
SalesData$Date <- date
SalesData$DoW <- weekdays.Date(date)
SalesData$WeekNum <- recode(SalesData$DoW,'Monday'=1,'Tuesday'=2, 'Wednesday'=3,'Thursday'=4,
                                        'Friday'=5,'Saturday'=6,'Sunday'=7)
## Make the date an EoM
require("lubridate")
### add the number of the month
SalesData$MonthNum <- month(SalesData$Date)
SalesData$MonthFactor <- recode(SalesData$MonthNum, '1'="Jan", '2'= "Feb", '3'= "Mar", '4'= "Apr", '5'= "May",
                                '6'= "Jun", '7'="Jul",'8' = "Aug", '9'= "Sep", '10' = "Oct", '11'= "Nov", '12' = "Dec")

#adding a normalized version of daily sales values to the dataframe, using a 1 to 10 range to avoid zero values
SalesData$ValNorm <- normalize(SalesData$Total_Daily_Value , method="range", range=c(1,10))

#need to transform sales data into ts (time series) objects and then into a msts (multi-seasonal time-series). The default is daily.
timeseriesItems <- ts( SalesData$Daily_Items_Sold, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesItems) + ylab("Daily Items") + xlab("Year")

timeseriesValue <- ts( SalesData$Total_Daily_Value, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesValue) + ylab("Daily Value GBP") + xlab("Year")

timeseriesATV <- ts( SalesData$Avg_Trans_Value_GBP, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesATV) + ylab("Average Transaction Value") + xlab("Year")

timeseriesValNorm <- ts( SalesData$ValNorm, start = c(2014,001) , frequency = 365.25)
autoplot(timeseriesValNorm) + ylab("Sales Volume Normalised") + xlab("Year")

#setting the seasonality variables
weekly <- 7
monthly <- 30.5
yearly <- 364.25

#function for decomposing data series - timeseries_obj is the chosen measure, seasonal periods are the chosen season(s)
#By default the Full Daily Sales Value will be used for ease of visualisation, alongside weekly and yearly patterns.

decomp<- function(timeseries_obj,s1=NULL,s2=NULL,s3=NULL) {
  a = msts(timeseries_obj, start = c(2014,001), seasonal.periods=c(s1,s2,s3))
  xy1 <- mstl(a, iterate = 3)
  return(xy1) 
}

decVals <- decomp(timeseriesValue, yearly,weekly)

autoplot(decVals) + ylab("Daily Value") + xlab("Year")

#generating a decomposition of a normalised set, for use with other variables.
salesNormmsts_mwy <- msts(timeseriesValNorm, seasonal.periods=c(7,30.5,365.25))
xnorm_wmy <- mstl(salesNormmsts_mwy, lambda = "auto", iterate = 3)
autoplot(xnorm_wmy)
summary(xnorm_wmy)
#generating a dataset with decomposed values to use with GAM
dat_decomp <- as.data.frame(xnorm_wmy)
dat_decomp$Date <- date


#forecasting based on sales data and seasonality only


#When x is a msts object, then K should be a vector of integers specifying the
# number of sine and cosine terms for each of the seasonal periods. 
#To find the best number of Fourier terms by computing the lowest AICc - however this code take several minutes to run,
#which is why it will not be used
'''
salesmsts = msts(timeseriesValue, start = c(2014,001), seasonal.periods=c(365.25))
bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(salesmsts, xreg=fourier(salesmsts, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}
fc <- forecast(bestfit,
               xreg=fourier(salesmsts, K=bestK, h=6*365.25))

autoplot(fc)

fit[["aicc"]]
'''


#arima fcast with week and year seasonalities, for future comparison with GAM models
#the value of K = 19 was found by running a script for a few hours, based on the lowest AIcc
#The code below retunrs forecasting using a dynamic regression for 90 days ahead
salesmsts = msts(timeseriesValue, start = c(2014,001), seasonal.periods=c(7,365.25))
fitwy <- auto.arima(salesmsts, seasonal=FALSE,
                      xreg=fourier(salesmsts, K=c(3,19)))
fitwy %>% forecast(xreg=fourier(salesmsts, K=c(3,19), h=1*90)) %>% autoplot(include=6*336)


