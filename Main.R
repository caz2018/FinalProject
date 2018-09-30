#!/usr/bin/env Rscript

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

require(lubridate)
date <- ymd(SalesData$Date)
SalesData$Date <- date
SalesData$DoW <- as.factor(weekdays.Date(date))
SalesData$WeekNum <- recode(SalesData$DoW,'Monday'=1,'Tuesday'=2, 'Wednesday'=3,'Thursday'=4,
                                        'Friday'=5,'Saturday'=6,'Sunday'=7)
summary(SalesData)
## Make the date an EoM
require("lubridate")
### add the number of the month
SalesData$MonthNum <- month(SalesData$Date)
SalesData$MonthFactor <- as.factor(recode(SalesData$MonthNum, '1'="Jan", '2'= "Feb", '3'= "Mar", '4'= "Apr", '5'= "May",
                                '6'= "Jun", '7'="Jul",'8' = "Aug", '9'= "Sep", '10' = "Oct", '11'= "Nov", '12' = "Dec"))
summary(SalesData)

#setting the seasonality variables
weekly <- 7
monthly <- 30.5
yearly <- 364.25

#adding a normalized version of daily sales values to the dataframe, using a 1 to 10 range to avoid zero values
SalesData$ValNorm <- normalize(SalesData$Total_Daily_Value , method="range", range=c(1,10))

#need to transform sales data into ts (time series) objects and then into a msts (multi-seasonal time-series). The default is daily.
#function to transform a vector into a time series object. Need the day of the year as a number from 1 to 365/366.
#freq is the data interval, for daily data use yearly.
ts.transform <- function(univariateseries, YYYY,DDD, freq){
  x = ts(univariateseries, start = c(YYYY,DDD) , frequency = freq)
  return(x)
}

tsVal <- ts.transform(SalesData$Total_Daily_Value, 2014,001, yearly)
autoplot(tsVal) + ylab("Daily Value")+ xlab("Year")
autoplot.zoo(tsVal)

tsItems <- ts.transform(SalesData$Daily_Items_Sold, 2014,001, yearly)
autoplot.zoo(tsItems) + ylab("Daily Items") + xlab("Year")

tsATV <- ts.transform(SalesData$Avg_Trans_Value_GBP, 2014,001, yearly)
autoplot.zoo(tsATV) 

tsValNorm <- ts.transform(SalesData$ValNorm,2014,001, yearly)
autoplot(tsValNorm) + ylab("Sales Volume Normalised") + xlab("Year")

#function for transforming a time series object into a msts, which is a multiple-series time series
require(forecast)
msts.transform <- function(tsobj, YYYY,DDD,s1=NULL,s2=NULL,s3=NULL){
  a = msts(tsobj, start = c(2014,001), seasonal.periods=c(s1,s2,s3))
  return(a)
}

#function to decompose both single-seasonal and multi-seasonal time-series
decomp.msts <- function(mstsobj){
  xy1 = mstl(mstsobj, iterate = 3)
  return(xy1)
}

#By default the Full Daily Sales Value will be used for ease of visualisation, with yearly and weekly pattern.
mstsVal <- msts.transform(tsVal, 2014,001, weekly, yearly)
decVals <- decomp.msts(mstsVal)
autoplot.zoo(decVals) 


#generating a decomposition of the normalised Daily sales values for use in the analysis in conjunction with other variables.
salesNormmsts_mwy <- msts(tsValNorm, seasonal.periods=c(7,30.5,365.25))
xnorm_wmy <- mstl(salesNormmsts_mwy, lambda = "auto", iterate = 3)
autoplot(xnorm_wmy)
summary(xnorm_wmy)
#generating a dataset with decomposed values to use with GAM
dat_decomp <- as.data.frame(xnorm_wmy)
dat_decomp$Date <- date

#mergin the decomposition to the original data frame
SalesData<- merge(SalesData, dat_decomp, by.x = "Date", by.y = "Date", all.x = TRUE)

#forecasting based on sales data and seasonality only

#When x is a msts object, K should be a vector of integers specifying the
# number of sine and cosine (Fourier) terms for each of the seasonal periods. 
#To find the best number of Fourier terms, it is advisable to do it by computing the lowest AICc
#- however this code take several minutes to run, in fact, over 30 min, the code below is for the record only and it won't be run

#salesmsts = msts(timeseriesValue, start = c(2014,001), seasonal.periods=c(365.25))
#bestfit <- list(aicc=Inf)
#for(K in seq(25)) {
 # fit <- auto.arima(salesmsts, xreg=fourier(salesmsts, K=K),
  #                  seasonal=FALSE)
  #if(fit[["aicc"]] < bestfit[["aicc"]]) {
   # bestfit <- fit
    #bestK <- K
  #}
#}
#fc <- forecast(bestfit,
 #              xreg=fourier(salesmsts, K=bestK, h=6*365.25))

#autoplot(fc)

#fit[["aicc"]]

#arima fcast with week and year seasonalities, for future comparison with GAM models
#the value of K = 19 was found by running a script for a few hours, based on the lowest AIcc
#The code below retunrs forecasting using a dynamic regression for 90 days ahead
#the code below takes several minutes to run

#for predicting sales using the arima decomposition only for comparison to the gam
'fitwy <- auto.arima(mstsVal, seasonal=FALSE,
                      xreg=fourier(mstsVal, K=c(3,19)))
fitwy %>% forecast(xreg=fourier(salesmsts, K=c(3,19), h=1*90)) %>% autoplot(include=6*336)

' 
