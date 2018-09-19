library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)

attach(newdf)
autoplot(newdf[,c("LogSales", "TAVG")],
         Facet=TRUE) + scale_x_continuous(minor_breaks=NULL,
                                          breaks=2014+
                                            cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))/365,
                                          labels=month.abb) + xlab("Time") + ylab("")
plot(newdf)


ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")
