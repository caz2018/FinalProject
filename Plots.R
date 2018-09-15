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
