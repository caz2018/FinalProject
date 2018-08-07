## weaves
##
## Observed change in claims after 2016-07
## 

rm(list=ls())
gc()

## For footways

library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)

library(readr)
df1 <- read_csv("src/sales0-M.csv", col_types = cols(dt0 = col_date(format = "%Y-%m")))
View(df1)

## reshape this

df1m <- melt(df1, id.vars = c("dt0"))

p0 <- ggplot(data = df1m, aes(dt0, log(value), group=variable, colour=variable) ) + 
  geom_line(size = 0.8) + theme_bw()

df2 <- df1m[ df1m$variable == "sales", ]

## Force a log on the value and the decomposition is better.
data1 <- df2 %>% select(dt0, value) %>% 
  mutate(yr0=as.integer(year(dt0)), month0=as.integer(month(dt0)), value=log(value)) %>%
  select(dt0, yr0, month0, value)

data2 <- ts(data1$value, 
            start=c(data1$yr0[1], data1$month0[1]), 
            end=c(data1$yr0[nrow(data1)], data1$month0[nrow(data1)]), 
            frequency=12)

sales0 <- decompose(data2)

jpeg(filename=paste("decompose", "-%03d.jpeg", sep=""), 
     width=1024, height=768)

print(p0)

plot(sales0)
title("Sales")

x0 <- lapply(dev.list(), function(x) dev.off(x))

