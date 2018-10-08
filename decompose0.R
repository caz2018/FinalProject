## 
##
## Observed change in sales after 2013-01
## 

rm(list=ls())
gc()



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

## Get the noise from the decomposition and test if it is normal

sales0 <- decompose(data2)

x0 <- as.vector(sales0$random)
x0 <- x0[!is.na(x0)]

## Checking
summary(x0)
sd(x0)

hist(x0)

## Useful function

p2o <- function(p) (1-p)/p              # Convert probabililty to odds

## We can remove a rogue value if we want.
n <- 0
x1 <- sort(x0)[1:length(x0)-n]

## Use a Kolmogorov test using pnarm (normal percentiles)
ks0 <- ks.test(x1, "pnorm", mean(x1), sd(x1), alternative=c("two.sided"))

p2o(ks0[["p.value"]])



jpeg(filename=paste("decompose", "-%03d.jpeg", sep=""), 
     width=1024, height=768)

print(p0)

plot(sales0)
title("Sales")

qqnorm(x0); qqline(x0, col=2)

x0 <- lapply(dev.list(), function(x) dev.off(x))

