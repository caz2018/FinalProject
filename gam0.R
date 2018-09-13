## weaves
##
## Following petolau
## https://github.com/PetoLau/petolau.github.io.git

rm(list=ls())
gc()

## Key library is mgcv

library(mgcv)
library(car)
library(ggplot2)
library(grid)
library(caret)

if(length(commandArgs(trailingOnly=TRUE)) <= 0) {
    x.args <- c("src/sales0-M.csv")
}

## This loads the data and performs adjustments.
source("gam00.R")

p1 <- ggplot(data0, aes(dt0, log(sales))) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
    labs(x = "Date", y = "count")

print(p1)

