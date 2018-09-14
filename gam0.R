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
library(reshape2)

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

x.plots <- list()

## Scaled data
c0 <- colnames(data0)
idx <- grepl("s\\.sales$|dt1|s\\..+1", c0)

data1 <- data0[, c0[idx]]

data1 <- melt(data1, id.vars="dt1")

p1 <- ggplot(data = data1, aes(dt1, value, group = variable, colour = variable)) +
    geom_line(size = 0.8) +
    theme_bw() +
    labs(x = "date", y = data1$variable,
         title = sprintf("Scaled and centred except %s", 's.sales'))

x.plots[['scaled']] <- p1

## Write plots

jpeg(filename=paste("gam0", "-%03d.jpeg", sep=""), 
     width=1024, height=768)

## print(p1)

lapply(x.plots, print)

x0 <- lapply(dev.list(), function(x) dev.off(x))


