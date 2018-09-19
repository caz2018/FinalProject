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
library(gtools)

x.plots <- list()

x.args <- c("src/sales0-M.csv", "sales", "cp")

if(length(commandArgs(trailingOnly=TRUE)) > 0) {
    x.args <- commandArgs(trailingOnly=TRUE)
}

## This loads the data and performs adjustments.
source("gam00.R")

p1 <- ggplot(data0, aes(dt1, log(sales))) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
    labs(x = "Date", y = "count")

x.plots[['log-sales']] <- p1

## Scaled data
c0 <- colnames(data0)
idx <- grepl("s\\.growth|s\\.sales$|dt1|s\\..+1", c0)

data1 <- data0[, c0[idx]]

data1 <- melt(data1, id.vars="dt1")

p1 <- ggplot(data = data1, aes(dt1, value, group = variable, colour = variable)) +
    geom_line(size = 0.8) +
    theme_bw() +
    labs(x = "date", y = "deviations",
         title = sprintf("Scaled and centred except %s", 's.sales'))

x.plots[['scaled']] <- p1

## Pre-process

data1 <- data0[, !colnames(data0) %in% ltp[['xclde']] ]

## theory: GLM: Iteratively Re-weighted Least Squares (IRLS) theory:

## GAM: Penalized Iteratively Re-weighted Least Squares (P-IRLS)
##
## k -> ## (knots) is upper boundery for EDF
##
## how smooth fitted value will be (more knots more overfit (under
## smoothed), less more smooth)

## bs -> basis function..type of smoothing function
##
## dimension can be fixed by fx = TRUE EDF (trace of influence matrix)
## and lambda (smoothing factor) is estimated (tuned) by GCV, UBRE or REML.

## We will use the default GCV (Generalized Cross Validation) (more in
## Woods) basis function.

## I will use "cr" - cubic regression spline or "ps", which
## is P-spline (more in Woods).

## More options: "cc" cyclic cubic regression spline (good too with
## our problem), default is "tp" thin plane spline family.

## How response is fitted -> gaussian, log_norm, gamma, log_gamma is
## our possibilities, but gaussian is most variable in practice
## because gamma distibution must have only positive values gamm -
## possibility to add autocorrelation for errors

## Families and weights

family0 <- gaussian
family0 <- ziP                          # locks up
family0 <- Tweedie                      # needs setting

family0 <- poisson(link="log")          # rank 3
# family0 <- log_norm                     # identical to gaussian with log?
family0 <- gaussian(link="log")         # best

## Active inputs only

data1 <- data0[, !colnames(data0) %in% ltp[['xclde']] ]

## For non-logged outcome use a logged link method
if (ltp[['outcomen']] == "sales") {
    family0 <- gaussian(link="log")         # best
}

## Use a logged and scaled figure with a Poisson
if (ltp[['outcomen']] == "s.sales1") {
    s0 <- data1[["s.sales1"]]
    m0 <- min(s0)
    if (m0 < 0) {
        data1[["s.sales1"]] <- abs(m0) + s0
    }
    family0 <- poisson()
}

## Rename the outcome to something generic.

c0 <- colnames(data1)
c0[c0 == ltp[["outcomen"]] ] <- "value"
colnames(data1) <- c0

## Store GAMs in here

gs <- list()

## Weirdly, the month doesn't help. It seems to deduce it from the weather
## I only use it for the tensor as the basis

## GAM 1

## It doesn't like the deltas or af. af is dropped as a NZV.

## Check
## ftres <- ltp[['inputn']]
## ftres <- head(ftres, 3)

x.bag0 <- ltp[['inputn']]

n0 <- length(x.bag0)

ftres1 <- combinations(n=n0, r=n0-1, v=x.bag0)
ftres2 <- lapply(1:nrow(ftres1), function(x) ftres1[x,])

force0 <- TRUE
for (ftres in ftres2) {

    print(ftres)
    descrs <- paste(sapply(ftres, ltp.paste0), collapse=" + ")

    fmla <- as.formula(paste("value ~ ", descrs))
    tag <- paste("ps:", ftres, collapse=", ")

    gam0 <- gam(fmla,
                weights = wts,
                data = data1,
                family = family0)

    gam0[["name0"]] <- tag
    gam0[["ftres"]] <- unique(ftres)
    gs[[tag]] <- gam0

    ltp.gamS(gam0, force0=force0)       # just to start with
    force0 <- FALSE

    summary(gam0)$r.sq
}

## Cubic splines

x.bag0 <- setdiff(ltp[['inputn']], c("dh0")) # can't use dh0

n0 <- length(x.bag0)

ftres1 <- combinations(n=n0, r=n0, v=x.bag0)
ftres2 <- lapply(1:nrow(ftres1), function(x) ftres1[x,])

for (ftres in ftres2) {

    print(ftres)
    descrs <- paste(sapply(ftres, function(x) ltp.paste0(x, bs0=x.args[3])), collapse=" + ")

    fmla <- as.formula(paste("value ~ ", descrs))
    tag <- paste("ps:", ftres, collapse=", ")

    gam0 <- gam(fmla,
                weights = wts,
                data = data1,
                family = family0)

    gam0[["name0"]] <- tag
    gam0[["ftres"]] <- unique(ftres)
    gs[[tag]] <- gam0

    ltp.gamS(gam0, force0=force0)       # just to start with
    force0 <- FALSE

    summary(gam0)$r.sq
}

## Ad-hoc: using a tensor 

ftres <- c("mm0", "growth", "fx1", "weather")

fmla <- "value ~ te(growth, fx1) + s(mm0, bs = \"ps\") "
tag <- paste("te:", fmla)

gam0 <- gam(as.formula(fmla),
            weights = wts,
            data = data1,
            family = family0)

gam0[["name0"]] <- tag
gam0[["ftres"]] <- unique(ftres)
gs[[tag]] <- gam0

ltp.gamS(gam0, force0=force0)       # just to start with
force0 <- FALSE

summary(gam0)$r.sq

x.plots[['final']] <- ltp.chart(gam0, tbl=data1)

## Write plots

jpeg(filename=paste("gam0", "-%03d.jpeg", sep=""), 
     width=1024, height=768)

plot(gam0)

scatter.smooth(x=1:length(d1), y=d1)    # see a scatter

x0 <- lapply(x.plots, print)

x0 <- lapply(dev.list(), function(x) dev.off(x))

plot(gs[[1]])
