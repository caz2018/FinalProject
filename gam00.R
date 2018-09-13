## weaves
##
## Support for GAM.

## Convert year and month to a date - BoM
data0 <- read.csv(x.args[1], stringsAsFactors=FALSE)
x.dt0 <- data0[["dt0"]]
x.dt0 <- sapply(data0[["dt0"]], function(x) paste(x, "-01", sep=""))
data0[["dt1"]] <- as.Date(x.dt0, format="%Y-%m-%d")

## Make the date an EoM
require("lubridate")
data0[["dt1"]] <- data0[["dt1"]] + months(1) - days(1)
### add the number of the month
data0[["mm0"]] <- month(data0[["dt1"]])

## Daylight hours for each month
dh0 <- read.csv("src/dhm.csv")
data0 <- merge(data0, dh0, by="mm0")

## Order this
data0 <- data0[ order(data0$dt1), ]

## Exponentially weighted moving average

if (!exists("ewma")) {
    require("zoo")

    ewma <- function(x,lambda = 1, init = x[1]) {

        rval<-filter(lambda * coredata(x),
                     filter=(1-lambda),method="recursive",
                     init=init)
        rval<-zoo(coredata(rval),index(x))
        rval
    }
}

## The weights to use within GAM.

### Flat weighting 
wts.flat <- rep(1, dim(data0)[1])

### Exponentially weighted - no need to reverse
wts.exp <- ewma(c(1, rep(0, length(wts.flat) - 1)), lambda=1-0.90)
wts.expmean <- wts.exp / mean(wts.exp)

### Set a default
wts <- wts.flat

## Others
