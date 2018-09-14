## weaves
##
## Support for GAM.

## Convert year and month to a date - BoM
data0 <- read.csv(x.args[1], stringsAsFactors=FALSE)
x.dt0 <- data0[["dt0"]]
x.dt0 <- sapply(x.dt0, function(x) paste(x, "-01", sep=""))
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

## Add a log of sales
data0[["sales0"]] <- log(data0[["sales"]])
d0 <- data0[["sales0"]]
d1 <- d0 - min(d0)

### Try and remove the growth
scatter.smooth(x=1:length(d1), y=d1)    # see a scatter
plot(density(d1))                       # strange distribution

## Make a model
d2 <- data.frame(sales1=d1,idx=1:length(d1))
mdl0 <- lm(sales1 ~ idx, data=d2)

data0[["sales1"]] <- d1 - mdl0$fitted.values

## Make FX more explicit by inverting
data0[["fx1"]] <- 1 / data0[["fx0"]]

## Make weather more explicit by inverting
m0 <- max(data0[["weather"]])
data0[["weather1"]] <- abs(data0[["weather"]] - m0)

## Remove some growth
s0 <- summary(data0[["sales0"]])

## Scale and centre

data1 <- sapply(data0[, c("fx1", "sales", "sales0", "sales1", "weather1")], 
                function(x) scale(x, scale=TRUE, center=TRUE))
data1 <- as.data.frame(data1)
colnames(data1) <- paste("s.", colnames(data1), sep="")

data0 <- cbind(data0, data1)

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
