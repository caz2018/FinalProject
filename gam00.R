## Support for GAM.

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

## parameter holder
ltp <- list()

## The weights to use within GAM.

## Convert year and month to a date - BoM
data0 <- read.csv(x.args[1], stringsAsFactors=FALSE)
x.dt0 <- data0[["dt0"]] #date
x.dt0 <- sapply(x.dt0, function(x) paste(x, "-01", sep=""))
data0[["dt1"]] <- as.Date(x.dt0, format="%Y-%m-%d")

### Flat weighting 
wts.flat <- rep(1, dim(data0)[1])

### Exponentially weighted - no need to reverse
wts.exp <- rev(ewma(c(1, rep(0, length(wts.flat) - 1)), lambda=1-0.95))
wts.expmean <- wts.exp / mean(wts.exp)

### Set a default
wts <- wts.flat

## Make the date an EoM
require("lubridate")
data0[["dt1"]] <- data0[["dt1"]] + months(1) - days(1)
### add the number of the month
data0[["mm0"]] <- month(data0[["dt1"]])


## Add a log of sales
data0[["sales0"]] <- log(data0[["sales"]])
d0 <- data0[["sales0"]]
d1 <- d0 - min(d0)

### Try and remove the growth
### this includes some FX
p1 <- scatter.smooth(x=1:length(d1), y=d1)    # see a scatter

x.plots[['scatter']] <- p1

x0 <- loess.smooth(x=1:length(d1), y=d1, evaluation=length(d1))

data0[["growth"]] <- x0$y
                                        
plot(density(d1))                       # strange distribution

## Make a model for linear growth
d2 <- data.frame(sales1=d1,idx=1:length(d1))

mdl0 <- lm(sales1 ~ idx, weights=wts.flat, data=d2)

d3 <- mdl0$fitted.values
d3[[length(d3)]] - d3[[1]]

## with weights
mdl0 <- lm(sales1 ~ idx, weights=wts.exp, data=d2)

d3 <- mdl0$fitted.values
d3[[length(d3)]] - d3[[1]]


data0[["sales1"]] <- d1 - mdl0$fitted.values

data0[["growth"]] <- mdl0$fitted.values

## Make FX more explicit by inverting
data0[["fx1"]] <- 1 / data0[["fx0"]]


## Remove some growth
s0 <- summary(data0[["sales0"]])

## Scale and centre

data1 <- sapply(data0[, c("fx1", "sales", "sales0", "sales1", "weather1", "growth")], 
                function(x) scale(x, scale=TRUE, center=TRUE))
data1 <- as.data.frame(data1)
colnames(data1) <- paste("s.", colnames(data1), sep="")

data0 <- cbind(data0, data1)

## Make up some feature sets

## The outcome variable
ltp[['outcomen']] <- x.args[2]

ltp[['date']] <- c('dt1')

## The only valid inputs
ltp[['inputn']] <- c('mm0', 'weather', 'fx1', 'dh0', 'growth')

## The derived outputs can be disregarded
ltp[['xclde']] <- setdiff(colnames(data0),
                          union(ltp[['outcomen']], 
                                union(ltp[['date']], ltp[['inputn']]) ) )

ltp.paste0 <- function(x, bs0="ps") {
    return(sprintf("s(%s, bs=\"%s\")", x, bs0))
}

ltp.gamS <- function(gam1, zz="all.Rout", force0=FALSE) {
    if (force0) {
        unlink(zz, force=force0)
        fl <- file(zz, "wt")
    }
    
    x.gam <- gam1
    zz <- file(zz, open = "at")
    sink(zz)
    cat("\n\n")
    cat(sprintf("start: %s", x.gam$name), "\n")
    ## what to look at: summary: EDF, p-values, R^2, GCV; AIC, magic
    cat("summary: ")
    print(summary(x.gam))

    cat("R^2: ", summary(x.gam)$r.sq, "; ")

    cat("GCV: ", summary(x.gam)$sp.criterion, "; ")

    cat("AIC: ", x.gam$aic, "; ")

    cat("BIC: ", BIC(x.gam))
    cat("\n")

    # sink(zz, type = "message")
    ## revert output back to the console -- only then access the file!
    ## sink(type = "message")
    sink()
    return(zz)
}


ltp.chart <- function(gam1, ctr0="unknown", nocoef=FALSE, tbl=data0,
                      c0=c("value", "dt1"), name1="sales") {

    ## Number of coefficients
    if (!nocoef) {
        l0 <- length(unique(gsub("\\.[0-9]+$", "", names(gam1$coefficients))))

        layout(matrix(1:l0, nrow = 1))
        plot(gam1)
    }

    x.df1 <- tbl
    if (is.null(tbl)) {
        x.df1 <- (gam1$model)[, 1, drop = FALSE]
        x.df1[, c0[[2]] ] <- rownames(x.df1)
        colnames(x.df1)[1] <- c0[1]
    } else {
        x.df1 <- tbl[, c0]
    }
    x.df1$type <- "Real"

    x.df2 <- data.frame(value = gam1$fitted.values, 
                        dt1 = x.df1[, c0[[2]] ] )
    x.df2$type <- "Fitted"

    datas <- rbind(x.df1, x.df2)

    p1 <- ggplot(data = datas, aes(dt1, value, group = type, colour = type)) +
        geom_line(size = 0.8) +
        theme_bw() +
        labs(x = "date", y = name1,
             title = sprintf("Fit from GAM - %s", gam1[["name0"]]))

    return(p1)
}
