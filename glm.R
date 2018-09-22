library(feather)
library(data.table)
library(mgcv)
library(dplyr)
library(ggplot2)
library(grid)
library(animation)

#reading dataframe as data.table
DT <- as.data.frame.table(newdf)

#Setting variables

n_value <- unique(DT[, "Freq.Total_Daily_Value"])
n_date <- unique(DT[, "Freq.Date"])
n_weekdays <- unique(DT[, "Freq.WeekNum"])
n_monthFact <- unique(DT[, "Freq.MonthFactor" ])
n_monthn <- unique(DT[, "Freq.MonthNum" ])
n_temp <- unique(DT[, "Freq.TAVG"])
n_rain <- unique(DT[, "Freq.PRCP"])
week_period <- 7
year_period <- 365.25
#Weekly seasonality close-up - needs ordering
data_r <- DT

ggplot(data_r, aes( data_r$Freq.DoWFact , data_r$Freq.Total_Daily_Value)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8, face = "bold")) +
  labs(x = "Day of the week", y = "Avg Daily Sales GBP")

#Monthly seasonality close-up - needs ordering

ggplot(data_r, aes( data_r$Freq.MonthFactor, data_r$Freq.Total_Daily_Value)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8, face = "bold")) +
  labs(x = "Day of the week", y = "Avg Daily Sales GBP")

#weekly seasonality training data
a = nrow(DT)*0.5
data_train <- DT[0:a,]
data_test <- DT[-data_train]

N <- nrow(data_train) # number of observations in the train set

window <- N / week_period # number of periods in the train set
require(data.table)
matrix_gam <- data.table(Load = data_train[ ,"Freq.Total_Daily_Value" ],
                         Daily = rep(1:week_period, window),
                         Monthly = data_r[, "Freq.MonthNum" ],
                        Weekly = data_r[,"Freq.WeekNum" ])  

gam_1 <- gam(Load ~ s(Weekly, bs = "ps", k = week_period) +
               s(Monthly, bs = "cr", k = 12),
             
             data = matrix_gam,
             family = gaussian)  

layout(matrix(1:2, nrow = 1))
plot(gam_1, shade = TRUE)


