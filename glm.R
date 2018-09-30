#!/usr/bin/env Rscript

library(feather)
library(data.table)
library(mgcv)
library(dplyr)
library(ggplot2)
library(grid)
library(animation)

#reading dataframe as data.table
DT <- as.data.frame.table(newdf)
summary(DT)

#Setting variables

n_value <- unique(DT[, "Freq.Total_Daily_Value"])
n_date <- unique(DT[, "Freq.Date"])
n_weekdays <- unique(DT[, "Freq.WeekNum"])
n_dow <- unique(DT[, "Freq.DoW"])
n_monthFact <- unique(DT[, "Freq.MonthFactor" ])
n_monthn <- unique(DT[, "Freq.MonthNum" ])
n_temp <- unique(DT[, "Freq.TAVG"])
n_rain <- unique(DT[, "Freq.PRCP"])
n_price <- unique(DT[, "Freq.Price"])
n_temp_variation <- unique(DT[, "Freq.Remainder.y"])
week_period <- 7
year_period <- 365.25
#back-up
data_r <- DT


#Currency close-up - needs ordering

ggplot(data_r, aes( data_r$Freq.Price , data_r$Freq.Total_Daily_Value)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8, face = "bold")) +
  labs(x = "Currency", y = "Avg Daily Sales GBP")

#Precipitation close-up - needs ordering

'ggplot(data_r, aes( data_r$Freq.PRCP , data_r$Freq.Total_Daily_Value)) +
  geom_bar() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8, face = "bold")) +
  labs(x = "Rainfall", y = "Avg Daily Sales GBP")
' 

N <- nrow(data_r) # number of observations

window <- N / 365.25 # number of periods in the train set
require(data.table)

#weekday and month of the year effect
matrix_gam <- data.table(Sales = data_r[ ,"Freq.Total_Daily_Value" ],
                         Monthly = data_r[,"Freq.MonthNum"],
                        Weekly = data_r[,"Freq.WeekNum" ],
                        Temp = data_r[,"Freq.Remainder.y"],
                        price = data_r[,"Freq.Price" ],
                        trend = data_r[,"Freq.Trend.x"]
                       
                        )  

#individual gams on Day Of The Week and Month of The Year only, for visualisation of its effects
gam_1 <- gam(Sales ~ s(Weekly, bs = "ps", k = week_period) +
               s(Monthly, bs = "cr", k = 12),
             
             data = matrix_gam,
             family = gaussian)  

layout(matrix(1:2, nrow = 1))
plot(gam_1, shade = TRUE)

#function for displaying results

x <- function(a){
  return(summary(a)$r.sq)
}
 


#gam with week and month only

gam_0 <- gam(Sales ~ s(Weekly, Monthly),
             data = matrix_gam,
             family = gaussian)



#gam with monthly and trend

gam_2 <- gam(Sales ~ s(Weekly, Monthly, trend ),
             data = matrix_gam,
             family = gaussian)

display.results(gam_2)

#gam with all main variables
gam_3 <- gam(Sales ~ s(Weekly, Monthly, Temp, trend),
             data = matrix_gam,
             family = gaussian)
display.results(gam_3)

gam_4 <- gam(Sales ~ s(Weekly, Monthly, price, trend),
             data = matrix_gam,
             family = gaussian)
display.results(gam_4)

gam_5 <- gam(Sales ~ s(Weekly, Monthly, Temp, price, trend),
             data = matrix_gam,
             family = gaussian)
display.results(gam_5)

gam_6 <- gam(Sales ~ s(Weekly, Monthly, price),
             data = matrix_gam,
             family = gaussian)
display.results(gam_6)


gam_7 <- gam(Sales ~ s(Weekly, Monthly, Temp),
             data = matrix_gam,
             family = gaussian)
display.results(gam_7)

#Plotting fitted values
df2 <- data.table(value = gam_3$fitted.values, data_time = data_r[ , "Freq.Date"] )
df1 <- data.table(value = data_r$Freq.Total_Daily_Value, data_time = data_r[ , "Freq.Date"] )
df2$type <- "Real"
df1$type <- "pred"

datas <- rbind(df1,df2)
  
datas[, type := c(rep("Real", nrow(data_r)), rep("pred", nrow(data_r)))]

ggplot(data = datas, aes(data_time, value, group = type, colour = type)) +
  geom_line(size = 0.7) +
  theme_bw() +
  labs(x = "Time", y = "Sales",
       title = "Fit from GAM n.1")



summary(gam_2)$r.sq
summary(gam_1)$r.sq
summary(gam_2)$s.table
summary(gam_1)$s.table


#Plotting fitted values
dfs1 <- data.table(value = gam_2$fitted.values, data_time = data_r[ , "Freq.Date"] )
dfs2 <- data.table(value = data_r$Freq.Total_Daily_Value, data_time = data_r[ , "Freq.Date"] )
dfs1$type <- "Fitted"
dfs2$type <- "Real"

dataseason <- rbind(dfs1,dfs2) 

dataseason[, type := c(rep("Fitted", nrow(data_r)), rep("Real", nrow(data_r)))]

ggplot(data = dataseason, aes(data_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Sales",
       title = "Fit from GAM n.1")

#Plotting fitted values and fx
 
dffx1 <- data.table(value = data_r$Freq.Price*100000, data_time = data_r[ , "Freq.Date"] )
dffx2 <- data.table(value = data_r$Freq.Total_Daily_Value, data_time = data_r[ , "Freq.Date"] )
dffx1$type <- "GBP price in USD"
dffx2$type <- "Sales"

dataseason <- rbind(dffx1,dffx2) 

dataseason[, type := c(rep("GBP price in USD", nrow(data_r)), rep("Sales", nrow(data_r)))]

ggplot(data = dataseason, aes(data_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Sales",
       title = "Fit from GAM n.1")
#for the future, could add:
#support for multiple currencies
#support for visualization of (value of sale in currency selected)
#prediction of values based on decomposed data + currency value by user input

