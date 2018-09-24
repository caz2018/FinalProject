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
n_monthFact <- unique(DT[, "Freq.MonthFactor" ])
n_monthn <- unique(DT[, "Freq.MonthNum" ])
n_temp <- unique(DT[, "Freq.TAVG"])
n_rain <- unique(DT[, "Freq.PRCP"])
n_price <- unique(DT[, "Freq.Price"])
n_temp_variation <- unique(DT[, "Freq.Remainder.y"])
week_period <- 7
year_period <- 365.25
#Weekly seasonality close-up - needs ordering
data_r <- DT


ggplot(data_r, aes(x = n_weekdays, y=n_value )) + theme_bw() + geom_bar(stat = "identity")
   
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

ggplot(data_r, aes( data_r$Freq.PRCP , data_r$Freq.Total_Daily_Value)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8, face = "bold")) +
  labs(x = "Rainfall", y = "Avg Daily Sales GBP")


N <- nrow(data_r) # number of observations

window <- N / week_period # number of periods in the train set
require(data.table)
matrix_gam <- data.table(Load = data_r[ ,"Freq.Total_Daily_Value" ],
                         Monthly = data_r[, "Freq.MonthNum" ],
                        Weekly = data_r[,"Freq.WeekNum" ])  

gam_1 <- gam(Load ~ s(Weekly, bs = "ps", k = week_period) +
               s(Monthly, bs = "cr", k = 12),
             
             data = matrix_gam,
             family = gaussian)  

layout(matrix(1:2, nrow = 1))
plot(gam_1, shade = TRUE)

summary(gam_1)

#Plotting fitted values
df1 <- data.table(value = gam_1$fitted.values, data_time = data_r[ , "Freq.Date"] )
df2 <- data.table(value = data_r$Freq.Total_Daily_Value, data_time = data_r[ , "Freq.Date"] )
df1$type <- "Fitted"
df2$type <- "Real"

datas <- rbind(df1,df2)
  
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(data_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Sales",
       title = "Fit from GAM n.1")
#with season
gam_2 <- gam(Load ~ s(Weekly, Monthly),
             data = matrix_gam,
             family = gaussian)

summary(gam_2)$r.sq
summary(gam_1)$r.sq
summary(gam_2)$s.table
summary(gam_1)$s.table
#Plotting fitted values
dfs1 <- data.table(value = gam_2$fitted.values, data_time = data_r[ , "Freq.Date"] )
dfs2 <- data.table(value = data_r$Freq.Total_Daily_Value, data_time = data_r[ , "Freq.Date"] )
df1$type <- "Fitted"
df2$type <- "Real"

dataseason <- rbind(dfs1,dfs2) 

dataseason[, type := c(rep("Fitted", nrow(data_r)), rep("Real", nrow(data_r)))]

ggplot(data = dataseason, aes(data_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Sales",
       title = "Fit from GAM n.1")
