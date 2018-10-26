#!/usr/bin/env Rscript

#clean up env
rm(list = ls())

library(readr)
library(stats)
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(forecast)
library(BBmisc)
library(zoo)
library(feather)
library(data.table)
library(mgcv)
library(grid)


data <- as.data.frame(Mailchimp_data_final_for_real_)
summary(data)
#remove NA's
#data$Theme[is.na(data$Theme)] <- '(Other)'
data$Theme <- as.factor(data$Theme)
data$Segment <- as.factor(data$Segment)
data$`Email Feature Type` <- as.factor(data$`Email Feature Type`)
data$List <- as.factor(data$List)
data$`Send Weekday` <- as.factor(data$`Send Weekday`)
require(lubridate)
senddate <- mdy(data$`Send Date`)
data$`Send Date` <- senddate
hs <- substr(data$`Send Time (Hour)`, 0, 2)
data$hs <- as.integer(hs)

require(lubridate)
data$WeekNum <- recode(data$`Send Weekday`,'Monday'=1,'Tuesday'=2, 'Wednesday'=3,'Thursday'=4,
                            'Friday'=5,'Saturday'=6,'Sunday'=7)
summary(data$WeekNum)

data$MonthNum <- month(data$`Send Date`)

data$MonthFactor <- as.factor(recode(data$MonthNum, '1'="Jan", '2'= "Feb", '3'= "Mar", '4'= "Apr", '5'= "May",
                                          '6'= "Jun", '7'="Jul",'8' = "Aug", '9'= "Sep", '10' = "Oct", '11'= "Nov", '12' = "Dec"))

data$ThemeFactor <- as.factor(recode(data$Theme, "Offer"=1, "New On-Sale"=2, "Full-price"=3, "Cast Announcement"=4, "Reviews"=5,
                                     "Opening Soon"=6, "Closing Soon"=7, "Extension"=8))
summary(data$ThemeFactor)
#remove NA's
#data$ThemeFactor[is.na(data$ThemeFactor)] <- NULL
#summary(data$ThemeFactor)

data$type <- as.factor(recode(data$`Email Feature Type`, "Solus"=1, "Multi-show"=2))

summary(data$Segment)

data$SegmentNum <- as.factor(recode(data$Segment, "All"=1, "all"=1, "Celeb"=2,"Circus/Cabaret/Burlesque"=3, "Comedy"=4, "Dance/Opera/Classical"=5,"Family"=6, "Musicals"=7,
                                    "Plays"=8))

#for (item in data$SegmentNum){
 # if (is.na(item)){
  #  print(data[i])
  #}
#}   

o <- ( data$`Total Opens`/data$`Total Recipients`)
data$`Open Rate` <- o

data$Segment <- as.factor(data$Segment)
data$SegmentNum <- as.factor(data$SegmentNum)
data$type <- as.factor(data$type)
data$`Email Feature Type` <- as.factor(data$`Email Feature Type`)

data$Subject <- NULL
data$`Soft Bounces` <- NULL
data$`Successful Deliveries` <- NULL
data$`Hard Bounces` <- NULL
data$`Total Bounces` <- NULL
data$`Times Forwarded` <- NULL
data$`Send Time` <- NULL
data$`Send Time (Hour)` <- NULL


#begin analysis - where the MAGIC begins!

#reading dataframe as data.table

DT <- as.data.frame.table(data)
summary(DT)
#Setting variables

n_theme <- unique(DT[,"Freq.Theme"]) 
n_weekdaynum <- unique(DT[, "Freq.WeekNum"])
n_monthFact <- unique(DT[, "Freq.MonthFactor"])
n_monthn <- unique(DT[, "Freq.MonthNum" ])
n_hour <- unique(DT[, "Freq.sendtime"])
n_segment <- unique(DT[,"Freq.Segment"])
n_nofsubs <- unique(DT[,"Freq.Total.Recipients"])
n_ftype <- unique(DT[,"Freq.Email.Feature.Type"])
n_opens <- unique(DT[,"Freq.Unique.Opens"])
n_openrate <- unique(DT[,"Freq.Open.Rate"])

#back-up
data_r <- DT

require(data.table)

#weekday and month of the year effect
matrix_gam <- data.table(OpenRate = data_r[,"Freq.Open.Rate" ],
                         Monthly = data_r[,"Freq.MonthNum"],
                         Weekly = data_r[,"Freq.WeekNum" ],
                         Hour = data_r[,"Freq.hs"],
                         NumSubs = data_r[,"Freq.Total.Recipients" ],
                         Theme = data_r[,"Freq.ThemeFactor"],
                         Type = data_r[,"Freq.type"],
                         Segment = data_r[,"Freq.SegmentNum"]
                         
                         )  
#need number conversion Seg = data_r[,"Freq.Segment"],
#need number conversion Type = data_r[,"Freq.Email.Feature.Type"]

#individual gams on Day Of The Week and Month of The Year only, for visualisation of its effects
gam_weekmonth <- gam(OpenRate ~ s(Weekly, bs = "ps", k = 7) +
               s(Monthly, bs = "cr", k = 12),
                
             data = matrix_gam,
             family = gaussian)  

layout(matrix(1:4, nrow = 1))
plot(gam_weekmonth, shade = TRUE)

summary(gam_weekmonth)

gam_wmhs <- gam(OpenRate ~ s(Weekly, bs = "ps", k = 7) +
                       s(Monthly, bs = "cr", k = 12) +
                       s(Hour)+
                       s(NumSubs),
                     
                     data = matrix_gam,
                     family = gaussian)  

layout(matrix(1:4, nrow = 1))
plot(gam_wmhs, shade = TRUE)

summary(gam_wmhs)

#without splines
#gam with week and month only

gam_0 <- gam(OpenRate ~ s(Weekly, Monthly),
             data = matrix_gam,
             family = gaussian)
#layout(matrix(1:2, nrow = 1))
vis.gam(gam_0,theta = 45, phi = 42,ticktype = "detailed", color = "topo")

summary(gam_0)

#gam with monthly and weekly and number of subscribers

gam_hour <- gam(OpenRate ~ s(Weekly, Monthly, Hour),
             data = matrix_gam,
             family = gaussian)
summary(gam_hour)

gam_wmhsmerged <- gam(OpenRate ~ s(Weekly, Monthly, Hour, NumSubs),
                data = matrix_gam,
                family = gaussian)
summary(gam_wmhsmerged)

gam_numsubs <- gam(OpenRate ~ s(NumSubs),
                      data = matrix_gam,
                      family = gaussian)
summary(gam_numsubs)

gam_seg <- gam(OpenRate ~Segment,
                data = matrix_gam)
summary(gam_seg)

gam_type <- gam(OpenRate ~Type,
                data = matrix_gam)
summary(gam_type)



