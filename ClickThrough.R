send_to_all$click <- (send_to_all$`Unique Clicks`/send_to_all$`Total Recipients`)

DS <- as.data.frame.table(send_to_all)
summary(DS)
#Setting variables

data_s <- DS

#weekday and month of the year effect
matrix_gam <- data.table(ClickRate = data_s[,"Freq.click"],
                         OpenRate = data_s[,"Freq.Open.Rate"],
                         Monthly = data_s[,"Freq.MonthNum"],
                         Weekly = data_s[,"Freq.WeekNum" ],
                         Hour = data_s[,"Freq.hs"],
                         NumSubs = data_s[,"Freq.Total.Recipients" ],
                         Theme = data_s[,"Freq.ThemeFactor"],
                         Type = data_s[,"Freq.type"],
                         Segment = data_s[,"Freq.SegmentNum"]
                         
)  
#need number conversion Seg = data_r[,"Freq.Segment"],
#need number conversion Type = data_r[,"Freq.Email.Feature.Type"]

#individual gams on Day Of The Week and Month of The Year only, for visualisation of its effects
gam_weekmonth <- gam(ClickRate ~ s(Weekly, bs = "ps", k = 7) +
                       s(Monthly, bs = "cr", k = 12),
                     
                     data = matrix_gam,
                     family = gaussian)  

layout(matrix(1:2, nrow = 1))
plot(gam_weekmonth, shade = TRUE)

summary(gam_weekmonth)

gam_wh <- gam(ClickRate ~ s(Weekly, bs = "ps", k = 7) +
                s(Hour),
              
              data = matrix_gam)  

layout(matrix(1:2, nrow = 1))
plot(gam_wh, shade = TRUE)

summary(gam_wh)
summary(gam_weekmonth)

#without splines
#gam with week and month only

gam_0 <- gam(ClickRate ~ s(Weekly, Monthly),
             data = matrix_gam,
             family = gaussian)
#layout(matrix(1:2, nrow = 1))
vis.gam(gam_0,theta = 45, phi = 42,ticktype = "detailed", color = "topo")

summary(gam_0)

#gam with monthly and weekly and number of subscribers

gam_wmh <- gam(ClickRate ~ s(Weekly, Monthly, Hour),
               data = matrix_gam)
summary(gam_wmh)
vis.gam(gam_wmh, view=c("Weekly","Hour"),theta = 55, phi = 32,cond=list("Month"=7)    ,ticktype = "detailed", color = "topo")
vis.gam(gam_wmh, view=c("Weekly","Monthly"),theta = 55, phi = 32,ticktype = "detailed", color = "topo")


gam_wmht <- gam(ClickRate ~ s(Weekly, Monthly, Hour, Theme),
                data = matrix_gam
)  

layout(matrix(1:2, nrow = 1))
plot(gam_weekmonth, shade = TRUE)






#plotting predicted values
library(ggplot2)
dffx1 <- data.table(value = gam_wmh$fitted.values, data_time = data_s[ , "Freq.WeekNum"] )
dffx2 <- data.table(value = data_s$Freq.Open.Rate, data_time = data_s[ ,"Freq.WeekNum"] )
dffx1$t <- "Fitted"
dffx2$t <- "Real"

dataseason <- rbind(dffx1,dffx2, fill=TRUE) 
dataseason[, type := c(rep("Fitted", nrow(data_s)), rep("Sales", nrow(data_s)))]
ggplot(data = dataseason, aes(data_time, value, group = type, colour = type)) +
  geom_line(size = 0.6) +
  theme_bw() +
  labs(x = "Time", y = "Sales",
       title = "Fit from GAM weather - Week, month + currency price and weather as tensors")


