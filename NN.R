#Neural networks have been shown to have the ability not only to learn the load series but also 
#to model an unspecified nonlinear relationship between load and weather variables

'''
previous demand observations are used as predictors;
• temperature effects are modelled using regression splines;
• errors are serially correlated.'''
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)

m <- model.matrix(~LogSales + Date + TAVG + PRCP, data = newdf)
summary(m)
library(neuralnet)
print(net.weathersales <- 
        neuralnet(TicketsSold~Date+TAVG+PRCP,  
                  data = m, err.fct="sse", 
                  linear.output=FALSE, 
                  likelihood=TRUE, 
                  hidden=10,
                  
                  act.fct = 'tanh'))
gwplot(net.weathersales, selected.covariate="TAVG")
gwplot(net.weathersales, selected.covariate="PRCP")
gwplot(net.weathersales, selected.covariate="Date")

pred <- prediction(net.weathersales)
prem <- melt(id.vars = c("Date", "TAVG","PRCP"), data = pred)


main <- glm(TicketsSold~Date+Var2+Var3, sum.data, family=poisson())
full <- glm(SUM~Var1*Var2*Var3, sum.data, family=poisson())
prediction(net.sum, list.glm=list(main=main, full=full))
