#Dynamic harmonic regression with multiple seasonal periods

#Seasonal periods 48 and 336

fit <- auto.arima(salesmsts, seasonal=FALSE, lambda=0,xreg=fourier(salesmsts, K=c(10,10)))
