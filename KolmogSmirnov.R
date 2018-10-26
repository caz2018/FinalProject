#Kolmogorov-smirnov test for currency vs sales relationship

x=normalize(newdf$Price, method="range", range=c(-3,3))
y=normalize(newdf$Total_Daily_Value, method="range", range=c(-3,3))
y2=normalize(newdf$Avg_Trans_Value_GBP , method="range", range=c(-3,3))


sa0 <- qqnorm(y, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
sa2 <- qqnorm(y2, main = "Normal Q-Q Plot",
              xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
              plot.it = TRUE, datax = FALSE)

cu0 <- qqnorm(x, main = "Normal Q-Q Plot",
              xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
              plot.it = TRUE, datax = FALSE)

      
# Do x and y come from the same distribution?
ks.test(x, y)

# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")

plot(ecdf(x = x), main = "ECDF of x and y")
lines(ecdf(x = y), col = 2)

t.test(x, y, alternative = "g")
wilcox.test(x, y, alternative = "g")
ks.test(x, y, alternative = "l")

