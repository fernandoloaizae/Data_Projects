###########################################################################
# Nadaraya-Watson kernel regression estimate 
require(stats); require(graphics)
data(cars);
attach(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
     las = 1)
lines(ksmooth(speed, dist, "normal", bandwidth = 1), col = 'red')
lines(ksmooth(speed, dist, "normal", bandwidth = 5), col = 'blue')
rm(list = ls())
###########################################################################
# US inflation
infl = read.csv('USInflation.csv');
y = ts(infl, start = c(1948, 1), frequency = 12) 
T = nrow(infl)
t = time(y)

nw.fit = ksmooth(t, y, "normal", bandwidth = 5.4) 
y.hat = ts(nw.fit$y, start = c(1948, 1), frequency = 12)
ts.plot(y, y.hat)
plot(y, col='red')
lines(y.hat, lwd = 2, col = 'blue')
#######################################################################
rm(list = ls())

install.packages('ISLR')
library(ISLR)
attach(Wage)
help(Wage)
y = log(wage)
x = log(age)
xrange =range(x)
x.grid=seq(from = xrange[1], to = xrange[2], by = 0.01)
plot(x, y,xlim=xrange, cex=.5, col="darkgrey")
title("Local Regression")
fit =loess(y~x,span=.2)
fit2=loess(y~x,span=.5)
lines(x.grid,predict(fit,data.frame(x=x.grid)),col="red",lwd=2)
lines(x.grid,predict(fit2,data.frame(x=x.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
 


#######################################################################
# Density estimation
#######################################################################
y = age
s =  sqrt(var(y)) # standard deviation
q3 = quantile(y, 0.75);   # third quartile
q1 = quantile(y, 0.25);   # first quartile 
IQR = q3-q1; #  interquartile range
s =min(s,IQR/1.34);  # robust estimate of standard deviation
N = length(y); 
h.nrr = 1.06 * s/(N^(0.2)); # normal reference bandwidth
h.cv = bw.ucv(y);           

hist(y,40, freq = FALSE, col='lightgrey')
lines(density(y), col="blue", lwd=2); # uses nrr  0.9 * s/(N^(0.2));
lines(density(y , bw = h.cv,  kernel = "epanechnikov"), col = 'red', lwd=2)
lines(density(y , bw = h.nrr, kernel = "gaussian"), col = 'darkgreen', lwd=3); # smooth estimate
lines(density(y , bw = h.cv,  kernel = "gaussian"), col = 'darkred', lwd=2);
rug(y)
legend(0.70* max(y) , 0.000002, legend = c("R's Default", "Epan CV", "Gauss RoT", 
                                           "Gauss CV"), col = c("blue", "red", "darkgreen", "darkred"), lty = 1)


 


