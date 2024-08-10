#######################################################################
# Density estimation
#######################################################################
library(foreign)
clothing = read.dta("clothing.dta" );
attach(clothing); 
y =  tsales 
#########################################################
s =  sqrt(var(y)) # standard deviation
q3 = quantile(y, 0.75);   # third quartile
q1 = quantile(y, 0.25);   # first quartile 
IQR = q3-q1; #  interquartile range
s =min(s,IQR/1.34);  # robust estimate of standard deviation
N = length(y); 
h.nrr = 1.06 * s/(N^(0.2)); # normal reference bandwidth
h.cv = bw.ucv(y);           
 
hist(y,40, freq = FALSE, col='lightgrey', ylim = c(0,0.000002), axes=F, main = "")
lines(density(y), col="blue", lwd=2); # uses nrr  0.9 * s/(N^(0.2));
lines(density(y , bw = h.cv,  kernel = "epanechnikov"), col = 'red', lwd=2)
lines(density(y , bw = h.nrr, kernel = "gaussian"), col = 'darkgreen', lwd=3); # smooth estimate
lines(density(y , bw = h.cv,  kernel = "gaussian"), col = 'darkred', lwd=2);
rug(y)
legend(0.70* max(y) , 0.000002, legend = c("R's Default", "Epan CV", "Gauss RoT", 
                  "Gauss CV"), col = c("blue", "red", "darkgreen", "darkred"), lty = 1)

