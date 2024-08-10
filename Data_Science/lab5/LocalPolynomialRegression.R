###########################################################################
library(foreign)
clothing = read.dta("clothing.dta" );
attach(clothing); 
x = log(hoursw);
y = log(tsales)
## loess: local polynomial fitting using a nn bandwidth and a tricube kernel
# span controls the size of the neighbourhood 
# span < 1 is the proportion of points that will be used for fitting
# loess.smooth is useful for plotting
# the default value for p is 1 in loess.smooth
plot( log(hoursw), log(tsales) , 
      pch=1, cex = 0.6, # plotting symbol and dimension 
      ylab="log(tsales)", xlab="log(hoursw)");
abline(lm( y ~  x ), lty=2, col=2, lwd=1)
lines(loess.smooth(x, y, span=.9), col=3, lty=1, lwd=3)
lines(loess.smooth(x, y, span=.5), col=4, lty=2, lwd=2) 
lines(loess.smooth(x, y, span=.1), col=5, lty=3, lwd=2)
legend(5.0,12, c("span = 1", "span = 0.9", "span = 0.5", "span = 0.1"), 
       cex = 0.6, lty=c(2,1,2,3), col = 2:5 )
# Loess function in R -----------------------------------------
help(loess)
fit1 = loess(y~ x, span=.9, degree = 1) 
names(fit1);
fit1$trace.hat ; # df (complexity of model)
################################################################
# Selection of bandwidth
p = 1;
lambda = seq(0.4, 0.99, 0.01);
m = length(lambda); # 
N = length(x);    # sample size
maic = rep(0, m); mbic = rep(0, m); mgcv =rep(0, m);
for (i in 1:m)
{
  l = lambda[i];
  fit = loess(y~ x, span=l, degree = p);
  df = fit$trace.hat;
  s2 = (fit$s)^2;
  maic[i] = log(s2) + 2*df/N;  
  mbic[i] = log(s2) + log(N)*df/N;
  mgcv[i] = s2/((1-df/N)^2);
}
 
plot(lambda, mgcv);  # selected model has span = 0.9
 
 
# confidence bands
fit = predict(fit1 ,  se=TRUE)
str(fit)
plot( log(hoursw), log(tsales) , 
      pch=1, cex = 0.6, # plotting symbol and dimension 
      ylab="log(tsales)", xlab="log(hoursw)");
 
lines(sort(x), fit$fit[order(x)], col = 2, lwd = 3);
lines(sort(x) , (fit$fit + 2*fit$se.fit)[order(x)], col=4, lty=2, lwd=2)
lines(sort(x) , (fit$fit - 2*fit$se.fit)[order(x)], col=4, lty=2, lwd=2)
 
###############################################################
# Local polynomial fitting in 3 dimensions
x1  = log(hoursw)
x2 = log(ssize)
mod.loess= loess(y~x1*x2, span=.5, degree=1)
summary(mod.loess)

new.x = expand.grid(
  x1 = x1r <- seq(min(x1), max(x1), length=50),
  x2 = x2r <- seq(min(x2), max(x2), length=50)
  )
fit = predict(mod.loess, new.x)
# 3D perspective plot of fitted surface
 
graph = persp(x=x1r, y=x2r, fit,
               xlab="log(hoursw)", ylab="log(ssize)", zlab="log(tsales)", expand=0.5,
               col="gray70", ticktype="detailed", theta=-45)
# this function from the persp() help file 
trans3d = function(x,y,z, pmat) 
{ 
  tr = cbind(x,y,z,1) %*% pmat
  list(x = tr[,1]/tr[,4], y= tr[,2]/tr[,4])
}
points(trans3d(x=x1, y=x2, z=y, pmat=graph), col=2, pch=16)

# contour plot of fitted surface
contour(x=x1r, y=x2r, fit, xlab="log(hoursw)", ylab="log(ssize)", ,
        levels=seq(min(y), max(y), length=20), labcex=0.6, col = "blue")
abline(h=seq(2, 8, by=1), v=seq(3, 8, by=1), col="gray80")
###################################################################
 
 