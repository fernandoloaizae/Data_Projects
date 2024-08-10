#################################################################
# Smoothing Cubic splines
# Simulated example
N = 100;
x = runif(100, 0,1);  # random design 
x = seq(0, 1, length.out = N);  # equally spaced points
f = sin(12*x+0.2)/(x+0.2); #true regression function  
e =  rnorm(N, 0, 1);
y = f+e;                     # observations 
plot(x, y, type = "p", col="red");
lines(x, f, type="l", col="blue", lwd = 3);
legend(0.25,3.5 , c("y", "f(x)"), lty=1, col = c("red", "blue"), cex = 0.9)
#########################################################
# Smoothing Spline Estimation 
# lambda is estimated by cross validation
ssplinefit.cv = smooth.spline(x, y, cv = T)  
ssplinefit.cv 
names(ssplinefit.cv)
ssplinefit2 = smooth.spline(x, y, df = 2) # df is set equal to 2
ssplinefit20 = smooth.spline(x, y, df = 20) # df is set equal to 20
ssplinefit50 = smooth.spline(x, y, df = 50) # df is set equal to 50
 
 
plot(x, y);
lines(ssplinefit.cv, col = 2, lwd = 3);
lines(ssplinefit2, col = 3, lwd = 2);
lines(ssplinefit20, col = 4, lwd = 2);
lines(ssplinefit50, col = 5, lwd = 2);
legend(0.3,3, c("CV", "df=2", "df=20", "df=50"), lty=1, col = 2:6 )
 
  