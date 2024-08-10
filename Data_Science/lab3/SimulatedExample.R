m0 = 80; m1 = 70;  # group means
s0 = 2; s1 = 2;    # standard deviations
p0 = 0.5; p1 = 1-p0; # prior probabilities
xval = seq(65, 85,length=100); # set range of x values for plotting
fx_0 = dnorm(xval,mean=m0,sd=s0); # Gaussian density f(x) for group 0
fx_1 = dnorm(xval,mean=m1,sd=s1);  # Gaussian density f(x) for group 1
############################################################
# Classification of candidate X = 78
# compare 
p0 * dnorm(78,mean=m0,sd=s0) 
# and 
p1 * dnorm(78,mean=m1,sd=s1) 
############################################################
# Plot densities and discriminant function
# Graphic parameters 
par(mfcol=c(2,1), mar=c(4,4,1.0,0.5), oma=c(1.5,2,1,1))
plot(xval, fx_0, type="l", col="red",    main = " Densities ",
		xlab="",  ylab="");  
lines(xval, fx_1, col="blue", lwd=1, lty=1) 
plot(xval, log(p0) + log(fx_0), col="red",  type="l", main = " Discriminant function ",
	xlab="",  ylab="")  
lines(xval, log(p1) + log(fx_1), col="blue",  lty=1) 
#############################################################
# Missclassification probability
p1g0 =  pnorm((75-m0)/s0)# P(x < 75|G = 0)
p1g0
p0g1 =  1-pnorm((75-m1)/s1)# P(x > 75|G = 1)
p0g1
#############################################################
# Consider the case when  p0 = 0.1; p1 = 1-p0; i.e. class 0 is much rarer
p0 = 0.1; p1 = 1-p0; 
plot(xval, p0 * fx_0, type="l", col="red",    main = " Densities ",
		xlab="",  ylab="")  
fx_1 = dnorm(xval,mean=m1,sd=s1)
lines(xval, p1 * fx_1, col="blue", lwd=1, lty=1) 

plot(xval, log(p0)+log(fx_0), col="red",  type="l", main = " Discriminant function ",
	xlab="",  ylab="")  
lines(xval, log(p1)+log(fx_1), col="blue",  lty=1) 
############################################################
# Simulate N=1000 dimensional training sample from population with parameters
m0 = 80; m1 = 70;  # group means
s0 = 2; s1 = 2;    # standard deviations
p0 = 0.5; p1 = 1-p0; # prior probabilities
#########
N = 1000;
N0 = round(p0 * N); N1 = N-N0; 
x0 = rnorm(N0, m0, s0);
x1 = rnorm(N1, m1, s1);
G = c(rep(0, N0), rep(1, N1));
x = c(x0, x1);
hist(x, 30)   # sample distribution of X (histogram)
fx_0 = dnorm(x,mean=m0,sd=s0);   # Gaussian density f(x) for group 0
fx_1 = dnorm(x,mean=m1,sd=s1);   # Gaussian density f(x) for group 1
Ghat = (p1 * fx_1 > p0 * fx_0);  # Classifier
table(G,Ghat)  # Confusion matrix
 

	