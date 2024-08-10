############################################################
# We fit a regression tree to Y using two inputs
# The function rpart in the library rpart 
library(rpart)
###########################################################################
library(foreign)
clothing = read.dta("clothing.dta" );
attach(clothing); 
x1 = log(hoursw); 
x2 = log(ssize);
y  = log(tsales);
# Regression tree using rpart
rtree = rpart( y~x1+x2, method ='anova');
# plot the tree #########################
windows()
par(mfrow=c(1,2))
plot(rtree, uniform=T); 
text(rtree,use.n=T,all=T, cex=0.65, col = 'red')
plotcp(rtree, col='blue')   #plot cross-validation results
########################
x1p = seq(min(x1), max(x1), length = 50)
np = length(x1p)
x2p = seq(min(x2), max(x2), length = 50)
pt = expand.grid(x1 = x1p, x2 = x2p)
fit = predict(rtree, pt ) 
windows()
graph = persp(x=x1p, y=x2p, matrix(fit, np, np),
               xlab="log(hoursw)", ylab="log(ssize)", zlab="log(tsales)", expand=0.5,
               col="darkgreen", ticktype="detailed", theta= -45)
