###########################################################################
library(foreign)
clothing = read.dta("clothing.dta" );
attach(clothing); 
x1  = log(hoursw)
x2 = log(ssize)
y =  log(tsales);

pairs(data.frame(y,x1, x2), col = 'blue')
 
#########################################################
library(gam)
am = gam(y~s(x1)+s(x2), data = clothing) # cubic spline
summary(am)
windows()
par(mfrow=c(1,2))
plot.gam(am,residuals=TRUE, shade=TRUE, se=TRUE, col = 'red')

#########################################################
 
summary(am)
new.x = expand.grid(
  x1 = x1r <- seq(min(x1), max(x1), length=50),
  x2 = x2r <- seq(min(x2), max(x2), length=50)
  )
fit = predict.gam(am, newdata = new.x)
windows()
graph = persp(x=x1r, y=x2r, matrix(fit, length(x1r), length(x2r)),
               xlab="log(hoursw)", ylab="log(ssize)", zlab="log(tsales)", expand=0.5,
               col="gray70", ticktype="detailed", theta=-45)
# this function from the persp() help file 
trans3d <- function(x,y,z, pmat) 
{ 
  tr <- cbind(x,y,z,1) %*% pmat
  list(x = tr[,1]/tr[,4], y= tr[,2]/tr[,4])
}
points(trans3d(x=x1, y=x2, z=y, pmat=graph), col=2, pch=16)

# you can also estimate an additive model using loess (local polynomial) fitting
am = gam(y~lo(x1)+lo(x2), data = clothing) # 
# etc. 
#########################################################
library(mgcv) #  
#########################################################
cdata = read.csv("german.csv", header = T);
attach(cdata);
# Target variable: Creditability:
#  1 : credit-worthy;  2 : not credit-worthy 
Group = Group-1; # change the score to 0, 1.
############

mspl = gam(factor(Group)~
            s(Duration)+       # Duration in months  
            s(CreditAmount)+          # Amount of credit
            s(Age)+          # Age in years
            factor(Purpose)+      # Purpose of credit
            factor(SAccBonds)+  # Value of savings or stocks
            factor(StatusCAccount)+  # Balance of current account
            Instalment,      # Installment in % of available income
           family=binomial(link="logit"),
           data = cdata);
summary(mspl);
plot(mspl,residuals=TRUE)
windows()
par(mfrow=c(1,3))
plot(mspl,shade=TRUE,seWithMean=TRUE,scale=0, col='red')
gam.check(mspl)  # deviance residuals
mspl$sp  # smoothing parameters
 
