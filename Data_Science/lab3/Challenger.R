challenger = read.table("challenger.dat",head=T)
attach(challenger)
Temperature;
Damage = (NFailed > 0)
Dp = as.numeric(Damage) 
Dp[14] = Dp[14]+0.05;Dp[17] = Dp[17]+0.1;Dp[23] = Dp[23]+0.05;
Dp[13] = Dp[13]-0.05;
 
plot(Temperature,Dp,xlab="Temperature",ylab="Damage",
     col = c("red", "blue")[factor(Damage)])
lgstmodel = glm(Damage~Temperature, family=binomial(link="logit")); 
summary(lgstmodel)
phat = fitted(lgstmodel)
ghat = phat > 0.5
 
plot(Temperature, phat,xlab="Temperature",ylab="fitted p",
     col = c("red", "blue")[factor(Damage)])
abline(h=0.5)
##############################################################
