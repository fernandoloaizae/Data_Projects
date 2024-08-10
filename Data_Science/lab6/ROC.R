# RECEIVER OPERATING CHARACTERISTIC CURVE IN R
cdata = read.csv("german.csv", header = T);
y = Group-1; # change the score to 0, 1.
cdata = data.frame(y, cdata[,1:20]);
attach(cdata);
N = length(y);
sel =  glm(y~Duration+CreditAmount+Age+factor(StatusCAccount) + 
       factor(CreditHistory),family=binomial(link="logit"));
nullm =  glm(y~1 ,family=binomial(link="logit")); 
p.hat = predict(sel, type="response"); # predicted values
 
u = runif(N);
I1 = as.numeric(u < 0.7);
I0 = 1-I1;
p.hat.rg = u * I1 + (1-u) * I0;
 

library(ROCR)
pred = prediction(p.hat, y)
pred.rg = prediction(p.hat.rg, y)
perf = performance( pred, "tpr", "fpr" )
perf.null = performance( pred.rg, "tpr", "fpr" )
windows()
plot(perf, colorize = T,
     print.cutoffs.at=seq(0,1,by=0.1));
windows()
plot(perf.null, colorize = T,
     print.cutoffs.at=seq(0,1,by=0.1));
 