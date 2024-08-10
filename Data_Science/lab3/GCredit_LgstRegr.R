#---------------------------------------------------------------------
# German Credit Data - Logistic regression
#---------------------------------------------------------------------
rm(list = ls()) # remove previous objects
cdata = read.csv("german.csv", header = T);
attach(cdata);
N = nrow(cdata);
names(cdata);
# Dependent variable: Creditability:
#  1 : credit-worthy;  2 : not credit-worthy 
Group = Group-1; # change the score to 0, 1.
table(Group);
table(Group, Purpose)
plot(CreditAmount,Group,xlab="CreditAmount",ylab="Credit Worthy",
     col = c("red", "blue")[factor(Group)]) 
model_full = factor(Group)~
                  Duration+factor(StatusCAccount)+factor(CreditHistory)+
                  factor(Purpose)+CreditAmount+factor(Purpose)+
                  factor(SAccBonds)+factor(Employment)+factor(Instalment)+
                  factor(PStatusSex)+factor(OtherDebtors)+
                  factor(Residence)+factor(Property)+Age+
                  factor(OtherPlans)+factor(Housing) +NCredits+
                  factor(Job)+NPeople+factor(Telephone)+factor(Foreign)+
                  I(CreditAmount^2)+I(Duration^2)+I(Duration*CreditAmount);
 
full = glm(model_full,  family=binomial(link="logit"));
summary(full);
# restricted model (containing 2 explanatory variables)
model_restr = factor(Group)~Duration+CreditAmount;
restr = glm(model_restr,family=binomial(link="logit"));
summary(restr)
# stepwise selection (using the standard stat library)
sel.f = step(restr, scope=formula(full), direction="forward", k=log(N))
summary(sel.f)
sel.b = step(full, scope=formula(full), direction="backward", k=log(N))
summary(sel.b)
# the last option is for BIC instead of AIC
sel =  glm(formula(sel.f), family=binomial(link="logit"));
summary(sel)
 
p.hat = predict(sel, type="response"); # predicted values
g.hat = p.hat>0.5# classifier
logits = predict(sel);        # returns the linear predictor X*b 
table(factor(Group), p.hat>0.5)       # confusion table
# gof
r = residuals(sel, type="pearson"); # residuals
barplot(r)
outlier = which(abs(r) == max(abs(r)));
sel$fitted.values[outlier];
sel$y[outlier];
d = residuals(sel, type="deviance");  # deviance residuals
barplot(d)
sum(d^2); # this is equal to the deviance in the summary table
plot(logits, Group, col = c("red", "blue")[factor(Group)]);
lines(logits, p.hat, type = "p", col = c("red", "blue")[factor(Group)]); 