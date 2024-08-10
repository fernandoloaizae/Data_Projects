#install.packages("glmnet");
library(glmnet);
?glmnet

###########################################################################
br = read.table("br.csv", sep = ",", header=T) # reads the data from a csv file 
attach(br); 

# We randomly select a training sample of N=600 observations
Ntot = nrow(br);  # total n. of obs.
set.seed(401);    # random seed 
N = 600;          # size of training sample
s = sample(1:Ntot, N);  # index of selected units

# partition the dataset
y.train = price[s];
y.test = price[-s];
Occ = model.matrix(~factor(Occupancy))[,2:3]; # dummies for occupancy
Sty = model.matrix(~factor(Style))[,2:10]; # dummies for occupancy
X = data.frame(sqft, sqft^2, Age, Age^2, sqft*Age, Pool, Bedrooms,Fireplace,Waterfront, Occ, Sty );
X.train = as.matrix(X[s,]);
X.test = as.matrix(X[-s,]);
head(X.test) 

######################################################################### 
## Lasso   (note: y and x are standardized by the function glmnet) 
#########################################################################
fit.lasso = glmnet(X.train, y.train, family="gaussian")
plot(fit.lasso,label=TRUE)
plot(fit.lasso,xvar = "lambda", label=TRUE)
fit.lasso
cv.fit=cv.glmnet(X.train, y.train, nfolds=20)
plot(cv.fit)
coef(cv.fit, s="lambda.min")

#########################################################################
y.test.lasso = predict(cv.fit, newx=X.test, s="lambda.min")
y.test.full = predict(cv.fit, newx=X.test, s=0)
pairs(cbind(y.test, y.test.lasso, y.test.full) )
pred.errors.lasso = y.test - y.test.lasso;   # selected model predictions
pred.errors.full = y.test - y.test.full;   # selected model predictions
mse.lasso = mean(pred.errors.lasso^2)
mse.full = mean(pred.errors.full^2)
100 * (1- mse.lasso/mse.full)  # percent gain in accuracy due to shrinkage
