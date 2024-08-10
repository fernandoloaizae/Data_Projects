###########################################################################
br = read.table("br.csv", sep = ",", header=T) # reads the data from a csv file                               
# We randomly select a training sample of N=600 observations
Ntot = nrow(br);  # total n. of obs.
set.seed(123);    # random seed 
N = 600;          # size of training sample
s = sample(1:Ntot, N);  # index of selected units
train = br[s,];   # training sample
test = br[-s,];   # validation sample
summary(train);
attach(train); 
######################################################################### 
## Ridge regression 
#########################################################################
install.packages("MASS")
library(MASS);
y = scale(price)  # standardisation of the output
# we need to construct the dummies for the factors Occupancy and Style,
# as we are going to standardise all the inputs
Occ = model.matrix(~factor(Occupancy))[,2:3]; # dummies for occupancy
Sty = model.matrix(~factor(Style))[,2:10]; # dummies for occupancy
# standardisation of the inputs
X = scale(data.frame(sqft, sqft^2, Age, Age^2, sqft*Age, Pool, Bedrooms,Fireplace,Waterfront, 
                Occ, Sty ));
p = ncol(X); # number of columns of X matrix 
##########################################################
## Degrees of freedom df and penalty parameter lambda
## This script produces the plot of df versus lambda in the slides 
## df(lambda) = sum_j (d2_j/ d2_j + lambda)
XtX = t(X) %*% X; # same as crossprod(X) is the X'X matrix 
# eigenvalues and eigenvectors of the matrix X'X
E = eigen(XtX, symmetric= TRUE);  
d2 = E$values; # eigenvalues 
V = E$vectors; 
# spectral decomposition X'X = VDV'
V %*% diag(d2) %*% t(V); # this is the same as XtX
# lambda is a sequence 
lambda = seq(0,1600,.5);
df = rep(0,length(lambda));
for (i in 1:length(lambda))
  {       df[i] = sum(d2 / (d2+lambda[i]));    }   
plot(lambda, df, col = 'blue', type = 'l'); # relation between df and lambda
########################################################
# Ridge regression 
ridge = lm.ridge(y~-1+X,  lambda = lambda);
names(ridge)
# coefficient trace plot (versus lambda)
plot(ridge, lambda); 
# coefficient trace plot (versus df)
matplot(df, coef(ridge), type = "l");
# selection of the regularisation parameter lambda
select(ridge)
plot(lambda[1:10], ridge$GCV[1:10]) ;
# Estimate the selected model using the GCV estimate of lambda
ridge.sel = lm.ridge(y~-1+X,  lambda = 0.5);
#################################################################
## Validation: test sample prediction accuracy
## preparation of the inputs and output for the test sample
y_test = scale(test$price)  # standardised outputs for the test sample
Occ_test = model.matrix(~factor(test$Occupancy))[,2:3]; # dummies for occupancy
Sty_test = model.matrix(~factor(test$Style))[,2:10]; # dummies for occupancy
# standardisation of the inputs for the test sample
X_test = scale(data.frame(test$sqft, test$sqft^2, test$Age, 
                          test$Age^2, test$sqft*test$Age, 
                          test$Pool, test$Bedrooms,test$Fireplace,test$Waterfront, 
                          Occ_test, Sty_test ));
## Prediction from the full model
ypred_full =  X_test %*% coef(lm.ridge(y~-1+X,  lambda = 0.0))  
pred_errors_full = y_test - ypred_full; # full model predictions for the test sample
mse_full = mean(pred_errors_full^2)   # mean square prediction error of full model
#
ypred_sel = X_test %*% coef(lm.ridge(y~-1+X,  lambda = 0.5))
pred_errors_sel = y_test - ypred_sel;   # selected model predictions
mse_sel = mean(pred_errors_sel^2)
100 * (1- mse_sel/mse_full)  # percent gain in accuracy due to model selection
# As an exercise, try to compare the out of sample predictive performance
# for lambda = 300 and lambda = 0.4