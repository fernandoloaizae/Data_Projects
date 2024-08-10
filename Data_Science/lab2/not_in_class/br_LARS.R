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
## LARS Regression: library(lars)
#########################################################################
install.packages("lars")
library(lars);
# preparation of input data and output
y = scale(price)  # standardisation of the output
# we need to construct the dummies for the factors Occupancy and Style,
# as we are going to standardise all the inputs
Occ = model.matrix(~factor(Occupancy))[,2:3]; # dummies for occupancy
Sty = model.matrix(~factor(Style))[,2:10]; # dummies for occupancy
# standardisation of the inputs
X = scale(data.frame(sqft, sqft^2, Age, 
                     Age^2, sqft*Age, 
                     Pool, Bedrooms,Fireplace,Waterfront, 
                     Occ, Sty ));
p = ncol(X); # number of columns of X matrix
##########################################################
# LEAST ANGLE REGRESSION
l = lars(X, y, type = "lar", intercept = FALSE, trace = TRUE);
names(l)

# Values of Mallow's Cp verus step number
plot(l, xvar= c("step"), plottype = c("Cp"))
# Coefficients profile plot (versus step)
plot(l, xvar= c("step"), plottype = c("coefficients"))
# note: the last step gives the LS estimates  
# note: standardized coefficients  = beta_j * sqrt(deviance x_j)
# when the variables are standardized, sqrt(deviance x_j) = sqrt(N)
 
 
# Coefficient profiles of the lasso solutions versus L1 arc length,
# defined as the sum of the absolute changes, sum |delta_s|, of the
# coefficients, see
plot(l, xvar= c("arc.length"), plottype = c("coefficients"))
#
# Coefficient profiles of the lasso solutions versus the shrinkage factor
# s = t/max(t), where  t = sum_j |??_j| (arc length)
# represents the total departure from zero and thus model complexity;
# max(t) is obtained for the ols estimates
plot(l); # 
summary(l);
coef(l);  # values of the coefficients at different iterations
# the last line are the LS coefficients
# notice: variable 2 is entered first with coefficient 0.6371975;
# this is smaller than the LS coefficient, 0.826, compare
lm(y~ X[,2]);  #

# Model selection by 10-fold cross validation (K=10)
cv.l = cv.lars(X, y, intercept = FALSE, K =  10, trace = TRUE, type = "lar" )
ind.l = which(cv.l$cv == min(cv.l$cv));
ind.l
coef(l)[9,];  # step 9 is selected by crossvalidation
# note: some of the coefficients are zero
# also, the CV score is very flat as a function of the number of steps
 
############  LASSO          #############################
##########################################################
la = lars(X, y, type =  "lasso", intercept = FALSE, trace = TRUE);
la
plot(la, xvar= c("step"), plottype = c("Cp"))
plot(la, xvar= c("step"), plottype = c("coefficients"))
plot(la, xvar= c("arc.length"), plottype = c("coefficients"))
plot(la); 

summary(la);
coef(la);
# selection
cv.la = cv.lars(X, y, intercept = FALSE, K =  10, trace = TRUE, type = "lasso");
ind.la = which(cv.la$cv == min(cv.la$cv));
frac.sel = cv.la$index[ind.la]; # sected fraction of final L1 norm
# selected model coefficients
la.coef = predict.lars(la, type="coefficients", mode="fraction", s=frac.sel)
la.coef
# compare with
coef(l)[ind.l,];
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
ypred_full = predict.lars(l, X_test, s = 20, type="fit")$fit;   
pred_errors_full =  y_test - ypred_full; # full model predictions for the test sample
mse_full = mean(pred_errors_full^2)   # mean square prediction error of full model
#
ypred_sel =predict.lars(l, X_test, s = ind.l, type="fit")$fit;
pred_errors_sel = y_test - ypred_sel;   # selected model predictions
mse_sel = mean(pred_errors_sel^2)
100 * (1- mse_sel/mse_full)  # percent gain in accuracy due to model selection

ypred_sel =predict.lars(la, X_test, s = frac.sel, type="fit", ,mode="fraction")$fit;
pred_errors_sel = y_test - ypred_sel;   # selected model predictions
mse_sel = mean(pred_errors_sel^2)
100 * (1- mse_sel/mse_full) 