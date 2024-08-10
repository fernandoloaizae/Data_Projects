# Simulated example - Orthogonal regressors
N = 300;
p = 500;
sigma = 1.8;
set.seed(123) # to random the x
X=matrix(rnorm(N*p),N,p) # all the Xs are created like this
y=0.5 * X[,410] - 2 * X[,3] + .9 * X[,123] + sigma* rnorm(N); # the true value we look for
fit.lm = lm(y~X) # we cant do it because the rank of x is 300, not full rank
summary(fit.lm) # when p > N, OLS fails
# we can at most estimate N coefficients
#----------------------------------------------------------------
N = 1000; # in order to do it, we increase the N value from 500 to 1000 so we have N > p
p = 500;
sigma = 1.8;
set.seed(123)
X=matrix(rnorm(N*p),N,p)
y=0.5 * X[,410] - 2 * X[,3] + .9 * X[,123] + sigma* rnorm(N);
fit.lm = lm(y~X, -1)
summary(fit.lm)
beta.hat = coefficients(fit.lm)  # OLS coefficients
CovBeta.hat = vcov(fit.lm) # covariance matrix for model parameters  
stderr = sqrt(diag(CovBeta.hat)) # std err of beta_hat (compare with table)
t.values = beta.hat/stderr
p.values =  2* (1-pt(abs(t.values), N-p))
sort(p.values)
sort(p.values)  < 0.05/p
# if we estimated the true model: 
summary(lm(y~X[,410]+X[,3]+X[,123], -1) )
################################################################################
## Correlated explanatory variables
rho = 0.96;
mSigma = (1-rho) * diag(p) + rho * matrix(1,p,p);
C = chol(mSigma) # notice: t(C) %*% C = mSigma
X=  matrix(rnorm(N*p),N,p) %*% C;
cor(X)
y=0.5 * X[,410] - 2 * X[,3] + .9 * X[,123] + sigma* rnorm(N);
fit.lm = lm(y~X, -1)
summary(fit.lm)
beta.hat = coefficients(fit.lm)  # OLS coefficients
CovBeta.hat = vcov(fit.lm) # covariance matrix for model parameters  
stderr = sqrt(diag(CovBeta.hat)) # std err of beta_hat (compare with table)
t.values = beta.hat/stderr
p.values =  2* (1-pt(abs(t.values), N-p))
sort(p.values)
sort(p.values)  < 0.05/p