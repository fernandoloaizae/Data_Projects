library(glmnet)
# Simulated example - Orthogonal regressors
N = 300;
p = 500;
sigma = 1.8;
X=matrix(rnorm(N*p),N,p)
y=0.5 * X[,410] - 2 * X[,3] + .9 * X[,123] + sigma* rnorm(N);
fit.lasso = glmnet(X, y, family="gaussian")
plot(fit.lasso,label=TRUE)
plot(fit.lasso,xvar = "lambda", label=TRUE)
fit.lasso
cv.fit=cv.glmnet(X, y, nfolds=10)
plot(cv.fit)
coef(cv.fit)
################################################################################
## Correlated explanatory variables
rho = 0.996;
mSigma = (1-rho) * diag(p) + rho * matrix(1,p,p);
C = chol(mSigma) # notice: t(C) %*% C = mSigma
X=  matrix(rnorm(N*p),N,p) %*% C;
cor(X)
y=0.5 * X[,410] - 2 * X[,3] + .9 * X[,123] + sigma* rnorm(N);
fit.lasso = glmnet(X, y, family="gaussian")
plot(fit.lasso,label=TRUE)
plot(fit.lasso,xvar = "lambda", label=TRUE)
fit.lasso
cv.fit=cv.glmnet(X, y, nfolds=10)
plot(cv.fit)
coef(cv.fit)