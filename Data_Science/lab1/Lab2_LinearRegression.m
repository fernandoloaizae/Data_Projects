% Home sales in Baton - Rouge; br.xls dataset.
clear all; clc; clf;
%-----------------------------LOAD DATA------------------------------------
br = xlsread('br.xls');    % we read data from an excel file
y = br(:,1);                % select output variable  
X = br(:, [2, 5, 7, 3, 9, 10, 11]); % select the inputs
% select the columns for Age, Pool, Bedrooms, Pool, Fireplace, Waterfront, DOM
[N, p] = size(X);           % Matrix dimensions    
X = [ones(N,1), X];         % append a column of ones
%------------------------ OLS ESTIMATOR -----------------------------------
beta_hat = inv(X'*X) * X'*y
%------------------------ fitted values and residuals----------------------
H = X * inv(X'*X) * X';     % Hat matrix 
y_hat = H * y;              % Fitted values. Same as 
y_hat = X * beta_hat;       %  
e = y - y_hat;              % Residuals
hist(e, 50)                 % Residuals histogram
corr(e, y_hat)              % zero correlation property 
X' * e                      % vector of zeros (apart from numerical error)
h = diag(H);                % diagonal elements of H, leverage
trace(H)                    % sum of diagonal element == number of variables 
%------------------------ estimation of sigma2-----------------------------
s2 = e'*e / (N-p-1);        
s = sqrt(s2);               % standard error of regression
%------------------------ gof ------------------------------- 
TSS = sum((y-mean(y)).^2);    % Total sum of squares
ESS = sum((y_hat-mean(y_hat)).^2); % Regression (explained) sum of squares 
RSS = sum(e.^2);            % Residual sum of squares
R2 = ESS/TSS;               % R-squared - Coefficient of determination
R2a = 1-(RSS/(N-p-1))/(TSS/(N-1));  % Adjusted R-squared
%------------------------ Properties of OLS estimates --------------------- 
Cov_beta = s2 * inv(X'*X);  % Covariance matrix of beta_hat
var_beta = diag(Cov_beta);  % variance of parameter estimates
t_stat = beta_hat ./ sqrt(var_beta); % t statistic
p_values = 2*(1-tcdf(abs(t_stat), N-p-1));
F_stat = (ESS/p) / (RSS/(N-p-1)); % F-statistic
1-fcdf(F_stat, p, N-p-1)          % p-value  
