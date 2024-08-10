% Lab 1: Matrix algebra for data analysis

%---- Slide   example

A = [1  0; 5  -1; 3  2];    % 3 by 2 matrix
B = [2 -1; 3 6];            % 2 by 2 matrix
vi = ones(3,1);
% indexation 
A(3,2)  % element in row 3 col 2
A(:,1)  % first column
A(1,:)  % first row
% matrix transpose 
A'
% matrix multiplication
C =A * B
% B * A % does not exist 
vi'*C
% ----------------- Special matrices
% identity matrix
I = eye(2) 
B * I % neutral element of matrix multiplication
I * B % this is the same as B
% diagonal matrices
D = diag([1 2 3])
% symmetric matrices 
C = A'*A % is a 2 by 2 symmetric matrix 
C = A * A' % is a 3 by 2 symmetric matrix
%--------------------------------------------------------------------------
% Home sales in Baton - Rouge; br.xls dataset.
clear all; clc; clf;
%-----------------------------LOAD DATA------------------------------------
br = xlsread('br.xls');    % we read data from an excel file
X = br(1:end, [1, 2, 5, 11]); % select the columns 1, 2, 5, 11
%--------------------------------------------------------------------------
scatter(X(:,2), X(:,1));                           % scatterplot
plotmatrix(X(1:1080,1:end));
col_names = ['price', 'sqft', 'age', 'DOM'];
[N, p] = size(X);
%------------------------ Means -------------------------------------------
vi = ones(N,1);      % N by 1 vector of ones
x_means = X'*vi/N;
% this is the same as 
mean(X)' % mean() returns a row vector with column means
disp(x_means);
Xc = X - vi * x_means'; % centred observations
mean(Xc) % should be equal to a vector of zeros, however... 
%------------------------ Covariance matrix -------------------------------
S = Xc' * Xc / N;       % variance - covariance matrix
S 
cov(X) % direct calculation of covariance matrix using a Matlab fnct
% produces a different results as the divisor is N-1. To get the same
% result use:
cov(X,1)  % hence, S = cov(X, 1)
%----------------------- Correlation matrix -------------------------------
vd = diag(S); % diagonal elements of S are the variances
D = diag(vd); % creates a diagonal matrix with variances on the diagonal
R = diag(1 ./ sqrt(vd)) * S * diag(1 ./ sqrt(vd)); 
% the same R is produced by the Matlab function corr
corr(X) 
%----------------------- Standardization ---------------------------------- 
Z = Xc * diag(1 ./ sqrt(vd));
mean(Z)  % should be zero 
var(Z,1) % variance is 1 
%--------------------rank and determinant  --------------------------------
% for readablity of results we select a subset of the dataset
X = X(1:30,:)
det(corr(X))
w = 10000-X(:,1);   % we now add new variable to the dataset as a linear combination 
Xnew = [X w];
det(corr(Xnew))
rank(corr(Xnew))
%------------------- trace
trace(R) % this is equal to the number of variables
%------------------- inverse matrix
inv(R);
inv(R)  * R
inv([1 0.2; 0.2 1])
inv([1 0.9; 0.9 1])
inv([1 -0.9; -0.9 1])
inv([1 1; 1 1])
inv([1 -1; -1 1])
X = br(1:end, [1, 2, 5, 11, 6]); % select the columns 1, 2, 5, 11 and 6
%--------------------------------------------------------------------------
% Visualisation of multivariate data
scatter(X(:,2), X(:,1));                           % scatterplot
gscatter(X(:,2),X(:,1),X(:,end),'rbg' );      % by a classification variable
plotmatrix(X( : ,1:4));
varNames = {'price'; 'sqft'; 'age'; 'DOM'};
gplotmatrix(X(:,1:4),[],X(:,5),['g' 'b' 'r'],[],[],false);
parallelcoords(X(:,1:4), 'group', X(:,5), 'standardize', 'on' , 'labels', varNames);
glyphplot(X(1:30,1:4), 'glyph','star', 'varLabels', varNames );
% univariate graphical displays
boxplot(X(:,1))
boxplot(log(X(:,1))) 
hist(X(:,1),30)
hist(X(:,1),120)
hist(log(X(:,1)),30)
hist(log(X(:,1)),120)
% Bivariate histogram
hist3(log(X(1:1080,1:2)),[50, 50]);
xlabel('Price'); ylabel('Sqft');