###########################################################################
br = read.table("br.csv", sep = ",", header=T) # reads the data from a csv file                               
attach(br); 

# preparation of input data and output: standardization
y = scale(price)
X = scale(data.frame(sqft, sqft^2, sqft^3, Age, Age^2, Age^3, sqft*Age, Baths, Bedrooms));
# correlation matrix
S = cor(X)
# Eigenvalues and eigenvectors of S matrix
print(S, digits = 2)
print(eigen(S), digits = 2)

####################################################################
# We can obtain the principal components directly
# using the princomp function:
pc.br = princomp(X);
Z = pc.br$scores; # principal components scores
pairs(Z[,1:4]);   # scatterplot of the first 4 pc's
plot(pc.br);      # bar plot of the eigenvalues (screeplot)
pc.br$sdev        # Standard deviation of the pc's 
# same as sqrt of the eigenvalues:
sqrt(eigen(S)$values)
print(var(Z), digits=5);   # pc's are orthogonal and have decresing variances
# creates a matrix with zero covariances and eigenvalues as variances

####################################################################
# Principal components regression: regress y on Z (no intercept)
p = ncol(Z)
N = nrow(Z)
AIC = rep("NaN",p)
for (i in 1:p)
{
  pcr = lm(y~-1+Z[,1:i]);
  print(summary(pcr));
  AIC[i] = 2*log(summary(pcr)$sigma)+2*i/N;
}
plot(1:p, AIC) 
pcregr = lm(y~-1+Z);
summary(pcregr); 
# compare with 
summary(lm(y~-1+X)); #-1 is to not include the intercepts
# would you support a strategy that includes only the first
# m pc's?
# Later on we will introduce the following useful graph:
biplot(princomp(X), cex = 0.6);