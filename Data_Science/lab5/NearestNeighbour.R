#######################################################
library(class);
cdata = read.csv("german.csv", header = T);
# compute the principal components on the scaled quantititive variables
pc  =  princomp(scale(data.frame(cdata$CreditAmount, 
                                 cdata$CreditAmount^2,
                                 cdata$Age, 
                                 cdata$Duration, 
                                 cdata$Duration^2,
                                 cdata$Duration * cdata$CreditAmount,
                                 cdata$NCredits)))  
x= scale(pc$scores[,1:2]);
y = factor(cdata$Group-1)

ntot = length(y);
N = 700;
set.seed(121212)
s = sample(ntot, N )


x.train = x[s,]; x.val = x[-s,]; 
y.train = y[s];  y.val = y[-s];
nene1 = knn(x.train,x.val,y.train,k=1)
nene5 = knn(x.train,x.val,y.train,k=5)
nene10 = knn(x.train,x.val,y.train,k=10)
# Comparison of missclassification rates
t1 = table(y.val,nene1)
t5 = table(y.val,nene5)
t10 = table(y.val,nene10)
mr1 = 100* (t1[1,2]+t1[2,1])/nrow(x.val) # missclassification rate
mr5 = 100* (t5[1,2]+t5[2,1])/nrow(x.val)  
mr10 = 100* (t10[1,2]+t10[2,1])/nrow(x.val)  
mr1; mr5; mr10;
############# crossvalidation
t1 = table(y.train, knn.cv(x.train, y.train, k = 1))
t5 = table(y.train, knn.cv(x.train, y.train, k = 5))
t10 = table(y.train, knn.cv(x.train, y.train, k = 10))
mr1 = 100* (t1[1,2]+t1[2,1])/N # missclassification rate
mr5 = 100* (t5[1,2]+t5[2,1])/N  
mr10 = 100* (t10[1,2]+t10[2,1])/N)
mr1; mr5; mr10;
err.rate = rep(0, 50);
for (kval in 1:50)
{
  tk = table(y.train , knn.cv(x.train , y.train , k = kval));
  err.rate[kval] = 100* (N-sum(diag(tk)))/N ; # missclassification rate 
}
err.rate 
plot(1:50, err.rate )

###############################################################
x1  = seq(min(x[,1]), max(x[,1]), length  = 100)
x2  = seq(min(x[,2]), max(x[,2]), length = 100)
x1x2grid = expand.grid(pc1 = x1,   pc2=x2)
 
nene = knn(x, x1x2grid, y, k=31)

 
plot(x[,1], x[,2],   type = "n", xlab = "PC1", ylab = "PC2" )
text(x.train[,1], x.train[,2], labels = as.character(y), cex=0.6, col = 'blue')
text(x.val[ ,1], x.val[ ,2], labels = as.character(y.val), cex=0.6, col = 'red')
 
######################################################################        
 

 