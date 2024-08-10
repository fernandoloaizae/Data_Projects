install.packages("gbm")
library(gbm)
# http://www.r-bloggers.com/how-to-build-a-world-beating-predictive-model-using-r/
############################################################
rm(list = ls())
############################################################
cdata = read.csv("german.csv", header = T);
# Target variable: Creditability - replace variable with y in dataset
#  1 : credit-worthy;  2 : not credit-worthy 
attach(cdata)
y = Group-1; # change the score to 0, 1.
cdata = data.frame(y, cdata[,1:20]);
Ntot = nrow(cdata);
set.seed(22);      # random seed 
N = 800;           # size of training sample
s = sample(1:Ntot, N);  # index of selected units
train = cdata[s,]
test = cdata[-s,]
# ======================================================
####   Gradient boosting classification tree
gbmfit =   gbm(  y ~., data=train, distribution = "bernoulli", 
                 shrinkage = .3, # parameter v (learning rate)
                 bag.fraction = 0.5,  # parameter eta
                 cv.folds=5, n.trees=150)        # parameter M
summary( gbmfit)
############## Selection of the number of iterations (number of trees)
# select M using 50% heldout test set
select.M.iter = gbm.perf(gbmfit,method="test")

# select M on the basis of performance using 5-fold cross-validation
windows()
select.M.iter = gbm.perf(gbmfit,method="cv")
select.M.iter

# plot the performance
# plot variable influence
summary(gbmfit,n.trees=1)             # based on the first tree
summary(gbmfit,n.trees=select.M.iter) # based on the estimated M (cv)
# predictions (posterior probabilities of being credit-worthy) for test sample
pred.test.gbm = predict( gbmfit,  test[,-1], type="response", n.trees=select.M.iter )
hist(pred.test.gbm,30)
pred.gbm = as.numeric(pred.test.gbm > .5 ) # classifier 
error.gbm = mean( pred.gbm!=test[,1] )
table( pred.gbm, test[,1] )
error.gbm
# ========================================= 
install.packages("randomForest")
library(randomForest)
# ================== random forest
rf = randomForest(as.factor(y)~., data=train,
                  ntree=200,
                  mtry = 5, # number of variables sampled 
                  importance=TRUE)
print(rf)
importance(rf)
getTree(rf, k= 1, labelVar=TRUE)
getTree(rf,k = 22)
windows()
par(mfrow=c(1,3))
partialPlot(rf, train, Duration, "1", col = 'red', main = "")
partialPlot(rf, train, CreditAmount, "1", col = 'red', main = "")
partialPlot(rf, train, Age, "1", col = 'red', main = "")
# missclassification error
pred.test.rf = predict( rf, newdata=test[,-1], type='prob')[,2]
pred.rf = as.numeric(pred.test.rf > .5 ) # classifier
error.rf = mean( pred.rf!=test[,1] )
table( pred.rf, test[,1])
error.rf

# ROCR curves #########################
install.packages("gplots")
install.packages("ROCR")
library(ROCR) 
perf.gb = performance( prediction(pred.test.gbm, test[,1]), "tpr", "fpr" )
perf.rf = performance( prediction(pred.test.rf,  test[,1]), "tpr", "fpr" )
windows()
plot(perf.gb, col='red',
     print.cutoffs.at=seq(0,1,by=0.1));
plot(perf.rf, col='blue',
     print.cutoffs.at=seq(0,1,by=0.1), add = TRUE);
