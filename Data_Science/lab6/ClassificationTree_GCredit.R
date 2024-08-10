############################################################
library(rpart)
# The function rpart runs a 10 fold crossvalidation and stores
# the results
# If y is a factor the method class is assumed
# The criterion used for the splitting is the Gini index
############################################################
cdata = read.csv("german.csv", header = T);
attach(cdata);
# Target variable: Creditability - replace variable with y in dataset
#  1 : credit-worthy;  2 : not credit-worthy 
y = Group-1; # change the score to 0, 1.
cdata = data.frame(y, cdata[,1:20]);
Ntot = nrow(cdata);
set.seed(22);      # random seed 
N = 800;           # size of training sample
s = sample(1:Ntot, N);  # index of selected units
train = cdata[s,]
test = cdata[-s,]
 
# classification tree using rpart
rpart.out = rpart( factor(y)~.,data=train, method ='class')
# missclassification error in training sample
pred.rpart = predict( rpart.out, train[,-1], type="class" )
merror.rpart = mean( pred.rpart!= train$y )
merror.rpart
# missclassification error in validation sample
pred.rpart = predict( rpart.out, test[,-1], type="class" )
merror.rpart = mean( pred.rpart!= test$y ) 
table(pred.rpart, test$y)
merror.rpart
# plot the tree #########################
par(mfrow=c(1,2))
plot(rpart.out, uniform=T); 
text(rpart.out,use.n=T,all=T, cex=0.5, col='red')
printcp(rpart.out)   #plot cross-validation results
plotcp(rpart.out);    
post(rpart.out, file="GCredit_tree.eps")
# prune the tree ########################
ptree = prune(rpart.out, cp=rpart.out$cptable[which.min(rpart.out$cptable[,"xerror"]),"CP"])
# plot the pruned tree
plot(ptree, uniform=TRUE); 
text(ptree, use.n=TRUE, all=TRUE, cex=.8, col = 'red')
post(ptree, file="ClassificationTree2.eps",   col='red')