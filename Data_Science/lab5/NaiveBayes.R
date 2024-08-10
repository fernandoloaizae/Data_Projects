library(klaR)
#######################################################
data(iris)
mN = NaiveBayes(Species ~ Sepal.Length + Sepal.Width +
                 Petal.Length + Petal.Width , 
                 usekernel = TRUE,
                 data = iris,
                 prior = c(1,1,1)/3)
plot(mN)
predict(mN)
table(iris$Species, predict(mN)$class) # confusion matrix
#######################################################
cdata = read.csv("german.csv", header = T);
attach(cdata);
 
NaBa = NaiveBayes(factor(Group)~Duration+CreditAmount+Age+I(CreditAmount^2)+
                  I(Duration^2)+I(Duration*CreditAmount), 
                 usekernel = TRUE,
                 data = cdata,
                 prior = c(0.7,.3))
plot(NaBa)
p = predict(NaBa)
table(factor(Group), predict(NaBa)$class) # confusion matrix 