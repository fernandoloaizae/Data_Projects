n = 100;
prob = 0.1;
y = rbinom(n, 1, prob)
y
table(y)
p = seq(0.001,0.999,0.001);
loglik = sum(y) * log(p) + (n-sum(y)) * log(1-p);
par(mfrow=c(1,2))
plot(p, loglik, type = 'l', col = "red", main = "log likelihood")
plot(p, exp(loglik), type = 'l', col = "blue", main = "Likelihood")
p[loglik==max(loglik)] # maximum likelihood estimate