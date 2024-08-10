###########################################################################
br = read.table("br.csv", sep = ",", header=T) # reads the data from a csv file                               
# We now partition our dataset in two samples.
# We randomly select a training sample of N=600 observations
Ntot = nrow(br);  # total n. of obs.
set.seed(123);    # random seed 
N = 600;          # size of training sample
s = sample(1:Ntot, N);  # index of selected units
train = br[s,];   # training sample
test = br[-s,];   # validation sample
summary(train);   # data set summary 

## regression model formula: full model
model_full = price~sqft+I(sqft^2)+I(sqft^3)+Age+I(Age^2)+I(sqft*Age) +
                   Pool+Baths+Bedrooms+Fireplace+Waterfront+
                   DOM+factor(Occupancy)+factor(Style); 
regr_full = lm(model_full, data = train)
summary(regr_full);   

############### LIBRARY LEAPS ###############################################
#install.packages("leaps")
library(leaps);

############################################################################
############### BEST SUBSET SELECTION #######################################
subr = regsubsets(model_full, 
          data = train,
          method = "exhaustive",
          nbest = 1,
          nvmax = 15,
          force.in = 1); # all the models will contain sqft (1st variable)
                         # if you set it to zero, sqft will be subject to selection 

# Output of best subset selection analysis
subr.out = summary(subr);   # this object will contain the subr output
subr.out$cp;                # Mallow's C_p 
min(subr.out$cp);           # minimum value 
subr.sel= which(subr.out$cp==min(subr.out$cp)); # locates minimum
subr.sel    # this is the index of the selected model
subr.out$outmat; # matrix indicating which variables are included in each mod 
subr.out$outmat[subr.sel,]; # row for selected model
cbind(subr.out$cp, subr.out$adjr2, 
      subr.out$bic, subr.out$rsq); # compare different criteria: Cp, AdjR2, BIC, R2
plot(subr);    
# each row is a model. The shaded rectangle indicate that
# the variable is included in model. The value of BIC is 
# reported 
plot(subr, scale="Cp");

#### other useful functions for retrieving info about the model fit
coef(subr,subr.sel);  # OLS coefficients
# it may be useful to list the variables selected
subr$xnames[subr.out$which[subr.sel,]]

#########################################################################
# Selected model (according to Cp)
model_sel = price ~ sqft+I(sqft^3)+I(Age^2)+I(sqft * Age)+Baths+     
                    Fireplace+I(factor(Occupancy)==2)+
                    I(factor(Style)==2)+I(factor(Style)==4)+
                    I(factor(Style)==6)+I(factor(Style)==7)+
                    I(factor(Style)==10);
regr_sel = lm(model_sel, data = train); 
summary(regr_sel);

#### analysis of selected model ##################################
yf = fitted(regr_sel)   # fitted values
e = residuals(regr_sel) # residuals
hist(e, 50, col = 'blue', main = 'histogram of residual')  
plot(train$price,yf, col ="red") # predicted vs observed (any nonlinearity?)
plot(train$price,e, col ="red")  # residuals vs observed
abline(h=0)            # draws a horizontal line at zero 
plot(yf,e, col ="red") # residuals vs predicted

#################################################################
## Validation: test sample prediction accuracy
## We use the R function predict 
ypred_full = predict(regr_full, newdata = test) 
pred_errors_full = test$price - ypred_full; # full model predictions for the test sample
mse_full = mean(pred_errors_full^2)   # mean square prediction error of full model

ypred_sel = predict(regr_sel, newdata = test)
pred_errors_sel = test$price - ypred_sel;   # selected model predictions
mse_sel = mean(pred_errors_sel^2)
100 * (1- mse_sel/mse_full)  # percent gain in accuracy due to model selection


#########################################################################
############### Forward stepwise selection ######################
subr = regsubsets(model_full, 
                  data = train,
                  method = "forward",
                  nvmax = 15,
                  force.in = 1); # all the models will contain sqft
# Output
subr.out = summary(subr);
subr.out$cp;
min(subr.out$cp);
subr.sel= which(subr.out$cp==min(subr.out$cp)); # locates minimum
subr.out$outmat; # matrix indicating which variables are included in each mod 
subr.out$outmat[subr.sel,]; # row for selected model
cbind(subr.out$cp, subr.out$adjr2, 
      subr.out$bic, subr.out$rsq); # compare different criteria
plot(subr);    
plot(subr, scale="Cp");
coef(subr,subr.sel); # OLS coefficients
subr$xnames[subr.out$which[subr.sel,]]

# Is the selected model different?
########################################################
