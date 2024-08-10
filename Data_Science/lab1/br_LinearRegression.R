###########################################################################
## LINEAR REGRESSION   							 
## The br.csv data are taken from Principles of Econometrics, by 
## Hill, Griffihs and Lim (Wiley, 2008). 
## 1080 observations on the prices and characteristics of houses sold 
## in Baton Rouge, LA 2005. The variables considered are:
## price sqft Bedrooms Baths Age Occupancy Pool Style Fireplace Waterfront DOM
## price, dollars 
## sqft		total square feet
## bedrooms	number of bedrooms
## baths		number of full baths
## age		age in years
## Occupancy	Owner = 1, Vacant = 2, Tenant = 3
## Pool		Yes = 1, No = 0   
## Style		Traditional = 1, Townhouse = 2, Ranch = 3, New Orleans = 4,
##          Mobile Home = 5, Garden = 6, French = 7, Cottage = 8, 
##          Contemporary = 9, Colonial = 10, Acadian = 11
## Fireplace	Yes = 1, No = 0	
## Waterfront	Yes =1, No = 0
## DOM		Days on the market
###########################################################################
br = read.table("br.csv", sep = ",", header=T) # reads the data from a csv file
attach(br)                                     
# very useful: makes available each of the variables as a separate object 
# Preliminary analysis
hist(price, 30, col = "blue")      # Histogram
hist(log(price), 30, col = "red")  # Histogram of log transformation 
plot(sqft,price, col ="red") # Scatterplot of prices vs sqft
plot(Age, price, col ="red") # Scatterplot of prices vs age (note: age is discrete)
plot( factor(Occupancy), price, col ='red') # Boxplots by Occupancy
plot( factor(Style), price, col='yellow')
 
pairs(price~sqft+Age, upper.panel = panel.smooth )
## regression model
regr = lm(price~sqft+Age+Pool+Bedrooms+Fireplace+Waterfront+DOM) 
summary(regr)  # This summary provides all the relevant info on the model fit 
#### other useful functions for retrieving info about the model fit
coefficients(regr)  # OLS coefficients
CovBeta = vcov(regr) # covariance matrix for model parameters  
CovBeta[2,2] # variance of beta_1_hat
sqrt(CovBeta[2,2]) # std err of beta_1_hat (compare with table)
coefficients(regr)[2]/sqrt(CovBeta[2,2]) # t statistic
confint(regr, level=0.95) # Confidence intervals for model parameters 
########
yf = fitted(regr)   # fitted values
e = residuals(regr) # residuals
mean(e)             # a property of the residuals: zero mean 
hist(e, 60, col = 'blue', main = ' ') # do we observe anything peculiar?
cor(yf,e)     # as we know, residuals are uncorrelated with fitted values 
cor(sqft,e)   # and each of the explanatory variables
plot(price,yf, col ="red") # predicted vs observed (any nonlinearity?)
plot(price,e, col ="red")  # residuals vs observed
abline(h=0)            # draws a horizontal line at zero 
plot(yf,e, col ="red") # residuals vs predicted to check variance
abline(h=0)            
# note: there is evidence for misspecification of functional form
# diagnostic plots produced automatically by lm
################################
h = hatvalues(regr) # diagonal elements of hat matrix
sum(h) # this is trace(H)=p+1
mean(h)
barplot(h, col = 'blue')
abline(h=mean(h), col = 'red')
r = rstandard(regr) # standardized residuals
plot(yf,r)
plot(h,r) 
########
## categorical variables in regression
regr2 = lm(price~sqft+Age+Pool+Bedrooms+Pool+Fireplace+Waterfront+DOM+factor(Occupancy)+factor(Style)) 
summary(regr2)
## adding and removing variables 
summary(update(regr2,  .~. - Fireplace))
summary(update(regr2,  .~. - factor(Occupancy)))