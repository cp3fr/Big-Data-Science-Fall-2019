#Big Data Science: Assignment 1 - Solutions
#
# This assignment is based on the R tutorial code on regression, crossvalidation, 
# and regularization. The data used in the tutorial are Big Five personality trait 
# questionnaire scores from 2800 students, including thei age, education, and gender. 
# There are a total of six assignments. Grading is based on the correctness of the 
# output variables and the code used to produce the output.
#
# To achieve a maximum score you will need to use the internet (e.g. function documentations, 
# stack overflow) to learn about additional functions and parameter settings that were 
# not part of the tutorial.
#
# Author: christian.pfeiffer@uzh.ch



#Load software packages and prepare the data

#load required packages
if(!require("psych")){
  install.packages("psych")
  library("psych")
}

if(!require("glmnet")){
  install.packages("glmnet")
  library("glmnet")
}

if(!require("Matrix")){
  install.packages("Matrix")
  library("Matrix")
}

#load the dataset from: https://www.personality-project.org/r/html/bfi.html
data('bfi')

#remove participants with missing values
ind = rowSums(is.na(bfi))==0
bfi = bfi[ind,]

#Features: Big Five questionnaire scores
X = bfi[,c(1:5,6:10,11:15,16:20,21:25)]
rownames(X)= NULL
X = data.matrix(X, rownames.force = NA)

#Labels: Gender
y = bfi[,c(26)]
rownames(y)= NULL
y = data.matrix(y, rownames.force = NA)
y = rowMeans(y)


#Task 1
#
#Inspect your data. Return the following variables "nfeatures" (number of features), "nsamples" 
#(number of samples), "minlabel" (minimum value across labels), "maxlabel" (maximum value across labels).

nfeatures = dim(X)[2]
nsamples = dim(X)[1]
minlabel = min(y)
maxlabel = max(y)

print(nfeatures)
print(nsamples)
print(minlabel)
print(maxlabel)


#Task 2
#
#Predict y from X using a linear regression (no regularization, no crossvalidation). Return a variable 
#"rsquared" that contains the R-squared value of the model fit with at least four-digit precision (e.g. rsquared = 0.9752).

options(digits=4) # if we want to keep 4 significant digits.
fit.lm <- lm(y~X)
plot(fit.lm)
s.fit.lm <- summary(fit.lm)
print(s.fit.lm$r.squared)


#Task 3
#
#Predict y from X using a LASSO regression (no crossvalidation) with lambda=0.05. Inspect the 
#coefficients of the fitted LASSO model (e.g. "fit$beta") and return a variable "ncoeff" that contains 
#the number of non-zero coefficients in the model.

fit=glmnet(X,y,alpha=1,lambda=0.05)

ncoeff = sum(fit$beta!=0)

print(ncoeff)


#Task 4
#
#Predict y from X using a RIDGE regression (no crossvalidation). Hint: The difference between RIDGE and 
#LASSO regresssion in glmntet is in the choice of the alpha parameter (Use the internet to learn more 
#about the alpha parameter in glmnet, and about the difference between RIDGE and LASSO).
#
#Your task is to compare the coefficients two different RIDGE model model fits. The first model uses less 
#regularization (lambda=0.01) and the second model more regularization (lambda=1). Return two variables 
#"less_reg" and "more_reg" that each contain the minimum, maximum, and max-min difference of coefficient 
#values for each case, using at least a four-digit precision. Example: coefficients=c(-2,0,-5,4,1), 
#return_variable=c(-5.0000,4.0000,9.0000).

fit = glmnet(X,y,alpha=0,lambda=0.01)
less_reg = c(min(fit$beta), max(fit$beta), max(fit$beta) - min(fit$beta))

fit = glmnet(X,y,alpha=0,lambda=1)
more_reg = c(min(fit$beta), max(fit$beta), max(fit$beta) - min(fit$beta))

print(less_reg)
print(more_reg)


#Task 5
#
#Predict y from X using a LASSO regression with crossvalidation (glmnet default parameters for lambda and cv). 
#What is the maximum number of coefficients that can be dropped (set to zero) before LASSO looses fit performance 
#over linear regression? The loss of performance is defined as LASSO Mean-Squared Error confidence intervals 
#exceeding the Mean-Squared Error of the linear regression. Return a variable "ndrop" that contains the number 
#of coefficients that can be dropped.

lasso = cv.glmnet(X,y,alpha=1)
reg = cv.glmnet(X,y,alpha=0,lambda=exp(c(-10,-9)))

total = reg$nzero[1]
nonzero= min(lasso$nzero[lasso$cvlo <= mean(reg$cvm)])
ndrop = total-nonzero
ndrop=unname(ndrop)
print(ndrop)


#Task 6
#
#How much can we improve the model by adding polynomial features? To answer this question, extend the 25 
#features in X with second-order polynomials and interactions (X2 with 350 features). Consult the Tutorial 
#code section "Adding nonlinear predictors" for adding second-order polynomials. Then, run LASSO regressions 
#for predicting y from X (simple model) and y from X2 (polynomial model) using crossvalidation (default 
#parameters for lambda and cv). Return a variable "comparison" containing the minimum average Mean-Squared 
#Error for: the simple model, the polynomial model, and the polynomial - simple model difference using at least 
#four-point precision (i.e. comparison = c(0.5000,0.4200,0.0800)

#polynomial features
pairs <- t(combn(ncol(X), 2))
interactions <- sapply(seq(choose(ncol(X), 2)), function(i){
  X[,pairs[i,1]]*X[,pairs[i,2]]
})
X2 <- cbind(X, X^2, interactions)

#fit the models
fit <- cv.glmnet(X, y)
fit_poly <- cv.glmnet(X2, y)

#extract performance
performance = c(min(fit$cvm),min(fit_poly$cvm),min(fit$cvm)-min(fit_poly$cvm))

print(performance)

