
if(!require("lars")){
  install.packages("lars")
  library("lars")
}
data("diabetes")

X <- diabetes$x
(N <- nrow(X))
pairs(X, pch=20, gap=0, col=rgb(0,0,0,.2))

y <- diabetes$y
hist(y, col="gray")

reg <- lm(y ~ X)
summary(reg)

set.seed(12345) # the same combination i have on my luggage!
new_indices <- sample(nrow(X))
y <- y[new_indices]
X <- X[new_indices,]

breaks <- round(quantile(seq(N), probs=seq(0, 1, .1)))
groups <- cut(seq(N), breaks=breaks, include.lowest=TRUE)
indices <- split(seq(N), groups)
str(indices)

# empty vector to hold results
RMSE <- numeric(10)
# do this stuff for each fold
for(i in 1:10){
  # regress y (leaving out ith fold) on X (leaving out ith fold)
  mod <- lm(y[-indices[[i]]] ~ X[-indices[[i]],])
  # compute and save RMSE
  RMSE[i] <- sqrt(mean(mod$residuals^2)) 
}

mean(RMSE)
sd(RMSE)

if(!require("caret")){
  install.packages("caret")
  library("caret")
}

#ctrl <- trainControl(method="cv", number=10)
ctrl <- trainControl(method="repeatedcv", number=10, repeats=10)
model <- train(y=y, x=X, trControl=ctrl, method="lm")
model

if(!require("glmnet")){
  install.packages("glmnet")
  library("glmnet")
}

lasso <- glmnet(x=X, y=y)
CV <- cv.glmnet(x=X, y=y)

reg_CV <- cv.glmnet(x=X, y=y, lambda=exp(-100:-99))

# adjust the plot layout to make a two-panel plot
layout(rbind(1,2))
par(mar=c(3,3,2,1)+.1)

# coefficient path plot
plot(lasso, xvar="lambda", xlim=c(-4,4), mgp=2:0)

# CV error plot
plot(CV, xlim=c(-4,4), mgp=2:0)

# add the baseline performance of the ordinary regression model
with(reg_CV, polygon(x=c(-10,10,10,-10,-10),
                     y=c(cvup[1],cvup[1],cvlo[1],cvlo[1],cvup[1]),
                     border=NA, col=rgb(0,0,0,.25)))
abline(h=reg_CV$cvm[1], lty=2, lwd=2)
legend("topleft", lty=2, lwd=2, legend="Ordinary regression MSE", bty="n")

pairs <- t(combn(ncol(X), 2))
interactions <- sapply(seq(choose(ncol(X), 2)), function(i){
  X[,pairs[i,1]]*X[,pairs[i,2]]
})
X2 <- cbind(X, X^2, interactions)

ncol(X2)

# fit the models
lasso2 <- glmnet(x=X2, y=y)
CV2 <- cv.glmnet(x=X2, y=y)
reg_CV2 <- cv.glmnet(x=X2, y=y, lambda=exp(c(-10,-9)))

# adjust the plot layout to make a two-panel plot
layout(rbind(1,2))
par(mar=c(3,3,2,1)+.1)

# coefficient path plot
plot(lasso2, xvar="lambda", xlim=c(-4,4), mgp=2:0)

# CV error plot
plot(CV2, xlim=c(-4,4), mgp=2:0)

# add the baseline performance of the ordinary regression model
with(reg_CV2, polygon(x=c(-10,10,10,-10,-10),
                      y=c(cvup[1],cvup[1],cvlo[1],cvlo[1],cvup[1]),
                     border=NA, col=rgb(0,0,0,.25)))
abline(h=reg_CV2$cvm[1], lty=2, lwd=2)
legend("topleft", lty=2, lwd=2, legend="Ordinary regression MSE", bty="n")

summary(lm(y ~ X2))$sigma

sqrt(reg_CV2$cvm[1])

with(CV2, sqrt(cvm[lambda == lambda.min]))

# empty vector to hold results
RMSE <- numeric(10)
# do this stuff for each fold
for(i in 1:10){
  # fit LASSO to the 9 training folds
  mod <- cv.glmnet(x=X2[-indices[[i]],], y=y[-indices[[i]]])
  # get predicted values for test fold using "optimal" lambda from training
  predictions <- mean(y[indices[[i]]]) + X2[indices[[i]],] %*%
    mod$glmnet.fit$beta[,which.min(mod$cvm)]
  # compute and save RMSE
  errors <- y[indices[[i]]] - predictions
  RMSE[i] <- sqrt(mean(errors^2))
}
mean(RMSE)
