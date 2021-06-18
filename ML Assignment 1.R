# Assignment 1, Machine Learning
# Fergus Walsh
# Due 5th February, Epiphany Term, 2021

library(pls)
library(glmnet)
library(boot)
library(leaps)
library(MASS)
data("Boston")

set.seed(999)

# Introduction
sum(is.na(Boston))
names(Boston)
help(Boston)

# Model 1: Full LS
model1 <- glm(medv ~ ., data = Boston)
summary(model1)

# K-fold Cross Validation
cv.err.1 = rep(0,50)
for (i in 1:50){
  cv.err.1[i] = cv.glm(Boston, model1, K = 5)$delta[1]
}
mean(cv.err.1)

# Model 2: Best Subset
model2 = regsubsets(medv ~ ., Boston, nvmax = 13)
summary(model2)
results2 = summary(model2)
cbind(results2$rss, results2$rsq, results2$cp, results2$bic, results2$adjr2) 

# Figure 1
par(mfrow=c(1, 2))
plot(results2$rss, xlab = "Number of Predictors", ylab = "R.S.S.", type = "b", lwd = 2, pch = 4)
plot(results2$adjr2, xlab = "Number of Predictors", ylab = "R-square", type = "b", lwd = 2, pch = 4)

which.min(results2$cp)
which.min(results2$bic)
which.max(results2$adjr2)

# Figure 2
par(mfrow=c(1, 3))
plot(model2, scale = "bic", main = "B.I.C.", col = "black")
plot(model2, scale = "Cp", main = "Cp", col = "red")
plot(model2, scale = "adjr", main = "Adjusted R-Squared", col = "blue")

# Figure 3
par(mfrow=c(1,3))
plot(results2$bic, xlab = "Number of Predictors", ylab = "BIC", type = 'b', lwd = 2, main = "B.I.C.", pch = 4)
points(11, results2$bic[11], col = "red", cex = 2, pch = 8, lwd = 2)
plot(results2$cp, xlab = "Number of Predictors", ylab = "Cp", type = 'b', lwd = 2, main = "Cp", pch = 4)
points(11, results2$cp[11], col = "red", cex = 2, pch = 8, lwd = 2)
plot(results2$adjr2, xlab = "Number of Predictors", ylab = "Adjusted RSq", type = "b", lwd = 2, main = "Adjusted R-Squared", pch = 4)
points(11, results3$adjr2[11],  col = "red", cex = 2, pch = 8, lwd = 2)

coef(model2, 11)

# Predictive Performance, Model 2 chosen by Best Subset selection
model2b = glm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, data = Boston)
summary(model2b)
cv.err.2 = rep(0,50)
for (i in 1:50){
  cv.err.2[i] = cv.glm(Boston, model2b, K = 5)$delta[1]
}
mean(cv.err.2)

# Model 3: Ridge Regression
# Preparing data for glmnet() function
y = Boston$medv
x = model.matrix(medv ~., Boston)[,] # Note, if "[,-14]" then no coefficent for lstat in coef(model4). Inspecting x it seems to be okay

model3 = glmnet(x, y, alpha = 0)
model3$lambda
model3$beta[,1:3]
coef(model3)[,1:3]

# Selecting the optimum lambda value
model3.min = rep(0, 50)
model3.1se = rep(0, 50)

for (i in 1:50){
  model3.cv = cv.glmnet(x, y, alpha = 0)
  model3.min[i] = model3.cv$lambda.min
  model3.1se[i] = model3.cv$lambda.1se
}
mean(model3.min)
mean(model3.1se)
model3.cvm <- model3.cv$cvm
model3.cvm[which(model3.cv$lambda == model3.cv$lambda.min)]
model3.cvm[which(model3.cv$lambda == model3.cv$lambda.1se)]

round(cbind(
  coef(model3.cv, s = 'lambda.min'),
  coef(model3.cv, s = 'lambda.1se')),
  3)

# Figure 4
par(mfrow=c(1,2))
plot(model3.cv)
abline(v = log(mean(model3.min)), lty = 5, col = "red")
abline(v = log(mean(model3.1se)), lty = 5, col = "blue")
plot(model3, xvar = "lambda")
abline(v = log(mean(model3.min)), lty = 5, col = "red")
abline(v = log(mean(model3.1se)), lty = 5, col = "blue")

summary(model3)

# Predictive Performance
repetitions = 50
cor.1 = c()
cor.2 = c()
             
for(i in 1:repetitions){
  training.obs = sample(1:506,  253)
  y.train = Boston$medv[training.obs]
  x.train = model.matrix(medv~., Boston[training.obs, ])
  y.test = Boston$medv[-training.obs] 
  x.test = model.matrix(medv~., Boston[-training.obs, ])
  
  model3.train1 = cv.glmnet(x.train, y.train, alpha = 0)
  
  predict.1 = predict(model3.train1, x.test, s = 'lambda.min')
  predict.2 = predict(model3.train1, x.test, s = 'lambda.1se')

  cor.1[i] = cor(y.test, predict.1)
  cor.2[i] = cor(y.test, predict.2)
}

# Figure 6
dev.off()
boxplot(cor.1, cor.2, names = c('Model 3 Min. Lambda','Model 3 1 S.E. Lambda'), ylab = 'Test Correlation', col = 3)

mean(cor.1)
mean(cor.2)
range(cor.1)
range(cor.2)

mean(predict.1)
mean(predict.2)

# Shrinkage Methods, Lasso
model4 = glmnet(x, y, alpha = 1)
model4$lambda
model4$beta[,1:3]
coef(model4)[,1:3]

# Selecting the optimum lambda value
model4.min = c()
model4.1se = c()

for (i in 1:50){
  model4.cv = cv.glmnet(x, y, alpha = 1)
  model4.min[i] = model4.cv$lambda.min
  model4.1se[i] = model4.cv$lambda.1se
}
mean(model4.min)
mean(model4.1se)

model4.cvm <- model4.cv$cvm
model4.cvm[which(model4.cv$lambda == model4.cv$lambda.min)]
model4.cvm[which(model4.cv$lambda == model4.cv$lambda.1se)]

round(cbind(
  coef(model4.cv, s = 'lambda.min'),
  coef(model4.cv, s = 'lambda.1se')),
  3)

# Figure 5
par(mfrow=c(1,2))
plot(model4.cv)
abline(v = log(mean(model4.min)), lty = 5, col = "red")
abline(v = log(mean(model4.1se)), lty = 5, col = "blue")
plot(model5, xvar = "lambda")
abline(v = log(mean(model4.min)), lty = 5, col = "red")
abline(v = log(mean(model4.1se)), lty = 5, col = "blue")


# Predictive Performance
repetitions = 50
cor.3 = c()
cor.4 = c()

for(i in 1:repetitions){
  training.obs = sample(1:506,  253)
  y.train = Boston$medv[training.obs]
  x.train = model.matrix(medv~., Boston[training.obs, ])
  y.test = Boston$medv[-training.obs] 
  x.test = model.matrix(medv~., Boston[-training.obs, ])
  
  model5.train1 = cv.glmnet(x.train, y.train, alpha = 1)
  
  predict.3 = predict(model4.train1, x.test, s = 'lambda.min')
  predict.4 = predict(model4.train1, x.test, s = 'lambda.1se')
  
  cor.3[i] = cor(y.test, predict.3)
  cor.4[i] = cor(y.test, predict.4)
}

# Figure 7
dev.off()
boxplot(cor.3, cor.4, names = c('Model 4 Min. Lambda','Model 4 1 S.E. Lambda'), ylab = 'Test Correlation', col = 3)

mean(cor.3)
mean(cor.4)
range(cor.3)
range(cor.4)

mean(predict.3)
mean(predict.4)