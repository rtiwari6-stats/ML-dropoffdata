#Code for binary logistic regression

library(caret)
getwd()
setwd("C://Users//adria//OneDrive//STAT 636//636 Project//636-project")
source("C://Users//adria//OneDrive//STAT 636//636 Project//636-project//adrian_data_explore/DataUtilities.R")
datasets = read_data()
train_data = datasets$train_data
test_data = datasets$test_data
factors = datasets$factors

library(glmnet)
set.seed(100)
xtrain = train_data[ , !(names(train_data) %in% "Target")]
xtrain = scale(xtrain[ , !(names(xtrain) %in% factors)]) #scaling needed for lasso
xtest = test_data[ , !(names(test_data) %in% "Target")]
xtest = scale(xtest[ , !(names(xtest) %in% factors)])

ytrain = train_data["Target"]
ytrain$Target = ifelse(ytrain$Target == "Dropout", 1, 0) #needed for glmnet to work.
ytest = test_data["Target"]
ytest$Target = ifelse(ytest$Target == "Dropout", 1, 0) #needed for glmnet to work.


# Lasso model using lambda chosen by cross-validation
cv.out = cv.glmnet(as.matrix(xtrain), as.matrix(ytrain), alpha=1, family="binomial")
plot(cv.out)
bestlam = cv.out$lambda.min 
lambda1se = cv.out$lambda.1se

# coeficients of the final model
coef_cv=coef(cv.out, s = "lambda.min")
coef_cv

#Test error with bestlam
lasso.pred_base = predict(cv.out, s = bestlam, newx = as.matrix(xtest), type="response")
#roc auc
require(pROC)
roc_object = roc(ytest$Target, lasso.pred_base[,1])
auc(roc_object) #0.8858
plot(roc_object)

lasso.pred = ifelse(lasso.pred_base > 
                      coords(roc_object, "best", ret = "threshold")[1,], 1, 0)
error_glm = mean(lasso.pred != ytest$Target) #0.19819 -- not bad!
error_glm
##confusion matrix
table(lasso.pred, ytest$Target)


#also try using lambda1se
#One Standard Error Rule: The One Standard Error Rule can be used to compare models with 
#different numbers of parameters in order to select the most parsimonious model with
#low error.

# coeficients of the final model
coef_cv1se=coef(cv.out, s = "lambda.1se")
coef_cv1se

lasso.pred1se_base = predict(cv.out, s = lambda1se, newx = as.matrix(scale(xtest)), type="response")
require(pROC)
roc_object = roc(ytest$Target, lasso.pred1se_base[,1])
auc(roc_object) #0.8871
plot.roc(roc_object)

#Test error with lambda1se
lasso.pred1se = ifelse(lasso.pred1se_base > coords(roc_object, "best", ret = "threshold")[1,], 1, 0)
error_glm1se = mean(lasso.pred1se != ytest$Target) 
error_glm1se #0.1954751
##confusion matrix
table(lasso.pred1se, ytest$Target)

#Straight Logistic Regression
train_data_1=train_data
train_data_1$Target=as.factor(train_data_1$Target)
out1=glm(Target~.,family=binomial,data=train_data_1)
summary(out1)