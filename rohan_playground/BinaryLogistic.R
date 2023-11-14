#Code for binary logistic regression

#set working directory appropriately. It's recommended to set it to where your folder 636-project lives.
#Check current working directory with getwd()
#Set current working directory with setwd()
#read two class datasets
source("rohan_playground\\DataUtilities.R")
datasets = read_data()
train_data = datasets$train_data
test_data = datasets$test_data
factors = datasets$factors

library(glmnet)

xtrain = train_data[ , !(names(train_data) %in% "Target")]
xtrain = scale(xtrain[ , !(names(xtrain) %in% factors)]) #scaling needed for lasso
xtest = test_data[ , !(names(test_data) %in% "Target")]
xtest = scale(xtest[ , !(names(xtest) %in% factors)])

ytrain = train_data["Target"]
ytrain$Target = ifelse(ytrain$Target == "Dropout", 1, 0) #needed for glmnet to work.
ytest = test_data["Target"]
ytest$Target = ifelse(ytest$Target == "Dropout", 1, 0) #needed for glmnet to work.


# Lasso model using lambda chosen by cross-validation
cv.out = cv.glmnet(as.matrix(xtrain), as.matrix(ytrain), alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min 
lambda1se = cv.out$lambda.1se

# coeficients of the final model
coef_cv=coef(cv.out, s = "lambda.min")
coef_cv

#Test error with bestlam
lasso.pred = predict(cv.out, s = bestlam, newx = as.matrix(xtest), type="response")

#apply a basic threshold of 0.5
lasso.pred = ifelse(lasso.pred > 0.5, 1, 0)
error_glm = mean(lasso.pred != ytest$Target) #0.160181 -- not bad!
error_glm
##confusion matrix
table(lasso.pred, ytest$Target)
#roc auc
require(pROC)
roc_object = roc(ytest$Target, lasso.pred)
auc(roc_object) #0.7819
plot(roc_object)


#also try using lambda1se
#One Standard Error Rule: The One Standard Error Rule can be used to compare models with 
#different numbers of parameters in order to select the most parsimonious model with
#low error.

# coeficients of the final model
coef_cv1se=coef(cv.out, s = "lambda.1se")
coef_cv1se

#Test error with lambda1se
lasso.pred1se = predict(cv.out, s = lambda1se, newx = as.matrix(scale(xtest)), type="response")
lasso.pred1se = ifelse(lasso.pred1se > 0.5, 1, 0)
#apply a basic threshold of 0.5
error_glm1se = mean(lasso.pred1se != ytest$Target) 
error_glm1se #almost the same but model is more parsimonious
##confusion matrix
table(lasso.pred1se, ytest$Target)

require(pROC)
roc_object = roc(ytest$Target, lasso.pred1se)
auc(roc_object) #0.9072
plot.roc(roc_object)