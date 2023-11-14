#Code for random forest classifier

#set working directory appropriately. It's recommended to set it to where your folder 636-project lives.
#Check current working directory with getwd()
#Set current working directory with setwd()
#read two class datasets
source("rohan_playground\\DataUtilities.R")
datasets = read_data()
train_data = datasets$train_data
test_data = datasets$test_data
factors = datasets$factors

#scaling is not necessary for random forests
xtrain = train_data[ , !(names(train_data) %in% "Target")]
xtest = test_data[ , !(names(test_data) %in% "Target")]
ytrain = train_data["Target"]
ytrain$Target = ifelse(ytrain$Target == "Dropout", 1, 0) 
ytest = test_data["Target"]
ytest$Target = ifelse(ytest$Target == "Dropout", 1, 0) 

#this does bagging for OOB error calculation so technically no need for cv.
randomforestclassifier = function(ntree){
  require(randomForest)
  set.seed(120)  # Setting seed 
  classifier_RF = randomForest(x = xtrain, 
                               y = as.factor(ytrain$Target), 
                               xtest = xtest,
                               ytest = as.factor(ytest$Target),
                               importance = TRUE,
                               keep.forest = TRUE,
                               ntree = ntree) 
  print(classifier_RF) 
  print(classifier_RF$confusion) #confusion matrix
  #print(classifier_RF$err.rate) #OOB error on training data
  plot(1:ntree, classifier_RF$err.rate[,1], main = "Number of trees vs OOB error in training", 
       xlab = "number of trees", ylab = "OOB error")
  #print(classifier_RF$importance) #variable importance
  
  #look at testset metrics
  #print(classifier_RF$test$err.rate) #in-place error on testing data
  plot(1:ntree, classifier_RF$test$err.rate[,1], main = "Number of trees vs error rate in testing", 
       xlab = "number of trees", ylab = "error rate")
  print(classifier_RF$test$confusion)
  
  #compute roc auc
  require(pROC)
  xtest = rbind(xtrain[1, ] , xtest)
  xtest = xtest[-1,]
  pred=predict(classifier_RF, newdata = xtest, type="prob")
  pred = ifelse(pred[,2] > 0.5, 1, 0)
  roc_object = roc(ytest$Target, pred)
  print(auc(roc_object)) 
  plot.roc(roc_object)
  
  varImpPlot(classifier_RF, sort = FALSE, main = "Variable Importance Plot")
}

# 500 trees. #Test set error rate: 13.21%.  OOB estimate of  error rate: 11.96%. 
#auc=0.8292
#No. of variables tried at each split: 6
randomforestclassifier(500) 
#Test set error rate: 13.67%.OOB estimate of  error rate: 12.74%. auc: 0.8207
randomforestclassifier(100)

