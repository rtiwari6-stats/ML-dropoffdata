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
randomforestclassifier = function(ntree, maxnodes=NULL){
  require(randomForest)
  set.seed(120)  # Setting seed 
  classifier_RF = randomForest(x = xtrain, 
                               y = as.factor(ytrain$Target), 
                               xtest = xtest,
                               ytest = as.factor(ytest$Target),
                               importance = TRUE,
                               keep.forest = TRUE,
                               ntree = ntree,
                               maxnodes=maxnodes) 
  print(classifier_RF) 
  print(classifier_RF$confusion) #confusion matrix
  #print(classifier_RF$err.rate) #OOB error on training data
  plot(1:ntree, classifier_RF$err.rate[,1], main = "Number of trees vs OOB error in training", 
       xlab = "number of trees", ylab = "OOB error")

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
  roc_object = roc(ytest$Target, pred[,2])
  print(auc(roc_object)) 
  plot.roc(roc_object)
  
  varImpPlot(classifier_RF, sort = FALSE, main = "Variable Importance Plot")
  
  #test error using the optimal threshold
  test_error = mean(ytest$Target != ifelse(pred[,2] > 
                                             coords(roc_object, "best", ret = "threshold")[1,], 1, 0))
  print(test_error) 
}

#OOB estimate of  error rate: 19.34%
#Test set error rate: 21.36%
#auc: 0.8591
randomforestclassifier(500, 2) 

#OOB estimate of  error rate: 16.21%
#Test set error rate: 17.65%
#auc: 0.8682
randomforestclassifier(500, 4) 

#OOB estimate of  error rate: 15.19%
#Test set error rate: 16.38%
#auc: 0.8803
randomforestclassifier(500, 8) 

# OOB estimate of  error rate: 12.29%
# Test set error rate: 13.67%
#auc:  0.9101
#note that larger trees will take longer training time
randomforestclassifier(500, NULL)  


#OOB estimate of  error rate: 18.56%
#Test set error rate: 20.18%
#auc: 0.8583
randomforestclassifier(100, 2) 

#OOB estimate of  error rate: 15.97%
#Test set error rate: 17.65%
#auc: 0.8666
randomforestclassifier(100, 4) 

#OOB estimate of  error rate: 15.37%
# Test set error rate: 16.65%
#auc: 0.8711
randomforestclassifier(100, 8) 

#OOB estimate of  error rate: 12.23%
#Test set error rate: 12.94%
#auc: 0.9058
randomforestclassifier(100, NULL) 



