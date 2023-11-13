#Code for support vector machine

#set working directory appropriately. It's recommended to set it to where your folder 636-project lives.
#Check current working directory with getwd()
#Set current working directory with setwd()
#read two class datasets
train_data = read.csv("train_data_two_class.csv", header = TRUE)
test_data = read.csv("test_data_two_class.csv", header = TRUE)

#scaling is not necessary for random forests
train_data$Target = ifelse(train_data$Target == "Dropout", 1, 0)
test_data$Target = ifelse(test_data$Target == "Dropout", 1, 0)

require(e1071)
#cost parameter in svm(C)-defines the weight of how much samples inside the margin contribute to the overall error. 
#Consequently, with C you can adjust how hard or soft your large margin classification should be. 

#fit svm with cv, default k=10
set.seed (1000)
svm.out=tune.svm(x = train_data[,-37], 
                 y = as.factor(train_data[, 37]), 
                 cost = c(1,5), 
                 gamma = 10^(-3:3),
                 kernel = "radial")
summary(svm.out) #best cost is 5, error=0.1059329   
svm.out$best.parameters$cost
svm.out$best.parameters$gamma

#prediction using best model
best.svm = svm(as.factor(Target) ~., data=data.frame(train_data), 
               method = "C-classification", kernel = "radial", 
               cost = svm.out$best.parameters$cost, gamma=svm.out$best.parameters$gamma,
               probability=TRUE)
pred.svm = predict(best.svm, newdata = test_data[1:36])
mean(pred.svm != test_data[,37]) #error = 0.1321267

#compute roc auc
require(pROC)
roc_object = roc(test_data[,37], as.numeric(pred.svm))
print(auc(roc_object)) 
plot.roc(roc_object)