#please set working directory to where the data files are using setwd()

SPLIT_AND_STORE=FALSE # set only if you want to split again otherwise do not change and use the train/test data files directly

#Generate train/test splits and save them for 3 classes
if(SPLIT_AND_STORE){
  full_data = read.csv("full_data.csv", sep=";", header = TRUE)
  
  #split data
  require(caTools)
  set.seed(100) 
  #split 75/25
  sample = sample.split(full_data$Target, SplitRatio = .75)
  train = subset(full_data, sample == TRUE)
  test  = subset(full_data, sample == FALSE)
  
  write.csv(train, "train_data.csv")
  write.csv(test, "test_data.csv")
  
  train_data = read.csv("train_data.csv", header = TRUE)
  test_data = read.csv("test_data.csv", header = TRUE)
  train_data$Target = as.factor(train_data$Target)
  test_data$Target = as.factor(test_data$Target)
  
  #before we proceed we want to first convert this to a two-class problem and save it
  #we need to do it on both train/test
  #check class distributions for train
  
  train_data$Target = as.factor(ifelse(train_data$Target != "Dropout", "Not Dropout", "Dropout"))
  summary(train_data$Target)
  test_data$Target = as.factor(ifelse(test_data$Target != "Dropout", "Not Dropout", "Dropout"))
  summary(test_data$Target)
  
  write.csv(train_data, "train_data_two_class.csv")
  write.csv(test_data, "test_data_two_class.csv")
  
}

#read two class datasets
train_data = read.csv("train_data_two_class.csv", header = TRUE)
test_data = read.csv("test_data_two_class.csv", header = TRUE)

train_data$Target = as.factor(train_data$Target)
test_data$Target = as.factor(test_data$Target)

#check class distributions for train
barplot(summary(train_data$Target), main = "Class frequency for training data")
summary(train_data$Target)

#check class distributions for test
barplot(summary(test_data$Target), main = "Class frequency for test data")
summary(test_data$Target)
