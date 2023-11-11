#please set working directory to where the data files are using setwd()

SPLIT=TRUE # set only if you want to split again otherwise do not change and use the train/test data files directly

if(SPLIT){
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
}

train_data = read.csv("train_data.csv", header = TRUE)
test_data = read.csv("test_data.csv", header = TRUE)
train_data$Target = as.factor(train_data$Target)
test_data$Target = as.factor(test$Target)

#check class distributions for both
hist(as.integer(train$Target))
hist(as.integer(test$Target)) # don't look at this again, this is just to show how test data looks like in the report.

