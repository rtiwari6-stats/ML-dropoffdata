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
  
  write.csv(train, "train_data.csv", row.names = FALSE)
  write.csv(test, "test_data.csv", row.names = FALSE)
  
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
  
  write.csv(train_data, "train_data_two_class.csv", row.names = FALSE)
  write.csv(test_data, "test_data_two_class.csv", row.names = FALSE)
  
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

library(dplyr)
glimpse(train_data) #tells us which colmuns can be factors
#marital status, Daytime.evening.attendance., Displaced, Educational.special.needs, 
# Debtor, Tuition.fees.up.to.date, Gender, Scholarship.holder, International

#look at categorical variables
table(train_data$Marital.status)
table(train_data$Daytime.evening.attendance.)
table(train_data$Displaced)
table(train_data$Educational.special.needs)
table(train_data$Debtor)
table(train_data$Tuition.fees.up.to.date)
table(train_data$Gender)
table(train_data$Scholarship.holder)
table(train_data$International)

#two-way dependene
#let us be smart, write a function!
two_way_dependence_plot = function(var, varname){
  var_target = table(train_data$Target, var)
  barplot(var_target, main=paste(varname, " and dropout"), legend.text = TRUE)
}

two_way_dependence_plot(train_data$Marital.status, "Marital Status") #no
two_way_dependence_plot(train_data$Daytime.evening.attendance., 
                      "Daytime.evening.attendance") #no
two_way_dependence_plot(train_data$Displaced, "Displaced") #no
two_way_dependence_plot(train_data$Educational.special.needs, "Educational.special.needs") #no
two_way_dependence_plot(train_data$Debtor, "Debtor") #seems like debtor has higher dropout
two_way_dependence_plot(train_data$Tuition.fees.up.to.date, 
                      "Tuition.fees.up.to.date") #higher when fees not up to date
two_way_dependence_plot(train_data$Gender, "Gender") #no
two_way_dependence_plot(train_data$Scholarship.holder, "Scholarship.holder") #no scholarship higer dropout
two_way_dependence_plot(train_data$International, "International") #no

#boxplots for continuous
boxplot(train_data$GDP~train_data$Target, ylab = "GDP", xlab = "Target") #lower gdp for dropout
boxplot(train_data$Inflation.rate~train_data$Target, 
        ylab = "Inflation.rate", xlab = "Target")# nothing stands out?
boxplot(train_data$Unemployment.rate~train_data$Target, ylab = "Unemployment.rate",
        xlab = "Target") #hmmm, nothing special?
boxplot(train_data$GDP~train_data$Scholarship.holder, ylab = "GDP", xlab = "Scholarship")
boxplot(train_data$Inflation.rate~train_data$Scholarship.holder, ylab = "Inflation.Rate", xlab = "Scholarship")
boxplot(train_data$Unemployment.rate~train_data$Scholarship.holder, 
        ylab = "Unemployment.Rate", xlab = "Scholarship")

#too many variables so we stop doing variable by variable
#let's do automated
require(DataExplorer)
introduce(train_data) #no missing values
create_report(train_data, config = configure_report(add_plot_density = TRUE)) #report.html in current working directory



