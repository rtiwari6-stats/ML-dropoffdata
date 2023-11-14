train<-read.csv("C://Users//adria//OneDrive//STAT 636//636 Project//636-project//train_data_two_class.csv",header = TRUE)
head(train)
attach(train)
#Marital Status
table(Marital.status)
#1 - Single
#2 - Married
#3 - Widower
#4 - Divorced
#5 - Facto Union
#6 - Legally Separated
#Application Mode
table(Application.mode)
length(unique(Application.mode))
#18 factors for application.mode - examples are if someone is a transfer, over 23, etc...
table(Application.order)
#0 is first choice and up to 9 being last choice.  There appear to be nothing beyond 6 here.
table(Course)
#These are a sample of courses above?
table(Daytime.evening.attendance.)
#1=daytime 0=evening
table(Previous.qualification)
#Majority secondary education
