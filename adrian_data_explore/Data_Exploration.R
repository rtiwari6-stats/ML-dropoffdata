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
#Note: Majority secondary education
table(Previous.qualification..grade.)
#Not sure how you interpret grades in this context
table(Nacionality)
length(unique(Nacionality))
#19 different nationalities represented
table(Mother.s.qualification)
length(unique(Mother.s.qualification))
#Mother's education level
#28 different types of education levels and/or types of specializations
table(Father.s.qualification)
length(unique(Father.s.qualification))
#30 different types of education levels
table(Mother.s.occupation)
length(unique(Mother.s.occupation))
#30 different types of occupation levels represented
table(Father.s.occupation)
length(unique(Father.s.occupation))
#43 different types of occupation levels represented
table(Admission.grade)
#Not sure what "Admission Grade" means here.  There's clearly a scale for Portugal of which I am unfamiliar.
table(Displaced)
#The above, Displaced, is 0 if no, 1 if yes - what is displaced?
table(Educational.special.needs)
#0 if no, 1 if yes
table(Debtor)
#0 if no, 1 if yes
table(Gender)
#0 if female, 1 if male
table(Scholarship.holder)
#0 if no, 1 if yes
table(Age.at.enrollment)
range(Age.at.enrollment)
#age range from 17 to 61
hist(Age.at.enrollment,xlim=c(15,65),ylim = c(0,2000))
print(hist(Age.at.enrollment))
#ages are clustered where expected
table(International)
#0 if no, 1 if yes
table(Unemployment.rate)
range(Unemployment.rate)
#These are not low unemployment rates - 7.6 to 16.2 - maybe avg for European country
table(Inflation.rate)
#Inflation as expected
table(GDP)
#The models are built on data of undergraduate students from a Polytechnic University in Portugal, enrolled between 2009 and 2017
#Not surprising to see poor GDP numbers coming out of 2008

install.packages('DataExplorer')
library(DataExplorer)
introduce(train)
plot_str(train)
plot_intro(train)
plot_bar(train)
plot_histogram(train)
plot_qq(train)

boxplot(GDP~Target, ylab = "GDP", xlab = "Target")
