#reading input files
test = read.csv("test.csv", header = TRUE)
train = read.csv("train.csv", header = TRUE)


#adding survived column to test to combine train and test
test.survived = data.frame(survived = rep("None",nrow(test)),test[,])
data.combined = rbind(train,test.survived)

#checking data types of variables
str(data.combined)

#changing the data types of variables
data.combined$survived = as.factor(data.combined$survived)
data.combined$pclass = as.factor(data.combined$pclass)
data.combined$name = as.character(data.combined$name)

#checking gross survival rates
table(data.combined$survived)

#checking no. of members in each class
table(data.combined$pclass)

#load package ggplot2 for visualizations
library(ggplot2)

#Analysis of number of survivors by passenger class
train$survived = as.factor(train$survived)
train$pclass = as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = survived)) + geom_bar() + xlab("Passenger Class") + ylab("Total Count") + labs(fill = "survived")

#Analysis on names
nrow(data.combined)

#Check if all names are unique
length(unique(data.combined$name))

#Two duplicate names, finding the duplicate names and storing them in a vector
dup.names = data.combined[which(duplicated(data.combined$name)),"name"]

#getting data for duplicate names to check if they are the same persons
data.combined[which(data.combined$name %in% dup.names),]

#finding out if title(Mr., Miss has any effect)
#checking all records with title Miss.
#we need the package stringr which is already installed
library(stringr)
miss = data.combined[which(str_detect(data.combined$name,"Miss.")),]
nrow(miss)
head(miss)

#Let's explore relationship between survived and pclass and title combined by adding a title column to the combined dataset
#Creating a function for extracting titles from names

ExtractTitle = function(name)
 {
   if(str_detect(name,"Miss."))
     return("Miss.")
   else if(str_detect(name,"Mrs."))
     return("Mrs.")
   else if(str_detect(name,"Mr."))
     return("Mr.")
   else if(str_detect(name,"Master."))
     return("Master.")
   else
     return("Other")
 }

#creating a vector of titles for each record
titles = NULL
for (i in 1:nrow(data.combined)) {
  titles = c(titles,ExtractTitle(data.combined[i,"name"]))
}

head(titles)

#adding titles column to data.combined
data.combined = cbind(data.combined,titles)

# Since we only have survived lables for the train set, we use only that part from data.combined
nrow(train)
#using only first 891 rows as given by nrow(train)
ggplot(data.combined[1:891,],aes(x = titles,fill = survived))+
  facet_wrap(~pclass)+
  ggtitle("Passenger Class")+
  xlab("Title")+
  ylab("Total Count")+
  geom_bar()

#analysis by sex and passenger class
table(data.combined$sex)
ggplot(train,aes(x=sex,fill = survived))+
geom_bar()+
facet_wrap(~pclass)+
ylab("Total count")

summary(data.combined$age)
summary(train$age)

#analysis by age, sex and passenger class
ggplot(train,aes(x=age,fill = survived))+
  geom_histogram(binwidth = 10)+ #same as geom_bar but geom_bar() no longer uses binwidth parameter
  facet_wrap(~pclass+sex)+
  ylab("Total Count")

#analysis by age,title and pclass
ggplot(data.combined[1:891,],aes(x = age,fill = survived))+
geom_histogram(binwidth = 10)+
facet_wrap(~pclass+titles)

#We know a child is more likely to survive
#Bifurcating on child to go one step deeper into male child and female child survival rate
#For this, first let's verify if Master and Miss. can be regarded as male and female children respectively

male_children = data.combined[which(data.combined$titles=="Master."),]
summary(male_children$age)

#As we can see, age ranges from 0.33 to 14.5 indicating a child
#Similarly let's check for female kids

female_children = data.combined[which(data.combined$titles=="Miss."),]
summary(female_children$age)

#Here, the range is 0.17 to 63, however median and mean are small. Also, value of 3rd quantile is 30.
#Hence, there is high chance that the max value 63 can be an outlier
#Checking the plot to see if it is an outlier

ggplot(data.combined[1:891,],aes(x = age, fill = survived))+
  geom_histogram(binwidth = 5)

hist(data.combined[which(data.combined$titles=="Miss."),]$age)

#We can see that there are many females with age>25, hence cannot say that females with title miss are kids/young girls

#Analysis on sibsp, sex and passenger class
ggplot(data.combined[1:891,],aes(x = as.factor(sibsp),fill = survived))+
  geom_bar()+
  facet_wrap(~pclass+titles)
#As seen, as no. of siblings/spouse increases, survival rate decreases. Also master/miss with less siblings are more likely to survive

#Analysis on parch, title and passenger class
ggplot(data.combined[1:891,],aes(x = as.factor(parch),fill = survived))+
  geom_bar()+
  facet_wrap(~pclass+titles)
#As can be seen, in case of Mr. and Mrs., survival rate decreases with increase in the value of parch, indicating they are ensuring safety of their children

#Calculating family size
temp.sibsp = c(data.combined$sibsp)
temp.parch = c(data.combined$parch)
data.combined$family_size = (temp.sibsp + temp.parch + 1)

#Analysis by family size
ggplot(data.combined[1:891,], aes(x = family_size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + titles) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Analysis by ticket

str(data.combined$ticket)
data.combined$ticket = as.character(data.combined$ticket)
data.combined[,"ticket"]

#Extracting first character of each ticket#
ticket.first.ch = ifelse(data.combined$ticket==""," ",substr(data.combined$ticket,1,1))
unique(ticket.first.ch)
str(ticket.first.ch)
#converting ticket.first.ch to factor for the purpose of analysis
data.combined$ticket.first.ch = as.character(ticket.first.ch)
ggplot(data.combined[1:891,],aes(x=ticket.first.ch,fill = survived))+
  geom_bar()
#no significant pattern
#plotting ticket.first.ch against pclass
ggplot(data.combined[1:891,],aes(x=ticket.first.ch,fill = survived))+
  geom_bar()+
  facet_wrap(~pclass+titles)
#again no significant pattern,hence we don't consider variable ticket for analysis

#Analysis based on fare
str(data.combined$fare)
summary(data.combined$fare)
ggplot(data.combined[1:891,],aes(x=fare,fill=survived))+
  geom_histogram(binwidth = 10)+
  xlab("Fare")+
  ylab("Total Count")+
  ggtitle("Analysis by fare")
#no pattern

#Analysis by fare and pclass
ggplot(data.combined[1:891,],aes(x=fare,fill=survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~pclass)+
  xlab("Fare")+
  ylab("Total Count")+
  ggtitle("Analysis by fare")
#no new pattern

#Analysis by fare and pclass and title
ggplot(data.combined[1:891,],aes(x=fare,fill=survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~pclass+titles) +
  xlab("Fare") +
  ylab("Total Count") +
  ggtitle("Analysis by fare")+
  ylim(0,50)
#no trend can be seen

#Analysis by cabin variable
str(data.combined$cabin)
data.combined$cabin = as.character(data.combined$cabin)
data.combined$cabin[1:100]
#Extracting first letter of each cabin
cabin_first_ch = ifelse(data.combined$cabin == "","Blank",substr(data.combined$cabin,1,1))

str(cabin_first_ch)
cabin_first_ch = as.character(cabin_first_ch)
data.combined$cabin_first_ch = cabin_first_ch
ggplot(data.combined[1:891,],aes(x=cabin_first_ch, fill = survived))+
  geom_bar()+
  xlab("Cabin")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ggtitle("Survived by cabin type")

#Analysis by cabin, pclass and title
ggplot(data.combined[1:891,],aes(x=cabin_first_ch, fill = survived))+
  geom_bar()+
  facet_wrap(~pclass+titles)+
  xlab("Cabin")+
  ylab("Total Count")+
  labs(fill = "Survived")+
  ggtitle("Survived by cabin type, passenger class and title")

#there were some passengers wity multiple cabins, analyzing the effect of the same
cabin_multiple = ifelse(str_detect(data.combined$cabin," "),"Y","N")
data.combined$cabin_multiple = cabin_multiple
str(data.combined$cabin_multiple)
data.combined$cabin_multiple = as.factor(data.combined$cabin_multiple)
ggplot(data.combined[1:891,],aes(cabin_multiple,fill= survived))+
  geom_bar()

#multiple cabins by pclass and title
ggplot(data.combined[1:891,],aes(cabin_multiple,fill= survived))+
  geom_bar()+
  facet_wrap(~pclass+titles)+
  labs(fill = "Survived",x = "Multiple Cabins", y = "Total Count")+
  ggtitle("Analysis by multiple cabins, pclass and title")

#Analysis by variable embarked
str(data.combined$embarked)
levels(data.combined$embarked)
ggplot(data.combined[1:891,],aes(x=embarked,fill = survived))+
  facet_wrap(~pclass)+
  geom_bar()
#no trend by embarked as well

#Exploratory modeling
library(randomForest)
rf_train_1 = data.combined[1:891,c("pclass","titles")]
rf_train_1

set.seed(1234)
rf_labels = as.factor(train$survived)
rf_1 = randomForest(x = rf_train_1, y = rf_labels,importance = TRUE)
rf_1
varImpPlot(rf_1)

rf_train_2 = data.combined[1:891,c("pclass","titles","sibsp")]
set.seed(1234)
rf_2 = randomForest(x = rf_train_2, y = rf_labels,importance = TRUE)
rf_2

rf_train_3 = data.combined[1:891,c("pclass","titles","parch")]
set.seed(1234)
rf_3 = randomForest(x = rf_train_3, y = rf_labels,importance = TRUE)
rf_3

rf_train_4 = data.combined[1:891,c("pclass","titles","sibsp","parch")]
set.seed(1234)
rf_4 = randomForest(x = rf_train_4, y = rf_labels, importance = TRUE)
rf_4

rf_train_5 = data.combined[1:891,c("pclass","titles","family_size")]
set.seed(1234)
rf_5 = randomForest(x = rf_train_5, y = rf_labels,importance = TRUE)
rf_5

rf_train_6 = data.combined[1:891,c("pclass","titles","family_size","sibsp")]
set.seed(1234)
rf_6 = randomForest(x = rf_train_5, y = rf_labels,importance = TRUE)
rf_6

rf_train_7 = data.combined[1:891,c("pclass","titles","family_size","parch")]
set.seed(1234)
rf_7 = randomForest(x = rf_train_5, y = rf_labels,importance = TRUE)
rf_7

rf_train_8 = data.combined[1:891,c("pclass","titles","family_size","parch","sibsp")]
set.seed(1234)
rf_8 = randomForest(x = rf_train_5, y = rf_labels,importance = TRUE)
rf_8

#Thus rf_4 is the best model with 81.48% accuracy

test_sub = data.combined[892:1309,c("pclass","titles","sibsp","parch")]
#making prediction on test dataset
rf_4_1 = predict(rf_4,test_sub)
table(rf_4_1)

final_output = data.frame(PassengerID = rep(892:1309),Survived = rf_4_1)
write.csv(final_output, file = "Survival_Predictions_V1.csv", row.names = FALSE)

library(caret)
library(doSNOW)

set.seed(1234)
cv_10_folds = createMultiFolds(rf_labels, k=10, times = 10)

#check stratification
table(rf_labels)
342/549

table(rf_labels[cv_10_folds[[33]]])
308/494

ctrl_1 = trainControl(method = "repeatedcv",number = 10, repeats = 10, index = cv_10_folds)
cl = makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
rf_4_cv_1 = train(x = rf_train_4,y = rf_labels, method = "rf",tuneLength = 3, ntree = 1000, trControl = ctrl_1)

stopCluster(cl)

rf_4_cv_1

# The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
set.seed(1234)
cv_5_folds <- createMultiFolds(rf_labels, k = 5, times = 10)

ctrl_2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,index = cv_5_folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
rf_4_cv_2 <- train(x = rf_train_4, y = rf_labels, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl_2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf_4_cv_2


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(1234)
cv_3_folds <- createMultiFolds(rf_labels, k = 3, times = 10)

ctrl_3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv_3_folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
rf_5_cv_3 <- train(x = rf_train_5, y = rf_labels, method = "rf", tuneLength = 3, ntree = 64, trControl = ctrl_3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf_5_cv_3

