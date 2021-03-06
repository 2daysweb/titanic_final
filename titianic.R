#Install and load "caret" package for ML-algorithms to be used

install.packages("caret")

#Install and load ggplot2

install.packages("tidyverse")

library(ggplot2)

?geombarplot

#Load "caret"

library(caret)

#create new dataframes with intuitive variable names. I like having number of observations appended.

train_891 <- read.csv("train.csv", header=TRUE)

test_418 <- read.csv("test.csv", header=TRUE)

survival_id <- read.csv("gender_submission.csv", header=TRUE)

#Exploratory anlaysis begins here

#Looking for (1) variations within variables and (2) what covariations occur between variables

#Begin with summary for quick overview

#Clarification of few key variables:

#Pclass is a proxy for socioeconomic class; 1st corresponds to highest, 2nd to middle, 3rd to lowest

#Sibsp includes siblings and spouses. Siblings extends to step-siblings. Spouses does not extend to mistresses or fiances.

#Parch includes parent and child. Parent is mother or father. Child is son, daughter, including steps. Parch value = 0 if travelled with Nanny

#Embarked is port embarked from

#Survival is 1 if survived, 0 if dead

#Age is fractional if < 1 yr, if estimated it's in form of 

#What variables do I suppose have greatest influence on survival?

#Hypothesis (in order): Pclass, Age, Sex, Sibsplibrary('ggplot2')


#Quick look at the data

str(train_891)

#Quick histogram of passenger class breakdown. Clearly, way more 3rd class than 1st & 2nd. Interestingly, more 1st than 2nd.

hist(train_891$Pclass, main="Histogram of Passenger Classes", xlab="Class (1st, 2nd, 3rd)", border="blue", col="green")

with(train_891, table(Sex))

#Histogram for overview of passenger ages. 

ggplot(train_891, aes(x = Age)) + geom_histogram(binwidth=5)

#Histogram of passgenger ages vs #Install and load "caret" package for ML-algorithms to be used

install.packages("caret")

#Install and load ggplot2

install.packages("tidyverse")

library(ggplot2)

#Load "caret"

library(caret)

#create new dataframes with intuitive variable names. I like having number of observations appended.

train_891 <- read.csv("train.csv", header=TRUE)

test_418 <- read.csv("test.csv", header=TRUE)

survival_id <- read.csv("gender_submission.csv", header=TRUE)

#Exploratory anlaysis begins here

#Looking for (1) variations within variables and (2) what covariations occur between variables

#Begin with summary for quick overview

#Clarification of few key variables:

#Pclass is a proxy for socioeconomic class; 1st corresponds to highest, 2nd to middle, 3rd to lowest

#Sibsp includes siblings and spouses. Siblings extends to step-siblings. Spouses does not extend to mistresses or fiances.

#Parch includes parent and child. Parent is mother or father. Child is son, daughter, including steps. Parch value = 0 if travelled with Nanny

#Embarked is port embarked from

#Survival is 1 if survived, 0 if dead

#Age is fractional if < 1 yr, if estimated it's in form of 

#What variables do I suppose have greatest influence on survival?

#Hypothesis (in order): Pclass, Age, Sex, Sibsplibrary('ggplot2')


#Quick look at the data

str(train_891)

hist(train_891$Pclass, main="Histogram of Passenger Classes", xlab="Class (1st, 2nd, 3rd)", border="blue", col="green")

with(train_891, table(Sex))

#Histogram for overview of passenger ages

ggplot(train_891, aes(x = Age)) + geom_histogram(binwidth=5)

#Histogram of Age vs Survived

ggplot(train_891, aes(x = Age, fill = factor(Survived))) + geom_histogram(bins=50) +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") +
  ggtitle("Age Vs Survived")


#Bar Graph of Passenger Class vs. Survived

ggplot(train_891, aes(x=Pclass, fill=factor(Survived))) + geom_bar(stat = "count") +
xlab("Passenger Class") +
  scale_fill_discrete(name = "Survived") +
  ggtitle("Pclass Vs Survived")


#Clearly, class has a significant affect on Survival. 1st class has smallest (death/total) proportion, ascending to 3rd class.


#Another way I constructed Passenger Class vs. Survived (creating a new data frame with just pclass, survived and renaming to add Count)
dat <- data.frame(table(train_891$Pclass,train_891$Survived))
names(dat) <- c("Pclass","Survived","Count")
ggplot(dat, aes(x=Pclass, y=Count, fill=Survived)) + geom_bar(stat="identity") + xlab("Passenger Class") 


#Let's break it down between Male and Female passengers

dat <- data.frame(table(train_891$Pclass,train_891$Survived, train_891$Sex))
names(dat) <- c("Pclass","Survived", "Sex", "Count")
ggplot(dat, aes(x=Pclass, y=Count, fill=Survived)) + geom_bar(stat="identity") + xlab("Passenger Class") + facet_grid(Sex ~ .) 

#Clearly, first class female first and second class passengers have remarkable survival rates

#Let's measure effect of Sibsp on Survival, first on all, then males & fem

dat <- data.frame(table(train_891$SibSp,train_891$Survived))
names(dat) <- c("SibSp","Survived","Count")
ggplot(dat, aes(x=SibSp, y=Count, fill=Survived)) + geom_bar(stat="identity") + xlab("SibSp") 

#Appears that 0 SibSp leads to <33% chance survival (below av), 1 leads to approx 50% survival (above av), 2 similar but few data points, seems large familes died together (did they wait? indicates poverty level?)

dat <- data.frame(table(train_891$SibSp, train_891$Pclass, train_891$Survived))
names(dat) <- c("SibSp", "Pclass", "Survived","Count")
ggplot(dat, aes(x=Pclass, y=Count, fill=SibSp)) + geom_bar(stat="identity") + xlab("Passenger Class") 

#Naturally, 1st class passengers had 1 or 2 Siblings/Spouses, which makes previous graph make some sense

dat <- data.frame(table(train_891$SibSp, train_891$Pclass, train_891$Survived, train_891$Age, train_891$Fare))
names(dat) <- c("SibSp", "Pclass", "Survived", "Age", "Fare", "Sex", "Count")
ggplot(dat, aes(x=Age, y=Pclass, fill=Survived)) + geom_dotplot(dotsize = 0.4) + xlab("Age") + facet_grid(Sex ~ .) 


ggplot(train_891, aes(x = Age, fill = factor(Survived))) + geom_histogram(bins=50) +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") +
  ggtitle("Age Vs Survived")




ggplot(train_891, aes(x = Age, fill = factor(Survived))) + geom_histogram(bins=50) +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") +
  ggtitle("Age Vs Survived")

ggplot(train_891, aes(x = Age, fill = factor(Survived))) + geom_histogram(bins=50) +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") +
  ggtitle("Age Vs Survived")

ggplot(train_891, aes(x=Plcass, countfill=factor(Survived))) + geom_barplot(stat = "count")
xlab("PClass") +
scale_fill_discrete(name = "Survived") +
ggtitle("Pclass Vs Survived")

  
#Clearly, class has a significant affect on Survival. 1st class has smallest proportion, ascending to 3rd class.

dat <- data.frame(table(train_891$Pclass,train_891$Survived))
names(dat) <- c("Pclass","Survived","Count")
ggplot(dat, aes(x=Pclass, y=Count, fill=Survived)) + geom_bar(stat="identity") + xlab("Passenger Class") 


#Let's break it down between Male and Female passengers

dat <- data.frame(table(train_891$Pclass,train_891$Survived, train_891$Sex))
names(dat) <- c("Pclass","Survived", "Sex", "Count")
ggplot(dat, aes(x=Pclass, y=Count, fill=Survived)) + geom_bar(stat="identity") + xlab("Passenger Class") + facet_grid(Sex ~ .) 

#Clearly, first class female first and second class passengers have remarkable survival rates

#Let's measure effect of Sibsp on Survival, first on all, then males & fem

dat <- data.frame(table(train_891$SibSp,train_891$Survived))
names(dat) <- c("SibSp","Survived","Count")
ggplot(dat, aes(x=SibSp, y=Count, fill=Survived)) + geom_bar(stat="identity") + xlab("SibSp") 

#Appears that 0 SibSp leads to <33% chance survival (below av), 1 leads to approx 50% survival (above av), 2 similar but few data points, seems large familes died together (did they wait? indicates poverty level?)

dat <- data.frame(table(train_891$SibSp, train_891$Pclass, train_891$Survived))
names(dat) <- c("SibSp", "Pclass", "Survived","Count")
ggplot(dat, aes(x=Pclass, y=Count, fill=SibSp)) + geom_bar(stat="identity") + xlab("Passenger Class") 

#Naturally, 1st class passengers had 1 or 2 Siblings/Spouses, which makes previous graph make some sense

dat <- data.frame(table(train_891$SibSp, train_891$Pclass, train_891$Survived, train_891$Age, train_891$Fare))
names(dat) <- c("SibSp", "Pclass", "Survived", "Age", "Fare", "Sex", "Count")
ggplot(dat, aes(x=Age, y=Pclass, fill=Survived)) + geom_dotplot(dotsize = 0.4) + xlab("Age") + facet_grid(Sex ~ .) 
  

ggplot(train_891, aes(x = Age, fill = factor(Survived))) + geom_histogram(bins=50) +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") +
  ggtitle("Age Vs Survived")


#Quick first submission

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submission_test_2.csv", row.names = FALSE)

#Let's figure out what features "matter" --- using aggregate() + intuition

prop.table(table(train$Sex, train$Survived),1)

aggregate(Survived ~ Sex, data=train, FUN=sum)

aggregate(Survived ~ Parch + Sex, data=train, FUN=sum)

train

aggregate(Survived ~ Parch + Pclass + Sex, data=train, FUN=sum)

test$Survived
survival_id$Survived

aggregate(Survived ~ Parch + Pclass + Sex, data=train, FUN=sum)

prop.table(table(train$Sex, train$Parch),1)

153/233

#Notice 65.6% females with 0 children Survived --- expected given less struggle to find kids etc. Compare to 61% female with 0 child initially.

aggregate(Survived ~ Parch + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#Very interesting comparision of Fare, Pclass, and Sex ---- perhaps we'll engineer a feature combinning class, fare, and Sex later

aggregate(Survived ~ Fare_New + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submission_test_3.csv", row.names = FALSE)

train$Fare



train$Fare_New


#Feataure Engineering Begins Here

train


#Beginning with algorithms

library(rpart)


dat <- data.frame(table(train_891$SibSp,train_891$Survived))
names(dat) <- c("SibSp","Survived","Count")
ggplot(dat, aes(x=SibSp, y=Count, fill=Survived)) + geom_bar(stat="identity") + xlab("SibSp") 


#Quick CART decision tree algorithm from Rpart to power through and subset passengers for us

train$Fare <- as.numeric(train$Fare)


test$Fare <- as.numeric(test$Fare)



#Let's create new feature, called family size (combination of SibSp and Parch) and see what that gets us 


train$Fam_Size <- train$SibSp + train$Parch
test$Fam_Size <- test$SibSp + test$Parch



fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Fam_Size,
             data=train,
             method="class")


fancyRpartPlot(fit)

summary(test)


#Prediction scored .76007 (worse than manually earned 78%!)


Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "rpart_dtree_3.csv", row.names = FALSE)


#Let's create new feature, called family size (combination of SibSp and Parch) and see what that gets us

