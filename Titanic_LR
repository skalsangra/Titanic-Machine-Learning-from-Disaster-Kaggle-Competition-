#loading training and testing dataset
titanic_train <- read.csv(file = "Titanic Dataset/train_new.csv", stringsAsFactors = FALSE)
titanic_test <- read.csv(file = "Titanic Dataset/test_new.csv", stringsAsFactors = FALSE)

#here we will use "Age_new" column in both dataset(I have performed Data Cleaning in MS Excel to fix Missing values of "AGE" and updated values of Age were in "Age_new Column)
names(titanic_train)
titanic_train_new <- titanic_train[ , c(1:5,7:12,18)]

names(titanic_test)
titanic_test_new <- titanic_test[ , c(1:4,6:11,16)]

#check that no datapoint is missing, otherwise we need to fix the dataset
apply(titanic_train_new,2,function(x) sum(is.na(x)))
apply(titanic_test_new,2,function(x) sum(is.na(x)))

#fixing missing "Embarked" 
table(titanic_train_new$Embarked)
#2 row does not have embarked so we can use 'Mode' Embarked for that 2 values
#    C   Q   S 
#2 168  77 644
#so mode is "S"
titanic_train_new[titanic_train_new$Embarked=="", "Embarked"] <- "S"
table(titanic_train_new$Embarked)

#fixing missing "Age"
#we have already Fix "age" in Excel .... so move on........

#fixing missing "Fare" in titanic_test_new
table(is.na(titanic_train_new$Fare))  # no missing value of "Fare"
table(is.na(titanic_test_new$Fare))   # 1 Fare value is missing

median_fare <- median(titanic_test_new$Fare, na.rm = TRUE)
titanic_test_new[is.na(titanic_test_new$Fare), "Fare"] <- median_fare
table(is.na(titanic_test_new$Fare))

str(titanic_train_new)
str(titanic_test_new)
#before there were missing values are categorised into different Factor
#thats why first we fixed the missing values and now casting into factor 

#categorical casting on both train and test dataset
titanic_train_new$Pclass <- as.factor(titanic_train_new$Pclass)
titanic_train_new$Sex <- as.factor(titanic_train_new$Sex)
titanic_train_new$Embarked <- as.factor(titanic_train_new$Embarked)
titanic_train_new$Survived <- as.factor(titanic_train_new$Survived)

titanic_test_new$Pclass <- as.factor(titanic_test_new$Pclass)
titanic_test_new$Sex <- as.factor(titanic_test_new$Sex)
titanic_test_new$Embarked <- as.factor(titanic_test_new$Embarked)

str(titanic_train_new)
str(titanic_test_new)

#fitting a Logistic Regression Model

#now first defined formula for predictiion of "Survived"
Survived_equation <- "Survived ~ Pclass + Sex + Age_new + SibSp + Parch + Fare + Embarked"
Survived_formula <- as.formula(Survived_equation)

logistic_model <- glm(formula= Survived_formula, data= titanic_train_new, family='binomial')
summary(logistic_model)

#prediction on titanic_test_new dataset
prediction <- predict(logistic_model, titanic_test_new, type = 'response')
prediction

#"prediction" returns Probabilities , so we convert them into "0" and "1" by using if-else
predicted=ifelse(prediction>0.5, 1, 0)


#putting the result in new dataframe against PassengerId
PassengerId <- titanic_test_new$PassengerId
output <- as.data.frame(PassengerId)
output$Survived <- predicted

kaggle_result <- read.csv("Titanic Dataset/gender_submission.csv")

#now lets compare our solution with Kaggle's solution
library(caret)
confusionMatrix(table(my_prediction=output$Survived, Kaggle_prediction = kaggle_result$Survived))

#Accuracy : 0.9402  (Wow!!!! Logistic Regression model gives higher Accuracy) 

#writing dataframe "output" into new csv file
write.csv(output, file = "gender_submission_lr.csv", row.names = FALSE)
