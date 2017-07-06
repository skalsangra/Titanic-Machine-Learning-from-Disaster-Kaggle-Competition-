#loading training and testing dataset
titanic_train <- read.csv(file = "Titanic Dataset/train.csv", stringsAsFactors = FALSE)
titanic_test <- read.csv(file = "Titanic Dataset/test.csv", stringsAsFactors = FALSE)

str(titanic_train)
#check that no datapoint is missing, otherwise we need to fix the dataset
apply(titanic_train,2,function(x) sum(is.na(x)))
apply(titanic_test,2,function(x) sum(is.na(x)))

#to deal with missing values let first combine test and train dataset
#first we add a new column in both test and train, named "isTrainData"
#isTrainData =TRUE for all row in Train dataset and, =FALSE for all row in Test dataset
titanic_train$isTrainData <- TRUE
titanic_test$isTrainData <- FALSE

titanic_test$Survived <- NA

titanic_full <- rbind(titanic_train, titanic_test)

#fixing missing "Embarked" 
table(titanic_full$Embarked)
#2 row does not have embarked so we can use 'Mode' Embarked for that 2 values
#    C   Q   S 
#2 270 123 914 
#so mode is "S"
titanic_full[titanic_full$Embarked=="", "Embarked"] <- "S"
table(titanic_full$Embarked)

#fixing missing "Age"
table(is.na(titanic_full$Age))
#263 values of Age are missing
median_age <- median(titanic_full$Age, na.rm = TRUE)
titanic_full[is.na(titanic_full$Age), "Age"] <- median_age
table(is.na(titanic_full$Age))

#fixing missing "Fare"
table(is.na(titanic_full$Fare))
#only 1 Fare value is missing
median_fare <- median(titanic_full$Fare, na.rm = TRUE)
titanic_full[is.na(titanic_full$Fare), "Fare"] <- median_fare
table(is.na(titanic_full$Fare))

#before there were missing values are categorised into different Factor
#thats why first we fixed the missing values and now casting into factor 
#categorical casting
titanic_full$Pclass <- as.factor(titanic_full$Pclass)
titanic_full$Sex <- as.factor(titanic_full$Sex)
titanic_full$Embarked <- as.factor(titanic_full$Embarked)


#now extract back Train and Test dataset from full dataset
#to identify train and test data we use "isTrainData" variable value
titanic_train <- titanic_full[titanic_full$isTrainData==TRUE, ]
titanic_test <- titanic_full[titanic_full$isTrainData==FALSE, ]

titanic_train$Survived <- as.factor(titanic_train$Survived)

#fitting a random forest model
library(randomForest)
#now first defined formula for predictiion of "Survived"
Survived_equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived_formula <- as.formula(Survived_equation)
titanic_model <- randomForest(formula= Survived_formula, data= titanic_train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic_test))
 
#prediction on test dataset
Survived_prediction <- predict(titanic_model, newdata = titanic_test)

#putting the result in new dataframe against PassengerId
PassengerId <- titanic_test$PassengerId
output <- as.data.frame(PassengerId)
output$Survived <- Survived_prediction

#writing dataframe "output" into new csv file
write.csv(output, file = "gender_submission_rf.csv", row.names = FALSE)

kaggle_result <- read.csv("Titanic Dataset/gender_submission.csv")

#now lets compare our solution with Kaggle's solution
library(caret)
confusionMatrix(table(my_prediction=output$Survived, Kaggle_prediction = kaggle_result$Survived))
