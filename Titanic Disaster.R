setwd("C:/Users/hassn/Downloads/Titanic Disaster")
 
installed.packages("nnet") 
installed.packages("kknn") 
installed.packages("caret") 
installed.packages("rpart") 
installed.packages("tidyverse") 
installed.packages("xgboost")
installed.packages("here")
installed.packages("e1071") 
installed.packages("klaR") 
installed.packages("kknn") 
installed.packages("caTools")
installed.packages("class")  
installed.packages("randomForest") 

library(tidyverse) 
library(randomForest)
library(nnet)
library(e1071)
library(kknn)
library(xgboost)
library(here) 
library(rpart)
library(caret) 

# Read in the data 
titanic <-read.table("train.csv",header = TRUE,sep = ",") 
titanic_test <-read.table("test.csv",header = TRUE,sep = ",") 
passengerID<-subset(titanic_test, select = c(PassengerId)) 
str(titanic) 

#remove irrelevant columns  
titanic <- titanic %>% select(-c(PassengerId,Name,Ticket,Cabin))
titanic_test <- titanic_test %>% select(-c(PassengerId,Name,Ticket,Cabin))

#filling missing values
colSums(is.na(titanic))
colSums(is.na(titanic_test))

titanic$Age[is.na(titanic$Age)] <- median(titanic$Age,na.rm = TRUE) 
#titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare,na.rm = TRUE) 

titanic_test$Age[is.na(titanic_test$Age)] <- median(titanic_test$Age,na.rm = TRUE)  
titanic_test$Fare[is.na(titanic_test$Fare)] <- median(titanic_test$Fare,na.rm = TRUE) 

unique(titanic_test$Parch)
unique(titanic$Parch)

#convert to factor 
titanic$Sex<-factor(titanic$Sex)
titanic$Pclass<-factor(titanic$Pclass) 
titanic$Survived<-factor(titanic$Survived) 

titanic_test$Pclass<-factor(titanic_test$Pclass) 
titanic_test$Sex<-factor(titanic_test$Sex)




#view data
glimpse(titanic) 
glimpse(titanic_test)  

#split trainDataset into train and validation 
train_percentage<-caret::createDataPartition(titanic$Survived,p=0.7,list=FALSE)
train_set<-titanic[train_percentage,]
validation_set<-titanic[-train_percentage,]

set.seed( 20735)

#Random Forest
rfModel<-randomForest::randomForest(Survived~.,data=train_set)
pred<-predict(rfModel,validation_set[-8])
cm<-confusionMatrix(table(pred, validation_set$Survived))
cm
# acc=82%

#SVM model
svmmodel<-svm(formula = Survived ~ ., data = train_set,type = 'C-classification',kernel = 'linear')
pred<-predict(svmmodel,validation_set[-8])
cm<-confusionMatrix(table(pred, validation_set$Survived))
cm 
#acc=79%  

#decision trees  
fitControl <- trainControl(method ="cv",number = 5)
dtModel <- train(Survived~ .,data = train_set , method ="rpart", trControl= fitControl )
pred<-predict(dtModel,validation_set[-8])
cm<-confusionMatrix(table(pred, validation_set$Survived))
cm
#acc=79%



#naive bayes 
nbModel<- naiveBayes(Survived ~ ., data = train_set)
pred<-predict(nbModel,valid[-8])
cm<-confusionMatrix(table(pred, validation_set$Survived))
cm
#acc=50%   



#prediction for test data  
Survived<-predict(rfModel,titanic_test)
dt<-cbind(passengerID,Survived)
write.csv(dt,"C:/Users/hassn/Downloads/Titanic Disaster/rf_Submission.csv", row.names = FALSE)

