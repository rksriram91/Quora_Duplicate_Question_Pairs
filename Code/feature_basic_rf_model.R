#setwd("C:/Independent Projects/QUORA")
setwd("../Data")
list.files()
library(data.table)
library(randomForest)
library(class)
library(e1071)
library(rpart)
library(caret)
#Reading the Train dataset
#merged_data = fread("merged_data.csv",header = TRUE,stringsAsFactors = FALSE,select=2:103)
library(data.table) # has fread
train<-fread("train.csv",header = TRUE,stringsAsFactors = FALSE)
#test<-fread("test.csv",header = TRUE,stringsAsFactors = FALSE)
train<-data.frame(train,stringsAsFactors = FALSE)

#numwords function is in build_features.R
train['numcharsratio']<-nchar(train$question1)/nchar(train$question2)
train['numWordsratio']<-numWords(train$question1)/numWords(train$question2)
train['numCommonWordsNormalised']<-numCommonWordsNormalized(train$question1,train$question2)
train<-train[,-(2:5)]
train[!complete.cases(train),]
#test_data <- test_data[complete.cases(test_data), ]

#basic feature building
numWords <- function(question){
  return (sapply(strsplit(question, " "), length))
}

## number of characters in a question. this excludes spaces
numChars <- function(question){
  return(nchar(question) - length(gregexpr(" ", question)))
}

## number of common words between two questions
numCommonWords <- function(question1, question2){
  split1 <- unlist(strsplit(question1, split=" "))
  split2 <- unlist(strsplit(question2, split=" "))
  return(length(intersect(split1, split2)))
}

## number of common words normalized
numCommonWordsNormalized <- function(question1, question2) {
  return(numCommonWords(question1, question2)/(numWords(question1)*numWords(question2)))
}

## number of characters divided by length of question
numCharByWord <- function(question){
  return(numChars(question)/lengthQuestion(question))
}

library(randomForest)
set.seed(415)
rf_classifier <- randomForest(as.numeric(train$numcharsratio),as.factor(train$is_duplicate),
                              importance=TRUE, 
                              ntree=500,nodesize=10000)
rm(y_pred_naiveBayes)

write.csv(train,'train_basic_features.csv')
write.csv(test,'test_basic_features.csv')
rm(test)

#train<-fread("../Data/train_basic_features.csv")
#train<-data.frame(train)
#train<-train[,-1]
#id<-train$id
#train<-train[,-1]#removing id
#train$is_duplicate<- as.factor(train$is_duplicate)

train <- data.table(train)
invisible(lapply(names(train),function(.name) set(train, which(is.infinite(train[[.name]])), j = .name,value =NA)))
which(is.na(train),arr.ind=TRUE)
#imputing column mean
train<-data.frame(train)
for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}
rf_classifier <- randomForest(is_duplicate ~ .,data=train,
                              importance=TRUE, 
                              ntree=500,nodesize=10000)

#test<-fread("../Data/test_basic_features.csv")
#test<-data.frame(test)
#test<-test[,-1]
#tid<-test$test_id
#test<-test[,-1]#removing id

test <- data.table(test)
invisible(lapply(names(test),function(.name) set(test, which(is.infinite(test[[.name]])), j = .name,value =NA)))
which(is.na(test),arr.ind=TRUE)
#imputing column mean for missing features
test<-data.frame(test)
for(i in 1:ncol(test)){
  test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
}

predic_rf_test <- predict(rf_classifier,type = "prob",test)
head(predic_rf_test)
submission<-data.frame(test_id=tid,is_duplicate=predic_rf_test[,2])
write.csv(submission,'../submission.csv',row.names=FALSE)

#naive Bayes Prediction
naiveBayes_classifier<- naiveBayes(is_duplicate ~ ., data = train)
naiveBayes_pred <- predict(naiveBayes_classifier,type="raw",newdata = test)
head(head(predic_rf_test))
submission<-data.frame(test_id=tid,is_duplicate=naiveBayes_pred[,2])
write.csv(submission,'../submission_NB.csv',row.names=FALSE)
