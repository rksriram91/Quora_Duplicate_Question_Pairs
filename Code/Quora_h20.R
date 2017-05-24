setwd("C:/Independent Projects/QUORA")
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
test<-fread("test.csv",header = TRUE,stringsAsFactors = FALSE)
train<-data.frame(train,stringsAsFactors = FALSE)

#numwords function is in build_features.R
train['numcharsratio']<-nchar(train$question1)/nchar(train$question2)
train['numWordsratio']<-numWords(train$question1)/numWords(train$question2)
train['numCommonWordsNormalised']<-numCommonWordsNormalized(train$question1,train$question2)
train<-train[,-(2:5)]
train[!complete.cases(train),]
naiveBayes_classifier<- naiveBayes(train[,3:5],train[,2])
svm_linear_classifier= svm(train[1:10000,3:5],train[1:10000,2],type='C-classification',kernel='linear',
                           probability=T)
#test_data <- test_data[complete.cases(test_data), ]


test<-data.frame(test)
test['numcharsratio']<-nchar(test$question1)/nchar(test$question2)
test['numWordsratio']<-numWords(test$question1)/numWords(test$question2)
test['numCommonWordsNormalised']<-numCommonWordsNormalized(test$question1,test$question2)
test<-test[,-(2:3)]

y_pred_naiveBayes= predict(naiveBayes_classifier,type="raw",newdata=test[,-1],
                           nan.rm=TRUE,Inf.rm=TRUE,na.rm=TRUE,probability=TRUE)
confusionMatrix(test[,2],y_pred_naiveBayes)

library(randomForest)
set.seed(415)
rf_classifier <- randomForest(as.numeric(train$numcharsratio),as.factor(train$is_duplicate),
                    importance=TRUE, 
                    ntree=500,nodesize=10000)
rm(y_pred_naiveBayes)
rf_classifier <- randomForest(as.factor(train$is_duplicate) ~ as.numeric(train$numcharsratio) +
                                as.numeric(train$numWordsratio) + 
                                as.numeric(train$numCommonWordsNormalised),
                              importance=TRUE, 
                              ntree=500,nodesize=10000)
write.csv(train,'train_basic_features.csv')
write.csv(test,'test_basic_features.csv')
rm(test)

train['is_duplicate']<-as.factor(train$is_duplicate)
test<-test[,-1]

#H20 https://www.analyticsvidhya.com/blog/2016/05/h2o-data-table-build-models-large-data-sets/
install.packages("h2o")
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(train)
test<-fread("test_basic_features.csv",header = TRUE,stringsAsFactors = FALSE)
test.h2o<-as.h2o(test)
system.time(
  rforest.model <- h2o.randomForest(y=2, x=c(3,4,5), training_frame = train.h2o, 
                                    ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)
)

h2o.shutdown()
Y
