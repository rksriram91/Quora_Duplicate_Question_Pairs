setwd("C:/Independent Projects/QUORA")
library(data.table) # has fread
#1st column irrelevant
train <- fread("train.csv",header = TRUE,stringsAsFactors = FALSE)
train<-data.frame(train)

lengthQuestion <- function(question){
  return(nchar(question))
}

train=train[!(train$question2==''),]

train[c("lenQ1")] <- lapply(train[c("question1")], lengthQuestion)
train[c("lenQ2")] <- lapply(train[c("question2")], lengthQuestion)
#train[c("lenDiff")] <- train[c("lenQ1")]-train[c("lenQ2")]
train[c("lenDivide")] <- train[c("lenQ1")]/train[c("lenQ2")]

range(train$lenDivide)
plot(train$lenQ1,train$lenQ2,col=c("red","blue")[train$is_duplicate])
library(ggplot2)
ggplot(train, aes(lenQ1, lenQ2, color = train$is_duplicate)) + geom_point()

ggplot(train, aes(is_duplicate, lenDivide)) + geom_point()

range(train$lenDivide[train$is_duplicate==1]) #0.1643836 6.6666667
range(train$lenDivide[train$is_duplicate==0]) #6.711409e-03 1.170000e+02

#create empty vector pred_is_duplicate
pred_is_duplicate['train$question1=='' | train$question2==''']=0
#Model Rule 1
pred_is_duplicate[train$lenDivide<0.16 | train$lenDivide>6.7]=0

length(train[train$lenDivide<0.16 | train$lenDivide>6.7,])

#Model Rule 1
