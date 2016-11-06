setwd('/Users/hao.jiang/Playground/Kaggle_pjt/')

#loading librarys
library(readr)
library(dplyr)
library(caret)
library(reshape2)
library(ranger)

#loading data
TRAIN <- read.csv('./titanic/train.csv')
TEST <- read.csv('./titanic/test.csv')

#drop data that won't be used and wrangle data
TRAIN <- TRAIN[,c(-1,-4,-9,-11,-12)]
TEST <- TEST[,c(-3,-8,-10,-11)]
TRAIN$Survived <- as.factor(TRAIN$Survived)
TEST[is.na(TEST[,"Fare"]),"Fare"] <- 7.7

# Drop feature with low info
# nzv <- nearZeroVar(TRAIN)
# TRAIN <- TRAIN[,nzv]

# split data into train_test set
TRAIN_TEST_NO <- createDataPartition(TRAIN$Pclass,p=0.7,list=F)
TRAIN_TRAIN <- TRAIN[TRAIN_TEST_NO,]
TRAIN_TEST <- TRAIN[-TRAIN_TEST_NO,]
TRAIN_TRAIN <- data.frame(TRAIN_TRAIN)
TRAIN_TEST <- data.frame(TRAIN_TEST)

###########################################
model <- ranger(Survived~., data = TRAIN_TRAIN[,-4], write.forest = TRUE)
tmp_result <- predict(model,data=TRAIN_TEST,type="class")

print("This is the result from ranger(random forest)")
print(sum((tmp_result$prediction == TRAIN_TEST$Survived)*1)/length(TRAIN_TEST$Survived))

model <- ranger(Survived~., data = TRAIN[,-4],write.forest= T)
result <- predict(model,data=TEST,type="class")
result <- data.frame(PassengerId=TEST[,1],Survived=result$prediction)

write.csv(result,'./titanic/result.csv',row.names=F,quote=F)