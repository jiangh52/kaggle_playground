setwd('/Users/hao.jiang/Playground/Kaggle_pjt/')

#loading librarys
library(readr)
library(dplyr)
library(caret)
library(reshape2)

#loading data
TRAIN <- read.csv('./titanic/train.csv')
TEST <- read.csv('./titanic/test.csv')

#drop data that won't be used and wrangle data
TRAIN <- TRAIN[,c(-1,-4,-9,-11,-12)]
TEST <- TEST[,c(-3,-8,-10,-11)]
TRAIN$Survived <- as.factor(TRAIN$Survived)
TRAIN$Pclass <- as.factor(TRAIN$Pclass)
TEST$Pclass <- as.factor(TEST$Pclass)

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
model <- glm(Survived ~ Pclass + Sex + SibSp, data = TRAIN_TRAIN,family=binomial(link = "logit"))  #Parch and Fare doesn't seems to matter, and Age has too many NAs
tmp_result <- predict(model, newdata = TRAIN_TEST, type="response",na.action=na.omit)
#tmp_result[is.na(tmp_result)] <- 0

print("This is the result of glm:")
print(sum((TRAIN_TEST$Survived == round(tmp_result))*1)/length(tmp_result))

model <- glm(Survived ~ Pclass + Sex + SibSp, data = TRAIN,family=binomial(link = "logit"))
result <- predict(model,newdata=TEST,type="response")
#result[is.na(result)] <- 0

result <- data.frame(PassengerId=TEST$PassengerId,Survived=round(result))
write.csv(result,"./titanic/result.csv",row.names=F,quote=F)