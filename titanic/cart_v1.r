setwd('/Users/hao.jiang/Playground/Kaggle_pjt/')

#loading librarys
library(readr)
library(caret)
library(rpart)

#loading data
TRAIN <- read.csv('./titanic/train.csv')
TEST <- read.csv('./titanic/test.csv')

#drop data that won't be used and wrangle data
TRAIN <- TRAIN[,c(-1,-4,-9,-11,-12)]
TEST <- TEST[,c(-3,-8,-10,-11)]
TRAIN$Survived <- as.factor(TRAIN$Survived)

# Drop feature with low info
# nzv <- nearZeroVar(TRAIN)
# TRAIN <- TRAIN[,nzv]

# split data into train_test set
TRAIN_TEST_NO <- createDataPartition(TRAIN$Survived,p=0.7,list=F)
TRAIN_TRAIN <- TRAIN[TRAIN_TEST_NO,]
TRAIN_TEST <- TRAIN[-TRAIN_TEST_NO,]
TRAIN_TRAIN <- data.frame(TRAIN_TRAIN)
TRAIN_TEST <- data.frame(TRAIN_TEST)

##################
#model <- train(Survived ~., data = TRAIN_TRAIN, method="rpart",tuneLength=10) #round 0.25 seems to be best
model <- rpart(Survived ~., data = TRAIN_TRAIN,control=rpart.control(cp=0.25))

tmp_result <- predict(model,newdata=TRAIN_TEST,type="class")
print("This is the splitted set result")
print(sum((TRAIN_TEST$Survived == tmp_result)*1)/length(tmp_result))

model <- rpart(Survived ~., data = TRAIN, control=rpart.control(cp=0.25))
result <- predict(model,newdata=TEST,type="class")
result <- data.frame(PassengerId=TEST$PassengerId,Survived=result)

write.csv(result,"./titanic/result.csv",row.names=F,quote=F)
