#setwd("/users/hao.jiang/playground/kaggle_pjt/shelter-animal-outcomes")

# load librarys
library(readr)
library(dplyr)
library(reshape2)
library(lubridate)	#datetime data process
library(chron)		#datetime data process
library(caret)
library(ranger)		#random forest library ranger

# load data
train <- read.csv("./train.csv.gz")
test <- read.csv("./test.csv.gz")

# look at the data
summary(train)
summary(test)

# do some feature extracting, combine train test set first
test$OutcomeType <- NA
test$OutcomeSubtype <- NA
train$ID <- NA
combi <- rbind(train[,-1],test)

# extract feature from DateTime
combi$Year <- year(combi$DateTime)
combi$Month <- month(combi$DateTime)
combi$Dayofweek <- weekdays(as.Date(combi$DateTime))
combi$Isholiday <- is.holiday(combi$DateTime)	#this one's from chron library

# other feature extraction
combi$Hasname <- combi$Name == ""	#flag for whether or not the pool creature has a name
combi$SexuponOutcome[combi$SexuponOutcome == ""] <- "Unknown"
combi$Ismixbreed <- regexpr("Mix",combi$Breed) > 0 | regexpr("/",combi$Breed) > 0	#flag for mixed breed
combi$Ismixcolor <- regexpr("/",combi$Color) > 0		#flag for mixed color

# split combi back to train test set
train <- combi[!is.na(combi$OutcomeType),]
test <- combi[is.na(combi$OutcomeType),]

#let's do a performance check
log.loss <- function(RESULT, TEST){
  RESULT[,-1] <- RESULT[,-1] / rowSums(RESULT[,-1])
  melt_result <- melt(RESULT, id.vars="Id")
  melt_result$Id <- as.numeric(melt_result$Id)
  melt_result$variable <- as.character(melt_result$variable)
  TEST$Category <- as.character(TEST$Category)
  score <- left_join(TEST,melt_result,by=c("Id"="Id","Category"="variable")) %>%
    mutate(value = ifelse(is.na(value),1e-15,value)) %>%
    mutate(log_score = - log(ifelse(value < 1e-15,1e-15,value))) %>%
    summarize(sum(log_score)) %>%
    as.numeric
  score <- score / nrow(TEST)
  print(score)
  }

# train_no <- createDataPartition(train$OutcomeType,p=0.7,list=F)
# train_train <- train[train_no,]
# train_test <- train[-train_no,]

# cv_fit <- ranger(OutcomeType ~ AnimalType + SexuponOutcome + AgeuponOutcome + Breed + Color + Year + Month + Dayofweek + Isholiday + Hasname + Ismixbreed + Ismixcolor, data=train_train, write.forest=TRUE, probability=TRUE)
# cv_pred <- predict(cv_fit,data=train_test)
# cv_pred_check <- data.frame(Id=1:length(train_test$ID), cv_pred$predictions)
# cv_result <- data.frame(Id=1:length(train_test$ID),Category=train_test$OutcomeType)
# log.loss(cv_pred_check,cv_result)
# 0.884611


# fit it into ranger, we should be able to process breed and color data further, but for this one let's just leave them here.
fit <- ranger(OutcomeType ~ AnimalType + SexuponOutcome + AgeuponOutcome + Breed + Color + Year + Month + Dayofweek + Isholiday + Hasname + Ismixbreed + Ismixcolor, data=train, write.forest=TRUE, probability=TRUE)

predict_result <- predict(fit,data=test)
result <- data.frame(ID=test$ID,predict_result$predictions)

write.csv(result,"./result.csv",row.names=F,quote=F)