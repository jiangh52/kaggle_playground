setwd("/Users/hao.jiang/Playground/Kaggle_pjt")
##### Loading packages #####
library(dplyr)     
library(tidyr)     
library(reshape2)  
library(caret)     
library(readr)     
library(lubridate)  

# Create function to calculate Kaggle LogLoss score
# https://github.dena.jp/dena/kaggler_dena/blob/master/sf-crime/hao.jiang/score_checker.R
# https://www.kaggle.com/wiki/LogarithmicLoss
log.loss <- function(RESULT, TEST){
  RESULT[,-1] <- RESULT[,-1] / rowSums(RESULT[,-1])
  melt_result <- melt(RESULT, id.vars="Id")
  melt_result$Id <- as.numeric(melt_result$Id)
  melt_result$variable <- as.character(melt_result$variable)
  test_result$Category <- as.character(TEST$Category)
  score <- left_join(test_result,melt_result,by=c("Id"="Id","Category"="variable")) %>%
    mutate(value = ifelse(is.na(value),1e-15,value)) %>%
    mutate(log_score = - log(ifelse(value < 1e-15,1e-15,value))) %>%
    summarize(sum(log_score)) %>%
    as.numeric
  score <- score / nrow(TEST)
  print(score)
  }


##### Confirm and wrangling the data
TRAIN <- read_csv("./sf-crime/train.csv.zip") #Loading train data
# TEST <- read_csv("./sf-crime/test.csv.zip") #Loading train data
# summary(TRAIN) 
# dim(TRAIN) 

TRAIN <- TRAIN[,c(-3,-6,-7)] #Drop features that won't be used Descript、Resolution、Address
# TEST <- TEST[,-5]
# summary(TRAIN) 

# smoothScatter(TRAIN$X,TRAIN$Y)

# delete sample X-120.5 Y90
TRAIN <- TRAIN[TRAIN$Y!=90,]

# smoothScatter(TRAIN$X,TRAIN$Y)

# Generate year, month and hour from Dates
TRAIN$Id <- c(0:(nrow(TRAIN)-1))
TRAIN$value <- 1
TRAIN <- dcast(TRAIN,Id + Dates + Category + PdDistrict + X + Y ~ DayOfWeek,fill=0,value.var="value")
TRAIN$value <- 1
TRAIN <- dcast(TRAIN,Id + Dates + Category + X + Y + Monday + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday ~ PdDistrict,fill=0,value.var="value")

# TEST$value <- 1
# TEST <- dcast(TEST,Id + Dates + PdDistrict + X + Y ~ DayOfWeek,fill=0,value.var="value")
# TEST$value <- 1
# TEST <- dcast(TEST,Id + Dates + X + Y + Monday + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday ~ PdDistrict,fill=0,value.var="value")

TRAIN$Year <- year(TRAIN$Dates)
TRAIN$Month <- as.integer(month(TRAIN$Dates))
TRAIN$Hour <- as.integer(hour(TRAIN$Dates))
TRAIN$Category <- as.factor(TRAIN$Category)


# TEST$Year <- year(TEST$Dates)
# TEST$Month <- as.integer(month(TEST$Dates))
# TEST$Hour <- as.integer(hour(TEST$Dates))


# Drop Dates
TRAIN <- TRAIN[,c(-1,-2)]
# TEST <- TEST[,-1]
# summary(TRAIN)

# Drop feature with low info
#nzv <- nearZeroVar(TRAIN)
#TRAIN <- TRAIN[,nzv]

# split data into train - test set by 5:5
TRAIN_TEST_NO <- createDataPartition(TRAIN$Category,p=0.5,list=FALSE)
TRAIN_TRAIN  <- TRAIN[TRAIN_TEST_NO,]
TRAIN_TEST <- TRAIN[-TRAIN_TEST_NO,]
TRAIN_TRAIN <- data.frame(TRAIN_TRAIN)
TRAIN_TEST <- data.frame(TRAIN_TEST)

# ###################################################################
cat_lvs <- levels(TRAIN$Category)
result <- matrix(,nrow=nrow(TRAIN_TEST),ncol=40)
result <- data.frame(result)
colnames(result) <- c("Id",levels(TRAIN$Category))
result$Id <- c(0:(nrow(TRAIN_TEST)-1))

step_fit_list <- list()
for (cat_val in levels(TRAIN$Category)) {
	tmp_df <- TRAIN_TRAIN
	tmp_df$Category <- (tmp_df$Category == cat_val) * 1
	tmp_fit <- glm(Category ~ .,dat=tmp_df, family = "binomial")
	step_fit_list[[cat_val]] <- step(tmp_fit)
	result[,cat_val] <- predict(step_fit_list[[cat_val]],newdata=TRAIN_TEST[,-1],type="response")
}

test_result <- data.frame(Id=0:(nrow(TRAIN_TEST)-1), Category=TRAIN_TEST$Category)

log.loss(result, test_result)