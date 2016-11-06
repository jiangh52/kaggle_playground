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
TEST <- read_csv("./sf-crime/test.csv.zip") #Loading train data
# summary(TRAIN) 
# dim(TRAIN) 

TRAIN <- TRAIN[,c(-3,-6,-7)] #Drop features that won't be used Descript、Resolution、Address
# summary(TRAIN) 

# smoothScatter(TRAIN$X,TRAIN$Y)

# delete sample X-120.5 Y90
TRAIN <- TRAIN[TRAIN$Y!=90,]
#TRAIN <- TRAIN[sample(nrow(TRAIN),100000),]

# smoothScatter(TRAIN$X,TRAIN$Y)

# Generate year, month and hour from Dates
TRAIN$Year <- year(TRAIN$Dates)
TRAIN$Month <- as.integer(month(TRAIN$Dates))
TRAIN$Hour <- as.integer(hour(TRAIN$Dates))
TRAIN$Category <- as.factor(TRAIN$Category)
TRAIN$PdDistrict <- as.factor(TRAIN$PdDistrict)
TRAIN$DayOfWeek <- as.factor(TRAIN$DayOfWeek)

TEST$Year <- year(TEST$Dates)
TEST$Month <- as.integer(month(TEST$Dates))
TEST$Hour <- as.integer(hour(TEST$Dates))
TEST$PdDistrict <- as.factor(TEST$PdDistrict)
TEST$DayOfWeek <- as.factor(TEST$DayOfWeek)

# Drop Dates
TRAIN <- TRAIN[,-1]
# summary(TRAIN)

# Drop feature with low info
#nzv <- nearZeroVar(TRAIN)
#TRAIN <- TRAIN[,nzv]

# ###################################################################
cat_lvs <- levels(TRAIN$Category)
result <- matrix(,nrow=nrow(TEST),ncol=40)
result <- data.frame(result)
colnames(result) <- c("Id",levels(TRAIN$Category))
result$Id <- c(0:(nrow(TEST)-1))

for (cat_val in levels(TRAIN$Category)) {
	tmp_df <- TRAIN
	tmp_df$Category <- (tmp_df$Category == cat_val) * 1
	tmp_fit <- glm(Category ~ DayOfWeek + PdDistrict + X + Y + Year + Month + Hour,dat=tmp_df, family = "binomial")
	result[,cat_val] <- predict(tmp_fit,newdata=TEST[,-1],type="response")
}
