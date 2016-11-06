#This is a public score checker. Original train data is splitted here into train and test data.

library(readr)
library(dplyr)
library(tidyr)

test_set_ratio <- 0.1
data_pass <- "/Users/hao.jiang/Mystation/Kaggle_pjt/sf_crime/"
train_file_name <- "train.csv.zip"

train <- read_csv(paste(data_pass,train_file_name,sep=""))

id <- sample(1:2,nrow(train),replace=TRUE,prob=c(1 - test_set_ratio,test_set_ratio))

train_data <- train[id==1,]
test_data <- train[id==2,]

test_result <- test_data %>%
	mutate(Id=as.numeric(rownames(test_data)) - 1) %>%
	select(Id,Category)

test_data <- test_data %>%
	mutate(Id=as.numeric(rownames(test_data)) - 1) %>%
	select(Id,Dates,DayOfWeek,PdDistrict,Address,X,Y,Category)


write.csv(train_data,paste(data_pass,"train_spl.csv",sep=""),na="0",row.names=FALSE)
write.csv(test_data,paste(data_pass,"test_spl.csv",sep=""),na="0",row.names=FALSE)
write.csv(test_result,paste(data_pass,"test_rl.csv",sep=""),na="0",row.names=FALSE)
rm(list=ls())