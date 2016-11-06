#This is a public score checker. Original train data is splitted here into train and test data.

library(readr)
library(dplyr)
library(tidyr)
library(reshape2)

data_pass <- "./sf-crime/"
result_file_name <- "result.csv"
test_result_file_name <- "test_rl.csv.zip"

result <- read_csv(paste(data_pass,result_file_name,sep=""))
test_result <- read_csv(paste(data_pass, test_result_file_name,sep=""))

melt_result <- melt(result,id.vars="Id")
melt_result$Id <- as.numeric(melt_result$Id)
melt_result$variable <- as.character(melt_result$variable)

score <- left_join(test_result,melt_result,by=c("Id"="Id","Category"="variable")) %>%
	mutate(value = ifelse(is.na(value),1e-15,value)) %>%
	mutate(log_score = - log(ifelse(value < 1e-15,1e-15,value))) %>%
	summarize(sum(log_score)) %>%
	as.numeric
	
score <- score / nrow(test_result)
print(score)

rm(list=ls())