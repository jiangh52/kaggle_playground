#Testing decision tree with the script by MuditBachhawat(https://www.kaggle.com/muditbachhawat/sf-crime/decision-tree)
#決定木の練習。警察署を説明変数にしています。

library(readr)
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)

#train <- read_csv("./sf-crime/train.csv.zip")
train <- read_csv("./sf-crime/train_spl.csv.zip")
#test <- read_csv("./sf-crime/test.csv.zip")
test <- read_csv("./sf-crime/test_spl.csv.zip")
#train <- train[sample(nrow(train),100000),]

tree <- rpart(Category ~ PdDistrict + DayOfWeek,
              data = train,
              method = "class",
              control = rpart.control(minsplit = 200,cp=0)
              )

predicted <- predict(object = tree,newdata = test)
final <- data.frame(Id = test$Id , predicted)
colnames(final)  <- c("Id",levels(as.factor(train$Category)))

write.csv(final,"./sf-crime/result.csv",na="0",row.names = FALSE,quote = F)

rm(list=ls())