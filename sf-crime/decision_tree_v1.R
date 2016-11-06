#Testing decision tree with the script by MuditBachhawat(https://www.kaggle.com/muditbachhawat/sf-crime/decision-tree)
#決定木の練習。ほぼパクリ。時間のunixtimeと座標を説明変数にしています。

library(readr)
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)

#train <- read_csv("./sf_crime/train.csv.zip")
train <- read_csv("./sf_crime/train_spl.csv.zip")
#test <- read_csv("./sf_crime/test.csv.zip")
test <- read_csv("./sf_crime/test_spl.csv.zip")
#train <- train[sample(nrow(train),100000),]

train$Date2 <- as.numeric(unclass(as.POSIXct(train$Dates)))
test$Date2 <- as.numeric(unclass(as.POSIXct(test$Dates)))

tree <- rpart(Category ~ Date2 + X + Y,
              data = train,
              method = "class",
              control = rpart.control(minsplit = 200,cp=0)
              )

predicted <- predict(object = tree,newdata = test)
final <- data.frame(Id = test$Id , predicted)
colnames(final)  <- c("Id",levels(as.factor(train$Category)))

write.csv(final,"./sf_crime/result.csv",na="0",row.names = FALSE,quote = F)

rm(list=ls())