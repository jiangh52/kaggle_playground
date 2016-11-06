#ver1に対して、さらに幾つか試したいものがある
#1.いろんな情報から親かどうかを判定する
#2.年齢予測の精度を上げてみることもできるのでは？
#3.細かい家族情報が多いことで決定木のパフォーマンスが影響される可能性があるので、実は必要がなかったりする？
#4.家族判定の精度をもっと向上させることができないのか？


################先ずR環境の基本設定を行う################
#Rのワーキングダイレクトリーを設定する
# Rでは"#"でスクリプトをコメントアウトできる
setwd('/Users/hao.jiang/Playground/Kaggle_pjt/titanic/')


#ライブラリーを使うには、先ずロードする必要がある
library(readr)  #csvファイルを読むなとに必要なライブラリー
library(caret)
library(rpart)
library(reshape2)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(dplyr)
################データの読み込みや整理を行う################

#使われるデータを読み込む
TRAIN <- read.csv('./train.csv') #教師データを読み込む
TEST <- read.csv('./test.csv') #テストデータを読み込む

#先ず、基本的に使われるデータの状況を確認しよう
summary(TRAIN)
summary(TEST)

#今回、Nameからのデータをフル活用するために、TRAINとTESTのデータを一旦マージする
TEST$Survived <- NA   #TESTに存在しないSurvivedを追加する
combi <- rbind(TRAIN, TEST)  #TRAINとTESTをマージする
combi$Name <- as.character(combi$Name)  #Nameカラムを一旦普通な文字列に変換する

#Nameからタイトルを抽出
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

#タイトルをまとめる
combi$Title[combi$Title %in% c('Miss', 'Mlle')] <- 'Miss' #一般な未婚女性
combi$Title[combi$Title %in% c('Mme', 'Ms')] <- 'Ms' #婚姻状況不明な女性
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady' #高貴な女性
combi$Title[combi$Title %in% c('Don', 'Sir', 'Jonkheer')] <- 'Sir' #高貴な男性
combi$Title[combi$Title %in% c('Major', 'Col')] <- 'Col' #軍事関係しそうな男性

#table(combi$Title)
combi$Title <- factor(combi$Title)


combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize == 1] <- 'Single'
combi$FamilyID[combi$FamilySize == 2] <- 'Small'

#家庭サイズと実際のデータ数がマッチしない家庭に関しても処理をする
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

combi$Embarked[combi$Embarked==''] <- 'S'
combi$Fare[is.na(combi$Fare)] <- mean(combi$Fare[combi$Pclass==3 & !is.na(combi$Fare)])

###ageの予測をcv検証してみる
train_no <- createDataPartition(combi$Age[!is.na(combi$Age)],p=0.7,list=F)
train_train <- combi[!is.na(combi$Age),][train_no,]
train_test <- combi[!is.na(combi$Age),][-train_no,]

Agefit <- rpart(Age ~ Pclass + Sex + Fare + Embarked + Title + FamilySize + FamilyID,data= train_train,method="anova")
Agepredict <- predict(Agefit,newdata=train_test)
print((sum((Agepredict - train_test$Age) ^2) / length(Agepredict))^0.5)