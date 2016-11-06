####
#今回の内容は主にfeature engineeringに着目している。
#この課題に関する基本的な話は前回のwikiを参照にしてください
#https://wiki.dena.jp/pages/viewpage.action?pageId=90518984
####

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

#table(combi$Title)
combi$Title <- factor(combi$Title)

#家庭で状況を判別 苗字+乗船家族人数で家庭を判別する
#ちなみに細かく見ていくと、この識別方法にはそこそこ問題があることがわかる。理由は
#1.実はSibSpとParchの情報が必ずしも正確というわけではない
#2.女性が結婚すると苗字が変わる
#だが、ここで一旦このモデルで進んでみることにする
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

#家庭サイズと実際のデータ数がマッチしない家庭に関しても処理をする
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#一旦NAとなっている年齢を予測してから、問題の目的変数を予測する
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Survived <- as.factor(combi$Survived)

train <- combi[1:891,]
test <- combi[892:1309,]

#####↑↑↑↑↑ Feature処理はここまで終了とする######
####ここでやはり一旦CVで結果を検証してみる

train_no <- createDataPartition(train$Survived, p=0.7,list=F)
train_train <- train[train_no,]
train_test <- train[-train_no,]

#fit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train_train, method="rpart", tuneLength=5)

#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train_train, method="class",control=rpart.control(cp=0.0125))
# fit <- rpart(Survived ~ Pclass + Sex + Age +Fare + Embarked + Title + FamilySize + FamilyID,data=train_train, method="class",control=rpart.control(cp=0.0125))
# cv_rel <- predict(fit,newdata=train_test,type="class")
# print(sum((train_test$Survived == cv_rel)*1)/length(cv_rel))

####提出用データの生成
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train, method="class",control=rpart.control(cp=0.0125))
result <- predict(fit,newdata=test,type="class")
result <- data.frame(PassengerId=test$PassengerId,Survived=result)

write.csv(result,"./result.csv",row.names=F,quote=F)
fancyRpartPlot(fit)

