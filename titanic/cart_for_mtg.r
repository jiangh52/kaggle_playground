################先ずR環境の基本設定を行う################
#Rのワーキングダイレクトリーを設定する
# Rでは"#"でスクリプトをコメントアウトできる
setwd('/Users/hao.jiang/Playground/Kaggle_pjt/titanic/')

#必要なライブラリーをインストールする
#install.packages("caret") #cross validationやモデルパラメーターチューニング用の素晴らしいパッケージで
#install.packages("rpart") #cartの決定木用のパッケージ
#install.packages("rpart.plot") #決定木をプロットするためのパッケージ

#ライブラリーを使うには、先ずロードする必要がある
library(readr)  #csvファイルを読むなとに必要なライブラリー
library(caret)
library(rpart)
library(rpart.plot)

################データの読み込みや整理を行う################

#使われるデータを読み込む
TRAIN <- read.csv('./train.csv') #教師データを読み込む
TEST <- read.csv('./test.csv') #テストデータを読み込む

#先ず、基本的に使われるデータの状況を確認しよう
summary(TRAIN)
summary(TEST)

#Ageには大量のNAがある、空となっているCabinも多く、さらにTicketの番号は同一採番ルールに従っていないこともわかる
#このデータセットについて簡単に整理してみよう。幾つか今回のモデルに使わないパラメーターの削除する。
#例えば、教師データにあるPassengerId、Name、Ticket、Cabinの除いてみよう
TRAIN <- TRAIN[,c(-1,-4,-9,-11)]
TEST <- TEST[,c(-3,-8,-10)]
TRAIN$Survived <- as.factor(TRAIN$Survived) #Intとなっている目的変数をfactor化する

# 選んだモデルの精度を検証するために、教師データをさらに訓練用データと検証用データに分ける。
# これは交差検証(Cross Validation, 略してCV)という
TRAIN_TEST_NO <- createDataPartition(TRAIN$Survived,p=0.7,list=F)
TRAIN_TRAIN <- TRAIN[TRAIN_TEST_NO,]
TRAIN_TEST <- TRAIN[-TRAIN_TEST_NO,]

################実際に予測を行う################
# Caretのtrainを使って、最適となる決定木のcp変数をチューニングする。cpとは？はまた後日
model <- train(Survived ~., data = TRAIN_TRAIN, method="rpart",tuneLength=5) 

#チューニングの結果を見てみると。それぞれのcp値に対して、Accuracyなど予測精度の評価指標も示されている。
print(model$results)
#基本的にAccuracyは高ければよいわけで、とりあえず0.02あたりは良さげであることがわかる。


# cp =0.02で、訓練を行う
model <- rpart(Survived ~., data = TRAIN_TRAIN,control=rpart.control(cp=0.02))
#上記決定木のモデルをプロットしてみる
rpart.plot(model,type=1,under=1,extra=1,uniform=T)
# # 上記モデルに対して、検証用のテストデータを使って予測をする
# tmp_result <- predict(model,newdata=TRAIN_TEST,type="class")
# # 検証用の予測の精度を評価する
# print("モデルの予測精度の検証結果は：")
# print(sum((TRAIN_TEST$Survived == tmp_result)*1)/length(tmp_result))

# # 提出用の予測を行う
# model <- rpart(Survived ~., data = TRAIN, control=rpart.control(cp=0.02))
# # 最終的な決定木はこのようになる
# rpart.plot(model,type=1,under=1,extra=1,uniform=T)

# result <- predict(model,newdata=TEST,type="class")

# #出力用のデータを形成する
# result <- data.frame(PassengerId=TEST$PassengerId,Survived=result)
# head(result)

# #結果データをcsvファイルに出力する
# write.csv(result,"./hao.jiang/result.csv",row.names=F,quote=F)

# #データ可視化のおまけ

# #sex(性別)とpclass(乗客のクラス)は最終的な生存状況に大きく関与していることがわかるので、
# #この二つのパラメーターにまつわる可視化をいろいろ試してみよう

# #先ず、TRAINの中に、pclass別の乗船人数を見てみよう
# ggplot(TRAIN, aes(Pclass)) + stat_bin(binwidth=0.5)
# #Pclass=1とPclass=2の人数が少なく、Pclass=3の人数が一番多いことがわかる。
# #ではそれぞれのPclassでは、生存状況はどうなっているのでしょう？
# ggplot(TRAIN, aes(Pclass,fill=Survived)) + stat_bin(binwidth=0.5)  + geom_bar(position = "stack")
# #Pclass=3の乗客の生存率が低いことがわかる
# #では、さらに性別別でこのデータを見てみると
# ggplot(TRAIN, aes(Pclass,fill=Survived)) + stat_bin(binwidth=0.5)  + geom_bar(position = "stack") + facet_grid(. ~ Sex)
# #なるほど、女性の乗客数は少ないが、全体的に男性より生存率高いことがわかる
# #また、上記図を別の方法でも表現できる。例えば jitter図でみると
# ggplot(TRAIN,aes(Pclass,Sex)) + geom_jitter(aes(color=Survived))


# #決定木可視化のおまけ
# library(rattle)
# library(RColorBrewer)
# fancyRpartPlot(model)