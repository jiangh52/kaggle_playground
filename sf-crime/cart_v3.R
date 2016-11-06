#Featureの処理をしてみます。
setwd('/users/hao.jiang/playground/kaggle_pjt/')

#libraryの準備をします。
library(readr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(reshape2)
library(ggplot2)
library(caret)
library(lubridate)
library(chron)

TRAIN <- read_csv("./sf-crime/train.csv.zip")
TEST <- read_csv("./sf-crime/test.csv.zip")

#TRAINとテストデータをマージする
TRAIN <- TRAIN[,c(-3,-6)]
TRAIN$Id <- NA
TEST$Category <- NA
COMBI <- rbind(TRAIN,TEST)

#Dateからフィーチャー抽出をする
COMBI$Year <- year(COMBI$Dates)
COMBI$Month <- month(COMBI$Dates)
COMBI$Isholiday <- is.holiday(COMBI$Dates)
COMBI$Isbeforeholiday <- is.holiday(as.Date(COMBI$Dates) + 1)
COMBI$Hour <- hour(COMBI$Dates)

#Addressをみていくと、基本的に XXX / XXXと XXX Block of XXXの二パターンがあるとわかる。これはおそらく街角かどうかの違い。これも表現してみる。
COMBI$Iscorner <- regexpr("/",COMBI$Address) > 0
# plot_test <- data.frame(prop.table(table(Category=TRAIN$Category,Hour=hour(TRAIN$Dates)),1))
# ggplot(plot_test,aes(Hour,Category)) + geom_tile(aes(fill=Freq))
# それぞれの犯罪に対して、時間帯別の発生率が異なるっぽいので、Hourを時間帯にまとめれなさそう。

# smoothScatter(COMBI$X,COMBI$Y)
# summary(as.factor(COMBI$Category[COMBI$Y > 89]))
# Y=90のあたりに、変なデータ群があることが観察されました。そしてTRAINとTESTに共存在していることがわかった。
# 一応Address情報が存在しているようで、それでX, Yを予測してみることにします。

# table(COMBI$Address[COMBI$Y > 89])
# 基本的に全部街角っぽいですので、/で片方の街名を取得し、他のデータからのXとYを当てはめてみる

wierdY_idx <- COMBI$Y == 90
corner_idx <- COMBI$Y != 90 & COMBI$Iscorner == TRUE
COMBI$Addr_tmp[COMBI$Iscorner == TRUE] <- sapply(COMBI$Address[COMBI$Iscorner == TRUE], FUN=function(x) {strsplit(x, split=" / ")[[1]][2]})
ave_XY <- COMBI[corner_idx,] %>% group_by(Addr_tmp) %>% summarize(X_ave=mean(X),Y_ave=mean(Y))
COMBI <- COMBI %>% left_join(ave_XY,by="Addr_tmp")
COMBI$X[wierdY_idx] <- COMBI$X_ave[wierdY_idx]
COMBI$Y[wierdY_idx] <- COMBI$Y_ave[wierdY_idx]
# summary(COMBI)
# どうしてもNAが5個残るらしい

COMBI <- COMBI[,c(-15,-16,-17)]
COMBI$Addr_tmp[COMBI$Iscorner == TRUE] <- sapply(COMBI$Address[COMBI$Iscorner == TRUE], FUN=function(x) {strsplit(x, split=" / ")[[1]][1]})
ave_XY <- COMBI[corner_idx,] %>% group_by(Addr_tmp) %>% summarize(X_ave=mean(X),Y_ave=mean(Y))
COMBI <- COMBI %>% left_join(ave_XY,by="Addr_tmp")
COMBI$X[is.na(COMBI$X)] <- COMBI$X_ave[is.na(COMBI$X)]
COMBI$Y[is.na(COMBI$Y)] <- COMBI$Y_ave[is.na(COMBI$Y)]

# smoothScatter(COMBI$X,COMBI$Y)
# 修正されたことが確認できた
COMBI <- COMBI[,c(-15,-16,-17)] #Addr_tmp, X_ave, Y_aveを消す

#XとYを使って、5*5のグリッドを作成する。グリッド自体は当分ではなく、quantileで切る。左から右へ、上から下へ番付をする.
x_perc <- quantile(COMBI$X,c(0.1,0.35,0.6,0.9))
y_perc <- quantile(COMBI$Y,c(0.1,0.35,0.6,0.9))
# smoothScatter(COMBI$X,COMBI$Y)
# abline(v=x_perc,h=y_perc)
COMBI$X_grid <- sapply(COMBI$X, FUN=function(x) {sum(x >= x_perc * 1) + 1})
COMBI$Y_grid <- sapply(COMBI$Y, FUN=function(x) {sum(x >= y_perc * 1) + 1})
COMBI$Grid_no <- (COMBI$Y_grid - 1) * 5 + COMBI$X_grid
 
# 準備が大体できたので、DatesとAddressを落として、factorに変換すべきものを変換して、TRAINとTESTを復元する
COMBI <- COMBI[,c(-1,-5,-15,-16)]
COMBI$Grid_no <- as.factor(COMBI$Grid_no)
COMBI$Category <- as.factor(COMBI$Category)
COMBI$DayOfWeek <- as.factor(COMBI$DayOfWeek)
COMBI$PdDistrict <- as.factor(COMBI$PdDistrict)
COMBI$Hour <- as.factor(COMBI$Hour)
COMBI$Year <- as.factor(COMBI$Year)
COMBI$Month <- as.factor(COMBI$Month)
TRAIN <- COMBI[is.na(COMBI$Id),]
TEST <- COMBI[!is.na(COMBI$Id),]

###Feature処理完了。以下はCV####

# accuracyを評価する関数を作成
accuracy.rate <- function(table){
  sum(table[row(table)==col(table)])/sum(table)
  }

##loglossの評価関数を作成
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

# TRAIN_NO <- createDataPartition(TRAIN$Category, p=0.5,list=F)
# TRAIN_TRAIN <- TRAIN[TRAIN_NO,]
# TRAIN_TEST <- TRAIN[-TRAIN_NO,]

# fit <- train(Category ~ DayOfWeek + Year + Month + Isholiday + Isbeforeholiday + Hour + Iscorner + Grid_no + X +Y, data = TRAIN_TRAIN, method="rpart")
# print(fit$result)

# cv_pred <- predict(fit,newdata=TRAIN_TEST,type="raw")
# accuracy.rate(table(cv_pred,TRAIN_TEST$Category))
# result <- predict(fit,newdata=TRAIN_TEST,type="prob")
# cv_pred <- data.frame(Id=1:nrow(result),result)
# colnames(cv_pred)[-1]  <- colnames(result)
# cv_result <- data.frame(Id=1:length(TRAIN_TEST$Category),Category=TRAIN_TEST$Category)
# log.loss(cv_pred,cv_result)

#とりあえず提出ような結果を作ってみる
# fit <- rpart(Category ~ DayOfWeek + Year + Month + Isholiday + Isbeforeh# oliday + Hour + Iscorner + Grid_no + X + Y, data = TRAIN, method="class",control=rpart.control(cp=0.001))
# predict_result <- predict(fit,newdata=TEST)
# result <- data.frame(Id=TEST$Id,predict_result)
# colnames(result)[-1] <- colnames(predict_result)

# rpart.plot(fit)

# write.csv(result,"./sf-crime/result.csv",na="0",row.names=FALSE,quote=F)