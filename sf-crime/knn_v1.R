#This is a prediction implementing knn method. 
#The predicted probability of each category of the test set is given by 
#the proportion of category in the knn where k=100, 
#which the distance is calculated using unified x,y and time. 

#kNNの練習。座標のx,yと時間を全部均一化した上で、100の近隣データをとって、カテゴリー別の分数を計算している。


library(readr)
library(dplyr)
library(tidyr)
library(FNN)

k_for_knn = 100
knn_method = "kd_tree"

#train <- read_csv("./sf_crime/train.csv.zip")
train <- read_csv("./sf_crime/train_spl.csv.zip")
#test <- read_csv("./sf_crime/test.csv.zip")
test <- read_csv("./sf_crime/test_spl.csv.zip")


max_axis <- train %>% 
	mutate(time=as.numeric(as.POSIXct(Dates))) %>%
	summarize(max_time=max(time),max_x=max(X),max_y=max(Y))
min_axis <- train %>% 
	mutate(time=as.numeric(as.POSIXct(Dates))) %>%
	summarize(min_time=min(time),min_x=min(X),min_y=min(Y))
max_axis <- as.vector(as.matrix(max_axis))
min_axis <- as.vector(as.matrix(min_axis))
diff_axis <- max_axis - min_axis

train_data <- train %>% 
	mutate(time=(as.numeric(as.POSIXct(Dates)) - min_axis[1]) / diff_axis[1] * 100,x = (X-min_axis[2])/diff_axis[2] * 100, y = (Y - min_axis[3])/diff_axis[3] * 100) %>%
	select(time,x,y,Category)

train_coordinate <- train_data[,1:3]
train_label <- train_data[,4]

test_coordinate <- test %>%
	mutate(time=(as.numeric(as.POSIXct(Dates)) - min_axis[1]) / diff_axis[1] * 100,x = (X-min_axis[2])/diff_axis[2] * 100, y = (Y - min_axis[3])/diff_axis[3] * 100) %>%
	select(time,x,y)

test_id <- test %>% 
	mutate(index=rownames(test)) %>% 
	select(index,Id)
test_id$index <- as.numeric(test_id$index)

knn_index <- knnx.index(train_coordinate,test_coordinate,k=k_for_knn,algo= knn_method)
knn_index <- data.frame(knn_index)
tmp_df <- stack(knn_index)

tmp_df2 <- data.frame(value=tmp_df$values,k=tmp_df$ind,
                index=factor(rownames(knn_index))[row(knn_index)])
tmp_df2$index <- as.numeric(tmp_df2$index)
rm(list=c("knn_index","tmp_df"))


result <- tmp_df2 %>%
	mutate(Category = as.matrix(train_label)[tmp_df2$value]) %>%
	group_by(index,Category) %>%
	summarize(counts=n()) %>%
	ungroup() %>%
	mutate(Rate = counts / k_for_knn) %>%
	inner_join(test_id,by="index") %>%
	select(Id,Category,Rate) %>%
	spread(Category,Rate,fill=0)

write.table(result,"./sf_crime/result.csv",sep=",",append=FALSE,na="0",dec=".",quote=FALSE,row.names=FALSE)

rm(list=ls())