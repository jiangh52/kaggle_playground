#Try to decide and plot the boundary of PdDistrict using kNN

library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(FNN)


#Loading data
train <- read_csv("./sf-crime/train.csv.zip")
map <- readRDS("./sf-crime/sf_map_copyright_openstreetmap_contributors.rds")
train <- train[train$Y!=90,]
train_sample <- train[sample(nrow(train),50000,replace=FALSE),]

district_map <- expand.grid(X = seq(-122.5140,-122.3640,0.001),Y = seq(37.7070,37.82,0.001))

knn_district <- knn(train_sample[,c("X","Y")],district_map,as.matrix(train_sample[,"PdDistrict"]),k=200)

disc_ref <- data.frame(id=rank(levels(knn_district)),disc_level=levels(knn_district))
disc_df <- data.frame(district_map,knn_district)
disc_df <- left_join(disc_df, disc_ref, by=c("knn_district"="disc_level"))
q <- ggmap(map) + geom_contour(data=disc_df,aes(x=X,y=Y,z=id))
print(q)


