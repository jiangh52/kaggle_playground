setwd("/users/hao.jiang/playground/kaggle_pjt/shelter-animal-outcomes")

Sys.setlocale("LC_TIME", "en_US.UTF-8")
# load librarys
library(readr)
library(dplyr)
library(reshape2)
library(splitstackshape)
library(tidyr)
library(ggplot2)

# load data
train <- read.csv("./train.csv.gz")
train$Color <- as.character(train$Color)


# first let's see how many features are there in colors
# color_feature <- sapply(train$Color,FUN=function(x) {unlist(strsplit(x,split="[ /]"))})
# color_feature <- unique(unlist(color_feature))
# print(color_feature) #some features are color info , while some are patterns info
#as for color info, we have 23 features, inclueding Brown,White,Cream,Blue,Tan,Black,Red,Silver,Orange,Chocolate,Yello,Gray,Buff,Seal,Fawn,Sable,Liver,Apricot,Lilac,Gold,Pink,Ruddy,Tricolor
#as for patterns, we have 13 features inclueding Tabby,Brindle,Calico,Torbie,Tortie,Point,Flame,Tick,Merle,Tiger,Lynx,Smoke,Agouti

#we could try to split all color features individually 
colors <- c("Brown","White","Cream","Blue","Tan","Black","Red","Silver","Orange","Chocolate","Yello","Gray","Buff","Seal","Fawn","Sable","Liver","Apricot","Lilac","Gold","Pink","Ruddy","Tricolor")
patterns <- c("Tabby","Brindle","Calico","Torbie","Tortie","Point","Flame","Tick","Merle","Tiger","Lynx","Smoke","Agouti")

test <- train[,c("AnimalID","Color","OutcomeType")]
test$Color1 <- sapply(test$Color,FUN=function(x) {strsplit(x,split="[ /]")[[1]][1]})
test$Color2 <- sapply(test$Color,FUN=function(x) {strsplit(x,split="[ /]")[[1]][2]})
test$Color2[is.na(test$Color2)] <- test$Color1[is.na(test$Color2)]

group_ttl <- group_by(test,Color1,Color2) %>% summarize(AllCounts=n()) %>% ungroup()

test1 <- test %>%
	group_by(Color1,Color2, OutcomeType) %>%
	summarize(Counts=n()) %>%
	left_join(group_ttl,by=c("Color1"="Color1","Color2"="Color2")) %>%
	mutate(Rate=Counts/AllCounts)

ggplot(test1,aes(Color1,Color2)) + geom_bin2d(aes(fill=Rate)) + facet_grid(OutcomeType~.)
