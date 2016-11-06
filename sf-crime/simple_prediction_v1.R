#Very simple method to predict category of crime based on the ratio of each categories in the same PdDistrict on the same week of day and the same year.

library(readr)
library(dplyr)
library(tidyr)

#train <- read_csv("/Users/hao.jiang/Mystation/Kaggle_pjt/sf_crime/train.csv.zip")
train <- read_csv("/Users/hao.jiang/Mystation/Kaggle_pjt/sf_crime/train_spl.csv.zip")
#test <- read_csv("/Users/hao.jiang/Mystation/Kaggle_pjt/sf_crime/test.csv.zip")
test <- read_csv("/Users/hao.jiang/Mystation/Kaggle_pjt/sf_crime/test_spl.csv.zip")

base_count <- train %>% 
	mutate(Year=format(Dates,"%Y")) %>%
	group_by(Year,DayOfWeek,PdDistrict,Category) %>%
	summarise(Counts=n())

category_counts <- base_count %>%
	group_by(Year,DayOfWeek,PdDistrict) %>%
	summarise(Crimesum = sum(Counts))
	
category_result <- inner_join(base_count,category_counts,by=c("Year","DayOfWeek","PdDistrict")) %>%
	mutate(Rate=Counts/Crimesum,id2=paste(Year,DayOfWeek,PdDistrict,sep="-")) %>%
	ungroup() %>% 
	select(id2,Category,Rate) %>%
	spread(Category,Rate,fill=0)
	
test_rearrange <- test %>%
	mutate(id2=paste(format(Dates,"%Y"),DayOfWeek,PdDistrict,sep="-")) %>%
	select(id2,Id)

result <- left_join(test_rearrange,category_result,by="id2")
result <- result[2:41]

write.table(result,"/Users/hao.jiang/Mystation/Kaggle_pjt/sf_crime/result.csv",sep=",",append=FALSE,na="0",dec=".",quote=FALSE,row.names=FALSE)