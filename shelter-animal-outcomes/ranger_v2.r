setwd("/users/hao.jiang/playground/kaggle_pjt/shelter-animal-outcomes")

Sys.setlocale("LC_TIME", "en_US.UTF-8")
# load librarys
library(readr)
library(dplyr)
library(reshape2)
library(lubridate)	#datetime data process
library(chron)		#datetime data process
library(splitstackshape)
library(tidyr)
library(rpart)
library(caret)
library(ranger)

# load data
train <- read.csv("./train.csv.gz")
test <- read.csv("./test.csv.gz")

# look at the data
summary(train)
summary(test)

# do some feature extracting, combine train test set first
test$OutcomeType <- NA
test$OutcomeSubtype <- NA
train$ID <- NA
combi <- rbind(train[,-1],test)

# extract feature from DateTime
combi$Year <- year(combi$DateTime)
combi$Month <- month(combi$DateTime)
combi$Dayofweek <- weekdays(as.Date(combi$DateTime))
combi$Isholiday <- is.holiday(combi$DateTime)	#this one's from chron library

# other feature extraction
# about name
combi$Hasname <- combi$Name == ""	#flag for whether or not the pool creature has a name

# about sex and if neutered
combi$SexuponOutcome <- as.character(combi$SexuponOutcome)
combi$SexuponOutcome[combi$SexuponOutcome == ""] <- "Unknown"
combi$Isneutered <- sapply(combi$SexuponOutcome,FUN=function(x) {strsplit(x,split=" ")[[1]][1]})
combi$Isneutered[combi$Isneutered=="Spayed"] <- "Neutered"
combi$Sex <- sapply(combi$SexuponOutcome,FUN=function(x) {strsplit(x,split=" ")[[1]][2]})
combi$Sex[is.na(combi$Sex)] <- "Unknown"

#breed & color info
combi$Ismixbreed <- ifelse(regexpr("Mix",combi$Breed) > 0,"general breed",
					ifelse(regexpr("/",combi$Breed) > 0, "definite breed","pure"))	#flag for mixed breed
# combi$Breed <- as.character(combi$Breed)
# allbreeds <- gsub(" Mix","",combi$Breed)
# allbreeds <- sapply(allbreeds,FUN=function(x) {unlist(strsplit(x,split="/"))})
# allbreeds <- unique(unlist(allbreeds))
		
# color info
combi$Color <- as.character(combi$Color)
combi$Ismixcolor <- regexpr("/",combi$Color) > 0		#flag for mixed color

# first let's see how many features are there in colors
# color_feature <- sapply(combi$Color,FUN=function(x) {unlist(strsplit(x,split="[ /]"))})
# color_feature <- unique(unlist(color_feature))
# print(color_feature) #some features are color info , while some are patterns info
#as for color info, we have 23 features, inclueding Brown,White,Cream,Blue,Tan,Black,Red,Silver,Orange,Chocolate,Yello,Gray,Buff,Seal,Fawn,Sable,Liver,Apricot,Lilac,Gold,Pink,Ruddy,Tricolor
#as for patterns, we have 13 features inclueding Tabby,Brindle,Calico,Torbie,Tortie,Point,Flame,Tick,Merle,Tiger,Lynx,Smoke,Agouti

#we could try to split all color features individually 
combi <- cSplit(combi, "Color", sep=" ", direction="long")
combi <- cSplit(combi, "Color", sep="/", direction="long")
combi$val <- 1
combi <- spread(unique(combi),Color,val,fill=0)


# about age
combi$AgeuponOutcome <- as.character(combi$AgeuponOutcome)
combi$age_val <- sapply(combi$AgeuponOutcome,FUN=function(x) {strsplit(x,split=" ")}[[1]][1])
combi$age_val <- as.numeric(combi$age_val)
age_unit_val <- ifelse(regexpr('day',combi$AgeuponOutcome) > 0, 1/30,
                    ifelse(regexpr('week',combi$AgeuponOutcome) > 0, 1/4,
                    ifelse(regexpr('month',combi$AgeuponOutcome) > 0, 1, 
                    ifelse(regexpr('year',combi$AgeuponOutcome) > 0, 12, NA))))
combi$age_val <- combi$age_val * age_unit_val
age_fit <- rpart(age_val ~ AnimalType + Sex + Isneutered + Breed + +Ismixbreed + Ismixcolor + Hasname, data = combi[!is.na(combi$age_val),], method = 'anova')
combi$age_val[is.na(combi$age_val)] <- predict(age_fit, newdata=combi[is.na(combi$age_val),])

#life_stages seems to vary between dogs and cats
combi$life_stage <- ifelse(combi$age_val <=1 , "newly_born",
								ifelse(combi$age_val <=6 , "baby",
								ifelse(combi$age_val <=24 , "junior",
								ifelse(combi$age_val <=72 , "prime",
								ifelse(combi$age_val <=120 , "mature","senior")))))
combi <- data.frame(combi)

#combi2 <- subset(combi,select=-c(DateTime, OutcomeSubtype, SexuponOutcome, AgeuponOutcome))
combi2 <- combi[, - which(names(combi) %in% c("DateTime", "OutcomeSubtype", "SexuponOutcome", "AgeuponOutcome"))]


# split combi back to train test set
train <- combi2[!is.na(combi2$OutcomeType),]
train <- train[,-which(names(train) %in% c("ID"))]
test <- combi2[is.na(combi2$OutcomeType),]

#let's do a performance check
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

train_no <- createDataPartition(train$OutcomeType,p=0.7,list=F)
train_train <- train[train_no,]
train_test <- train[-train_no,]

# fit <- ranger(OutcomeType~., data=train_train, write.forest=TRUE, probability=TRUE,importance="impurity",num.trees=2000,mtry=10)
# cv_pred <- predict(fit,data=train_test)
# cv_pred_check <- data.frame(Id=1:nrow(cv_pred$predictions), cv_pred$predictions)
# cv_result <- data.frame(Id=1:nrow(train_test),Category=train_test$OutcomeType)
# log.loss(cv_pred_check,cv_result)
#mtry= 10 -> 0.8485
#mtry= 7 -> 0.8563541
#mtry= 5 -> 0.8716
#mtry= 3 -> 0.928

library(party)
set.seed(415)
fit <- cforest(OutcomeType ~., data = train_train, controls=cforest_unbiased(ntree=500, mtry=7))


# write.csv(result,"./result.csv",row.names=F,quote=F)