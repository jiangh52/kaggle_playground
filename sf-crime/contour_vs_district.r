setwd("/Users/hao.jiang/Playground/Kaggle_pjt")

# Plot sf-crime density with pddistrict 

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)

train <- read_csv("./sf-crime/train.csv.zip")
map <- readRDS("./sf-crime/sf_map_copyright_openstreetmap_contributors.rds")
#train <- read_csv("../input/train.csv.zip")
#map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")
train_sample <- train[sample(nrow(train),20000,replace=FALSE),]
district_map <- train[sample(nrow(train),100000,replace=FALSE),c("X","Y","PdDistrict")]

p <- ggmap(map) + 
geom_point(data= district_map,aes(x=X, y=Y, color=factor(PdDistrict)),size=0.5) +
guides(colour = guide_legend(override.aes = list(alpha=1, size=6.0),title="Police District")) +
scale_colour_brewer(type="qual",palette="Set3") +
geom_density2d(data= train_sample, aes(x=X, y=Y), size = 0.2, color = "black") +
ggtitle("Crimes density in San Francisco") +
facet_wrap(~Category,ncol=7) +
theme(axis.text=element_blank(),axis.line=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(),plot.title = element_text(size=12, face="bold"),strip.text.x = element_text(size=3), panel.margin=unit(0.005,"lines"))

ggsave("./sf-crime/sf_crimes_density_plot_wi_district.png", p, units="cm")