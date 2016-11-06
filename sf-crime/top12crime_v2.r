setwd("/Users/hao.jiang/Playground/Kaggle_pjt")

# Multiplot the occurences of the top 12 crimes in San Francisco.
# Hints mostly from Ben Hamner's San Francisco Top Crimes Map(url:https://www.kaggle.com/benhamner/sf-crime/san-francisco-top-crimes-map)

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)

train <- read_csv("./sf-crime/train.csv.zip")
map <- readRDS("./sf-crime/sf_map_copyright_openstreetmap_contributors.rds")
#train <- read_csv("../input/train.csv.zip")
#map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")

counts <- summarise(group_by(train, Category), Counts=length(Category)) %>% filter(Category != "OTHER OFFENSES")
counts <- counts[order(-counts$Counts),]
# This removes the "Other Offenses" category
top12 <- train[train$Category %in% counts$Category[c(1:12)],]

p <- ggmap(map) + 
geom_point(data= top12, aes(x=X, y=Y), alpha=0.05, size =0.01, show.legend=FALSE) + 
theme_light(base_size=1) + 
ggtitle("Top 12 Crimes in San Francisco") +
facet_wrap(~Category,ncol=4) +
theme(plot.title = element_text(size=12, face="bold"),strip.text.x = element_text(size=8))

#print(p)
ggsave("./sf-crime/sf_top_crimes_map.png", p, units="cm")