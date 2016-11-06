# Multiplot the occurences of the top 12 crimes in San Francisco.
# Hints mostly from Ben Hamner's San Francisco Top Crimes Map(url:https://www.kaggle.com/benhamner/sf-crime/san-francisco-top-crimes-map)

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)
library(grid)
library(gridExtra)

train <- read_csv("./sf_crime/train.csv.zip")
map <- get_map(location="sanfrancisco",zoom=12,source="osm",color="bw")
#train <- read_csv("../input/train.csv.zip")
#map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")

counts <- summarise(group_by(train, Category), Counts=length(Category)) %>% filter(Category != "OTHER OFFENSES")
counts <- counts[order(-counts$Counts),]
# This removes the "Other Offenses" category
top12 <- train[train$Category %in% counts$Category[c(1:12)],]
top12_sample <- top12[sample(nrow(top12),20000,replace=FALSE),]


graph=list()

for (i in c(1:12)) {
use_data <- top12_sample[top12_sample$Category %in% counts$Category[i],]

p <- ggmap(map) + 
geom_point(data=use_data, aes(x=X, y=Y, color=factor(Category)), alpha=0.05, size =0.01, show.legend=FALSE) + 
ggtitle(counts$Category[i]) + 
theme_light(base_size=1) + 
theme(plot.title = element_text(size=8, face="bold"))
graph = c(graph,list(p))
}

final_p <- arrangeGrob(grobs=graph,ncol=4,top="Top 12 Crimes in San Francisco")
plot(final_p)
#ggsave("sf_top_crimes_map.png", final_p, units="cm")