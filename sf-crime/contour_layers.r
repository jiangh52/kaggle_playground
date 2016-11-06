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

p <- ggmap(map) + 
geom_density2d(data= train, aes(x=X, y=Y), size = 0.2) +
stat_density2d(data = train, aes(x = X, y = Y,  fill = ..level.., alpha = ..level..),size = 0.001, geom = 'polygon', inherit.aes = TRUE) +
scale_fill_gradient(high = "red") +
scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
ggtitle("Crimes density in San Francisco") +
facet_wrap(~Category,ncol=7) +
theme(axis.text=element_blank(),axis.line=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(),legend.position = "none", plot.title = element_text(size=12, face="bold"),strip.text.x = element_text(size=3), panel.margin=unit(0.005,"lines"))

ggsave("./sf-crime/sf_crimes_density_plot.png", p, units="cm")