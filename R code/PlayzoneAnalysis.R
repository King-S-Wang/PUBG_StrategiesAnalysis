library(tidyverse)
library(parallel)
library(data.table)
library(dbplyr)
library(ggplot2)
# remove outliers 0,0

# set working directory to the current file location and read the csv file
pubg_data <- read.csv("PUBG_Data1_1.csv")
#summary(pubg_data)

# create a data frame
df_pubg <- data.frame(pubg_data$match_id,
                      pubg_data$killer_position_x,pubg_data$killer_position_y,
                      pubg_data$victim_position_x,pubg_data$victim_position_y,
                      pubg_data$time)

# convert gaming time to circle categories and create a new column
df_pubg <- mutate(df_pubg, circle = case_when(
  pubg_data.time <= (12*60) ~ 1,
  pubg_data.time <= (17*60+40) ~ 2,
  pubg_data.time <= (21*60+40) ~ 3,
  pubg_data.time <= (24*60+40) ~ 4,
  pubg_data.time <= (27*60+20) ~ 5,
  pubg_data.time <= (29*60+20) ~ 6,
  pubg_data.time <= (31*60+20) ~ 7,
  pubg_data.time <= (32*60+50) | pubg_data.time > (32*60+50) ~ 8
  
))

# count for unique matches
df_uniqMatch <- df_pubg %>% count(pubg_data$match_id)


# filter out circle 8 data
df_circle_8<- df_pubg %>% filter(circle==8)

# Method 1
# try kde2d function (kernel density) to build heatmap of the distribution of final blue zone
library(MASS)
circle_8_density <-kde2d(df_circle_8$pubg_data.killer_position_x,
                         df_circle_8$pubg_data.killer_position_y,
                         n=1000,
                         lims = c(range(df_circle_8$pubg_data.killer_position_x),
                                      rev(range(df_circle_8$pubg_data.killer_position_y)))
)

# define the color scale before ploting the heatmap
hm_col_scale<-colorRampPalette(c("black","blue","green","orange","red"))(1000)

# plot heatmap using image functin (matrixes)
image(circle_8_density$z,  
      col = hm_col_scale,
      zlim=c(min(circle_8_density$z), max(circle_8_density$z)))

# Method 2 <- Selected 
# plot heatmap with pubg ERANGEL map background
# import pubg map: ERANGEL to R
library(imager)
map <- load.image('ERANGEL.jpg')
#plot(map)

#Avoid auto science number i.e. e+....
options(scipen=999)
ggplot(df_circle_8, aes(pubg_data.killer_position_x,
                        pubg_data.killer_position_y))  + 
  annotation_raster(map, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  stat_density2d(geom = "polygon", aes(fill=..level..)) + 
  #geom_point(size=0.2)+
  scale_fill_gradient(low="transparent",high="red")+
  scale_y_reverse()+
  coord_fixed()+
  ggtitle("The Distribution of Final Play Zone", subtitle="Circle 8")+
  xlab("position_x") + ylab("position_y")

