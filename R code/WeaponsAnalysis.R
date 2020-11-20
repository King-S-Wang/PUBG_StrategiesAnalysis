# weapon analysis
# install and load packages
packages = c("magrittr","lubridate","reshape","tidyverse")
for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

library(ggplot2)
library(dplyr)
library(formattable)
library("ggpubr")

# read data
my_data_orig <- read.csv("PUBG_Data1_1.csv")
View(my_data_orig)
summary(my_data_orig)
not_weapon <- c("Down and Out","death.Buff_FireDOT_C","death.PlayerMale_A_C",
                "death.ProjMolotov_C","death.ProjMolotov_DamageField_C","RedZone",
                "Falling","Hit by Car","Motorbike","Motorbike (SideCar)","Uaz",
                "Aquarail","Bluezone","Boat","Buggy","Dacia","Drown","Punch")
my_data <- subset(my_data_orig,!(killed_by %in% not_weapon),select = c(1,2,3,7,8))

# check data details
View(my_data)
summary(my_data)
hist(my_data$time,
     main = "histogram of kills in different time",
     xlab = "time",
     ylab = "kills")
hist(my_data$killer_placement,
     main = "histogram of kill placement",
     xlab = "placement",
     ylab = "kills")

# explore kills in every match
my_data$count <- 1
kem_sum <- aggregate(x=my_data$count,by = list(my_data$match_id),FUN=sum)
colnames(kem_sum) <- c("match_id","kill_sum")
summary(kem_sum)
hist(kem_sum$kill_sum,
     main = "histogram of kills every match",
     xlab = "kills",
     ylab = "count")

# define time intervals of 8 circles
circle <- c(1,2,3,4,5,6,7,8)
start_time <- c(0,720,1060,1300,1480,1640,1760,1880,1970)
stage <- c(0,1300,1760)

# define 3 stages: prophase, metaphase and anaphase
kill_pro <- subset(my_data,time<stage[2],select = c(1,4,5,6))
kill_meta <- subset(my_data,time>=stage[2] & time<stage[3],select = c(1,4,5,6))
kill_ana <- subset(my_data,time>=stage[3],select = c(1,4,5,6))

range(kill_pro$time)
range(kill_meta$time)
range(kill_ana$time)

# compute kill count of every kind of weapon in different stages
kam_pro <- aggregate(x = kill_pro$count, by = list(kill_pro$killed_by), FUN = sum)
colnames(kam_pro) <- c("weapon_name","pro_kill_sum")
kam_pro$pro_kill_mean <- round(kam_pro$pro_kill_sum/118838,4)

kam_meta <- aggregate(x = kill_meta$count, by = list(kill_meta$killed_by), FUN = sum)
colnames(kam_meta) <- c("weapon_name","meta_kill_sum")
kam_meta$meta_kill_mean <- round(kam_meta$meta_kill_sum/118838,4)

kam_ana <- aggregate(x = kill_ana$count, by = list(kill_ana$killed_by), FUN = sum)
colnames(kam_ana) <- c("weapon_name","ana_kill_sum")
kam_ana$ana_kill_mean <- round(kam_ana$ana_kill_sum/118838,4)

# form new tables contain all data of three stages
kam_ft <- full_join(kam_pro[,c(1,3)],kam_meta[,c(1,3)],by = c("weapon_name"="weapon_name"))
kam_ft <- full_join(kam_ft,kam_ana[,c(1,3)],by = c("weapon_name"="weapon_name"))
kam_ft[is.na(kam_ft)] <- 0
kam_ft$total_kill_mean <- kam_ft$pro_kill_mean+kam_ft$meta_kill_mean+kam_ft$ana_kill_mean

# create percentage columns for kill all matches
kam_ft$pro_kill_per <- round(kam_ft$pro_kill_mean*100/sum(kam_ft$pro_kill_mean),4)
kam_ft$meta_kill_per <- round(kam_ft$meta_kill_mean*100/sum(kam_ft$meta_kill_mean),4)
kam_ft$ana_kill_per <- round(kam_ft$ana_kill_mean*100/sum(kam_ft$ana_kill_mean),4)
kam_ft$total_kill_per <- round(kam_ft$total_kill_mean*100/sum(kam_ft$total_kill_mean),4)

# visualization
top5kill_pro <- subset(kam_ft,kam_ft$weapon_name %in% kam_ft[order(kam_ft$pro_kill_per,
                                                             decreasing = T),][1:5,1],select = c(1,6))
ggplot(top5kill_pro,aes(x = reorder(factor(weapon_name),pro_kill_per,sum),y=pro_kill_per))+
  geom_bar(stat = "identity",fill = c("dark gray","light blue","light green","light yellow","pink"),color = "black")+
  labs(title = "top 5 kills weapon in prophase",
       x = "weapon name",
       y = "percentage")
top5kill_pro <- arrange(top5kill_pro,desc(pro_kill_per))
top5kill_pro$No <- c(1,2,3,4,5)
top5kill <- full_join(data.frame(No=c(1,2,3,4,5)),top5kill_pro[,c(3,1,2)],by = c("No"="No"))

top5kill_meta <- subset(kam_ft,kam_ft$weapon_name %in% kam_ft[order(kam_ft$meta_kill_per,
                                                            decreasing = T),][1:5,1],select = c(1,7))
ggplot(top5kill_meta,aes(x = reorder(factor(weapon_name),meta_kill_per,sum),y=meta_kill_per))+
  geom_bar(stat = "identity",fill = c("light blue","light yellow","dark gray","light green","white"),color = "black")+
  labs(title = "top 5 kills weapon in metaphase",
       x = "weapon name",
       y = "percentage")
top5kill_meta <- arrange(top5kill_meta,desc(meta_kill_per))
top5kill_meta$No <- c(1,2,3,4,5)
top5kill <- full_join(top5kill,top5kill_meta[,c(3,1,2)],by = c("No"="No"))

top5kill_ana <- subset(kam_ft,kam_ft$weapon_name %in% kam_ft[order(kam_ft$ana_kill_per,
                                                                    decreasing = T),][1:5,1],select = c(1,8))
ggplot(top5kill_ana,aes(x = reorder(factor(weapon_name),ana_kill_per,sum),y=ana_kill_per))+
  geom_bar(stat = "identity",fill = c("light blue","light yellow","dark gray","light green","white"),color = "black")+
  labs(title = "top 5 kills weapon in anaphase",
       x = "weapon name",
       y = "percentage")
top5kill_ana <- arrange(top5kill_ana,desc(ana_kill_per))
top5kill_ana$No <- c(1,2,3,4,5)
top5kill <- full_join(top5kill,top5kill_ana[,c(3,1,2)],by = c("No"="No"))

top5kill_total <- subset(kam_ft,kam_ft$weapon_name %in% kam_ft[order(kam_ft$total_kill_per,
                                                                   decreasing = T),][1:5,1],select = c(1,9))
ggplot(top5kill_total,aes(x = reorder(factor(weapon_name),total_kill_per,sum),y=total_kill_per))+
  geom_bar(stat = "identity",fill = c("light blue","dark gray","light yellow","light green","pink"),color = "black")+
  labs(title = "top 5 kills weapon in whole game",
       x = "weapon name",
       y = "percentage")
top5kill_total <- arrange(top5kill_total,desc(total_kill_per))
top5kill_total$No <- c(1,2,3,4,5)
top5kill <- full_join(top5kill,top5kill_total[,c(3,1,2)],by = c("No"="No"))
colnames(top5kill) <- c("No","pro_w","pro_mk","meta_w","meta_mk","ana_w","ana_mk","total_w","total_mk")

plotdata <- subset(kam_ft,select = c(1,6,7,8))
plotdata <- subset(kam_ft,kam_ft$total_kill_per>3,select = c(1,6,7,8))
plotdata <- subset(kam_ft,kam_ft$total_kill_per<0.2,select = c(1,6,7,8))
plotdata <- melt(plotdata,variable_name = "stages")

ggplot(plotdata,aes(x=reorder(factor(plotdata$weapon_name),plotdata$value,sum),y=value,fill=stages))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "percentage of each weapon",
       x = "weapon name",
       y = "percentage in total")

# top kills weapons and kill every match
kem_pro <- aggregate(x = kill_pro$count, by = list(kill_pro$killed_by,kill_pro$match_id), FUN = sum)
colnames(kem_pro) <- c("weapon_name","match_id","pro_kill_sum")
kem_meta <- aggregate(x = kill_meta$count, by = list(kill_meta$killed_by,kill_meta$match_id), FUN = sum)
colnames(kem_meta) <- c("weapon_name","match_id","meta_kill_sum")
kem_ana <- aggregate(x = kill_ana$count, by = list(kill_ana$killed_by,kill_ana$match_id), FUN = sum)
colnames(kem_ana) <- c("weapon_name","match_id","ana_kill_sum")

# kem <- aggregate(x = my_data$count, by = list(my_data$killed_by,my_data$match_id), FUN = sum)
# colnames(kem_ana) <- c("weapon_name","match_id","ana_kill_sum")

kem_ft <- full_join(kem_pro,kem_meta,by = c("weapon_name"="weapon_name","match_id"="match_id"))
kem_ft <- full_join(kem_ft,kem_ana,by = c("weapon_name"="weapon_name","match_id"="match_id"))
kem_ft[is.na(kem_ft)] <- 0
kem_ft$total_kill_sum <- kem_ft$pro_kill_sum+kem_ft$meta_kill_sum+kem_ft$ana_kill_sum

# top 3 weapons
kem_M416 <- subset(kem_ft,kem_ft$weapon_name == "M416")
kem_SCARL <- subset(kem_ft,kem_ft$weapon_name == "SCAR-L")
kem_M16A4 <- subset(kem_ft,kem_ft$weapon_name == "M16A4")
kem3weapons <- full_join(kem_sum,kem_M416[,c(2,6)],by=c("match_id"="match_id"))
kem3weapons <- full_join(kem3weapons,kem_SCARL[,c(2,6)],by=c("match_id"="match_id"))
kem3weapons <- full_join(kem3weapons,kem_M16A4[,c(2,6)],by=c("match_id"="match_id"))
colnames(kem3weapons) <- c("match_id","total","M416","SCARL","M16A4")
kem3weapons[is.na(kem3weapons)] <- 0

# visualization
plotdata2 <- kem3weapons[,c(1,3,4,5)]
plotdata2 <- melt(plotdata2,variable_name = "weapons")

ggplot(plotdata2,aes(x=weapons,y=value))+
  geom_boxplot()+
  labs(title = "Boxplot of 3 weapons in  different matches",
       x = "weapon name",
       y = "kills")

# inferential analysis
install.packages("Rmisc")
library(Rmisc)

CI(kem3weapons$M416)
CI(kem3weapons$SCARL)
CI(kem3weapons$M16A4)

# ANOVA
res.aov <- aov(plotdata2$value ~ plotdata2$weapons, data = plotdata2)
summary(res.aov)
TukeyHSD(res.aov)
plot(res.aov,2)

#Kruskal-Wallis test
kruskal.test(plotdata2$value ~ plotdata2$weapons, data = plotdata2)

#Multiple pairwise-comparisaon between groups
pairwise.wilcox.test(plotdata2$value,plotdata2$weapons,
                     p.adjust.method = "BH")

# t.test(kem3weapons$M416,kem3weapons$M16A4,alternative = "two.sided",conf.level = 0.95)
# t.test(kem3weapons$SCARL,kem3weapons$M16A4,alternative = "two.sided",conf.level = 0.95)

# weapon and placement analysis
place_whole <- aggregate(x = my_data$killer_placement,by = list(my_data$killed_by,my_data$killer_name,my_data$match_id),FUN = mean)
colnames(place_whole) <- c("weapon_name","killer_name","match_id","placement")
place_M416 <- subset(place_whole,weapon_name == "M416")
hist(place_M416$placement,
     main = "histogram of placements of M416 users",
     xlab = "placement",
     ylab = "Frequency")

place_P92 <- subset(place_whole,weapon_name == "P92")
hist(place_P92$placement,
     main = "histogram of placements of P92 users",
     xlab = "placement",
     ylab = "Frequency")

place_S1897 <- subset(place_whole,weapon_name == "S1897")
hist(place_S1897$placement,
     main = "histogram of placements of S1897 users",
     xlab = "placement",
     ylab = "Frequency")

place_UMP9 <- subset(place_whole,weapon_name == "UMP9")
hist(place_UMP9$placement,
     main = "histogram of placements of UMP9 users",
     xlab = "placement",
     ylab = "Frequency")

t.test(place_M416$placement,place_UMP9$placement,alternative = "two.sided",conf.level = 0.95)
t.test(place_P92$placement,place_S1897$placement,alternative = "two.sided",conf.level = 0.95)

# place_AKM <- subset(place_whole,weapon_name == "AKM")
# hist(place_AKM$placement,
#      main = "histogram of placements of AKM users",
#      xlab = "placement",
#      ylab = "Frequency")
# 
# place_AWM <- subset(place_whole,weapon_name == "AWM")
# hist(place_AWM$placement,
#      main = "histogram of placements of AWM users",
#      xlab = "placement",
#      ylab = "Frequency")



