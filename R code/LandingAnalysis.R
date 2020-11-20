install.packages('tidyverse')
install.packages('data.table')
library(tidyverse)
library(data.table)
data01 = read.csv("PUBG_Data1_1.csv")
data=data.frame(data01)
#9653114
chengqu_t5=0
yequ_t5=0
chengqu_b=0
yequ_b=0
#match_id="2U4GBNA0Ymk0-0_lchMMxG8bDAId5tduobGh22wgMXUtUmXAQbArmRwtdVauexaq"
for(i in 1:9653114){
  
  if(data[i,]$time<=720){
    #killer location
    
    Dist_Georgopol=sqrt((data[i,]$killer_position_x-108200)^2+(data[i,]$killer_position_y-193200)^2)
    Dist_Yasnaya=sqrt((data[i,]$killer_position_x-501200)^2+(data[i,]$killer_position_y-201100)^2)
    Dist_Rozhk=sqrt((data[i,]$killer_position_x-373400)^2+(data[i,]$killer_position_y-262200)^2)
    Dist_School=sqrt((data[i,]$killer_position_x-402700)^2+(data[i,]$killer_position_y-305500)^2)
    Dist_Pochinki=sqrt((data[i,]$killer_position_x-332170)^2+(data[i,]$killer_position_y-371447)^2)
    Dist_Mylta=sqrt((data[i,]$killer_position_x-560260)^2+(data[i,]$killer_position_y-443900)^2)
    Dist_Military=sqrt((data[i,]$killer_position_x-393110)^2+(data[i,]$killer_position_y-586880)^2)
    #victim location
    Dist_Georgopol_v=sqrt((data[i,]$victim_position_x-108200)^2+(data[i,]$victim_position_y-193200)^2)
    Dist_Yasnaya_v=sqrt((data[i,]$victim_position_x-501200)^2+(data[i,]$victim_position_y-201100)^2)
    Dist_Rozhk_v=sqrt((data[i,]$victim_position_x-373400)^2+(data[i,]$victim_position_y-262200)^2)
    Dist_School_v=sqrt((data[i,]$victim_position_x-402700)^2+(data[i,]$victim_position_y-305500)^2)
    Dist_Pochinki_v=sqrt((data[i,]$victim_position_x-332170)^2+(data[i,]$victim_position_y-371447)^2)
    Dist_Mylta_v=sqrt((data[i,]$victim_position_x-560260)^2+(data[i,]$victim_position_y-443900)^2)
    Dist_Military_v=sqrt((data[i,]$victim_position_x-393110)^2+(data[i,]$victim_position_y-586880)^2)

    # print(Dist_Georgopol)
    # print(Dist_Yasnaya)
    # print(Dist_Rozhk)
    # print(Dist_School)
    # print(Dist_Pochinki)
    # print(Dist_Mylta)
    # print(Dist_Military)
  #killer location~rank
    if(Dist_Georgopol<57000|Dist_Yasnaya<31850|Dist_Rozhk<21650|Dist_School<17548.5|Dist_Pochinki<23823.5|Dist_Mylta<23919|Dist_Military<51263.5)
    { #????????????killer in downtown
      if(data[i,]$killer_placement<=5)
      {chengqu_t5=chengqu_t5+1}#killer rank top 5
      if(data[i,]$killer_placement>5)
      {chengqu_b=chengqu_b+1}#killer rank behind 5
     
    }
    else{
    #????????????killer in wild
    if(data[i,]$killer_placement<=5)
    {yequ_t5=yequ_t5+1}#killer rank top 5
    if(data[i,]$killer_placement>5)
    {yequ_b=yequ_b+1}#killer rank behind 5
    }

    #victim location~rank
    if(Dist_Georgopol_v<=57000|Dist_Yasnaya_v<31850|Dist_Rozhk_v<21650|Dist_School_v<17548.5|
       Dist_Pochinki_v<23823.5|Dist_Mylta_v<23919|Dist_Military_v<51263.5)
    { #????????????victim in downtown
      if(data[i,]$victim_placement<=5)#A
      {chengqu_t5=chengqu_t5+1}#victim rank top 5
      if(data[i,]$victim_placement>5)
      {chengqu_b=chengqu_b+1}#killer rank behind
    } 
    else
    {
      #????????????victim in wild
      if(data[i,]$victim_placement<=5)#B
      {yequ_t5=yequ_t5+1} #killer rank top 5
      
      if(data[i,]$victim_placement>5)
      {yequ_b=yequ_b+1}#killer rank behind 5
    }
 
    } #720
  print(i)
}
print(chengqu_t5)
print(chengqu_b)
print(yequ_t5)
print(yequ_b)

R1 <- c(yequ_t5,yequ_b)
R2 <- c(chengqu_t5,chengqu_b)
rows <- 2

Matriz <- matrix(c(R1,R2),nrow=rows, byrow =TRUE)

rownames(Matriz) <- c("Wild_Area","Downtown_Area")
colnames(Matriz) <- c("Ranking<=5","Ranking<=5")

Result <- chisq.test(Matriz,correct=FALSE)
print(Result)
