### Graphing the cows movement a certain days....

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(readr)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)
library(gganimate)
library(png)
library(gifski)
library(transformr)
library(DT)


graph_path <- file.path("W:", "VF", "Eden_valley", "graphs")

VF1_recal <- mutate(VF1_recal, VF = 1)
VF2_recal <- mutate(VF2_recal, VF = 2)
VF3_recal <- mutate(VF3_recal, VF = 3)
VF4_recal <- mutate(VF4_recal, VF = 4)
VF5_recal <- mutate(VF5_recal, VF = 5)

head(VF1_recal)
#Try plotting a few days day 1 vf 1
day1Vf1 <- filter(VF1_recal, day_since_start == 1 & VF == 1) 
head(day1Vf1)
  ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = day1Vf1, aes(colour = animal_ID)) #+
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 

gsave(path= graph_path, filename = "day1Vf1_facet_wrap.png", device = "png" ,
        width = 20, height = 12, units = "cm")

  
  
  

x_yInclusion_Dist20_05 <- ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = sp_VF1_InclusionBord_animalID_20, aes(colour = animal_ID))+
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 
x_yInclusion_Dist20_05
ggsave(filename = "x_yInclusion_Dist20_05_Vf1.png", device = "png" ,dpi=600)


ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "blue") +
  geom_sf(data = sp_VF1_InclusionBord_animalID_20, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 







########################################################################################################

Animation giff day 1 

```{r animation for day 1}
#having trouble getting this going - could try the df not spatial object data??
#getting close
p4a <- ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = sp_VF1_InclusionBord_animalID_20, aes(colour = animal_ID))+
  #geom_point(data = Inclusion_20_05_2019, aes(POINT_X, POINT_Y,colour = collar), inherit.aes = FALSE) +
  #facet_wrap(.~collar)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p4a  
p4b <- p4a +
  labs( title =   'Date:  {format(as_datetime(frame_time ), tz="GMT")}',
        #title =   'Date:  {format(as_datetime(frame_time, "%b %e"), tz="GMT")}',
        #subtitle = 'Hour: {format(as_datetime(frame_time, "%H"), tz="GMT")}',
        caption = "Frame {frame} of {nframes} ({progress * 100}%)") +
  transition_time(time) +
  shadow_wake(0.3)
animation_20thVf1 <- animate(p4b, duration = 60) 
animation_20thVf1
anim_save(animation = animation_20thVf1 , filename = "animation_20thVf1.gif")
