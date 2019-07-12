install.packages("gganimate")
install.packages("png")


library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(lubridate)
library(ggplot2)
library(readr)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)
library(gganimate)
library(png)


VF_week1 <- read_csv("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/VF_week1.csv")
VF_week2 <- read_csv("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/VF_week2.csv")
VF_week3 <- read_csv("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/VF_week3.csv")


VF_week1_InclusionBord <- filter(VF_week1, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value)) 

VF_week2_InclusionBord <- filter(VF_week2, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value)) 
VF_week3_InclusionBord <- filter(VF_week3, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value)) 

#3.do projections
########################## set up coods ##################################  

#https://spatialreference.org/ref/epsg/gda94-mga-zone-56/
#epsg projection 28356

mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

####################  convert lat and longs to x and Y    Thsi is not working yet ##########################################
coordinates(VF_week1_InclusionBord) <- ~ lon + lat
proj4string(VF_week1_InclusionBord) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_week1_InclusionBord_1 <- spTransform(VF_week1_InclusionBord, mapCRS)
#make new df_1
VF_week1_InclusionBord = as.data.frame(VF_week1_InclusionBord_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_week1_InclusionBord <- mutate(VF_week1_InclusionBord,POINT_X = lon,  POINT_Y = lat )
glimpse(VF_week1_InclusionBord)

########################  filter data week 1######################## 

VF_week1_InclusionBord_VF1 <- filter(VF_week1_InclusionBord, time > as_datetime('2019-05-20 14:30:00', tz="GMT")) 
###################      Filter out rows of data for 23/5/2019 ac207 and ac213
###This pulls out the dates I dont want I want to chuck these dates and times out #####
test <-  filter(VF_week1_InclusionBord_VF1, 
                collar_ID == "ac207" & 
                between(time, as_datetime('2019-05-23 08:00:00', tz="GMT"),
                              as_datetime('2019-05-23 09:00:00', tz="GMT")))                                        
#change the syntax to something like this
#filter(FL_DATE >= as.Date("2014-01-05") & FL_DATE <= as.Date("2014-01-10"))









########################  display data week 1######################## 
VF_week1_InclusionBord %>% 
  #filter(date == "2019-05-17") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-17",
       x= "Time of day",
       y = "Distance (m) from VF")



