### Graphing the cows movement a certain days....
install.packages("DT")
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

#Bring in the data
input_data <- file.path("W:", "VF", "Eden_valley", "logged_VF_data", "Jax_Dec_2019_processing")

VF1_recal <- read_csv(paste0(input_data, "/VF1_recal.csv"))
VF2_recal <- read_csv(paste0(input_data, "/VF2_recal.csv"))
VF3_recal <- read_csv(paste0(input_data, "/VF3_recal.csv"))
VF4_recal <- read_csv(paste0(input_data, "/VF4_recal.csv"))
VF5_recal <- read_csv(paste0(input_data, "/VF5_recal.csv"))

VF1_recal <- mutate(VF1_recal, VF = 1)
VF2_recal <- mutate(VF2_recal, VF = 2)
VF3_recal <- mutate(VF3_recal, VF = 3)
VF4_recal <- mutate(VF4_recal, VF = 4)
VF5_recal <- mutate(VF5_recal, VF = 5)

str(VF1_recal)
unique(VF5_recal$date)
#need to make df into spatial object for graphing and try clipping to paddock boundary
VF1_recal <-
  st_as_sf(VF1_recal,
           coords = c("X", "Y"),
           crs = 28354,
           agr = "constant") %>% 
  st_intersection( eden_valley)

VF2_recal <-
  st_as_sf(VF2_recal,
           coords = c("X", "Y"),
           crs = 28354,
           agr = "constant") %>% 
  st_intersection( eden_valley)

VF3_recal <-
  st_as_sf(VF3_recal,
           coords = c("X", "Y"),
           crs = 28354,
           agr = "constant") %>% 
  st_intersection( eden_valley)

VF4_recal <-
  st_as_sf(VF4_recal,
           coords = c("X", "Y"),
           crs = 28354,
           agr = "constant") %>% 
  st_intersection( eden_valley)

VF5_recal <-
  st_as_sf(VF5_recal,
           coords = c("X", "Y"),
           crs = 28354,
           agr = "constant") %>% 
  st_intersection( eden_valley)


########################################################################################################
###  paddock bounadry
eden_valley <- st_read("W:/VF/Eden_Valley/VF_Boundary/EdenValley_site1GDA_a.shp")
#assign a coord ref epsg
st_crs(eden_valley) <- 28354

#make this the epsg that will be used for all data
the_crs <- st_crs(eden_valley, asText = TRUE)

### 9b. VF lines

fence1 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence1.shp")
fence2 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence2.shp")
fence3 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence3.shp")
fence4 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence4a.shp")
fence5 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence5.shp")

st_crs(fence1) <- 28354
st_crs(fence2) <- 28354
st_crs(fence3) <- 28354
st_crs(fence4) <- 28354
st_crs(fence5) <- 28354



################################################################################################################################
### 1a. Day 1 facet wrap

head(VF1_recal)
#Try plotting a few days day 1 vf 1
day1Vf1 <- filter(VF1_recal, day_since_start == 1 & VF == 1) 
head(day1Vf1)
str(day1Vf1)


  ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = day1Vf1, aes(colour = animal_ID)) +
  #facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day1Vf1_facet_wrap.png", device = "png" ,
        width = 20, height = 12, units = "cm")


### 1b. Day 1 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "blue") +
  geom_sf(data = day1Vf1, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day1Vf1_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 1c. Day 1 balck and white and problem cow overlayed Q26, Q36, Q29
head(day1Vf1)
day1Vf1_problem_cow <- filter(day1Vf1,
                              animal_ID == "Q26" |
                              animal_ID == "Q36" |
                              animal_ID == "Q29")

unique(day1Vf1_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "blue") +
  geom_sf(data = day1Vf1, alpha = 0.01)+
  geom_sf(data = day1Vf1_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
        legend.position = "bottom",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day1Vf1_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")

#################################################################################################################################

### 2a. Day 1 with vf 2 facet wrap

head(VF1_recal)
#Try plotting a few days day 1 vf 2
day1Vf2 <- filter(VF2_recal, day_since_start == 1 & VF == 2) 
head(day1Vf2)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence2, color = "grey") +
  geom_sf(data = day1Vf2, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day1Vf2_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 2b. Day 1-2 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence2, color = "blue") +
  geom_sf(data = day1Vf2, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day1Vf2_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 2c. Day 1-2 balck and white and problem cow overlayed Q26, Q36, Q29

day1Vf2_problem_cow <- filter(day1Vf2,
                              animal_ID == "Q26" |
                                animal_ID == "Q36" |
                                animal_ID == "Q29")

unique(day1Vf2_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence2, color = "blue") +
  geom_sf(data = day1Vf2, alpha = 0.01)+
  geom_sf(data = day1Vf2_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-20")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day1Vf2_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")

####################################################################################################################################
### 3a. Day 2 with vf 2 facet wrap

head(VF1_recal)
#Try plotting a few days day 2 vf 2
day2Vf2 <- filter(VF2_recal, day_since_start == 2 & VF == 2) 
head(day2Vf2)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence2, color = "grey") +
  geom_sf(data = day2Vf2, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-21")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day2Vf2_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 3b. Day 2 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence2, color = "blue") +
  geom_sf(data = day2Vf2, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-21")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day2Vf2_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 3c. Day 2 balck and white and problem cow overlayed Q26, Q36, Q29

day2Vf2_problem_cow <- filter(day2Vf2,
                              animal_ID == "Q26" |
                                animal_ID == "Q36" |
                                animal_ID == "Q29")

unique(day2Vf2_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence2, color = "blue") +
  geom_sf(data = day2Vf2, alpha = 0.01)+
  geom_sf(data = day2Vf2_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-21")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day2Vf2_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")




####################################################################################################################################
### 4a. Day 4 with vf 3 facet wrap



day4Vf3 <- filter(VF3_recal, day_since_start == 4 & VF == 3) 


ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "grey") +
  geom_sf(data = day4Vf3, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-23")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day4Vf3_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 4b. Day 4 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "blue") +
  geom_sf(data = day4Vf3, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-23")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day4Vf3_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 4c. Day 4 balck and white and problem cow overlayed Q26, Q36, Q29

day4Vf3_problem_cow <- filter(day4Vf3,
                              animal_ID == "Q26" |
                                animal_ID == "Q36" |
                                animal_ID == "Q29")

unique(day4Vf3_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "blue") +
  geom_sf(data = day4Vf3, alpha = 0.01)+
  geom_sf(data = day4Vf3_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-23")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day4Vf3_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")



####################################################################################################################################
### 5a. Day 5 with vf 3 facet wrap



day5Vf3 <- filter(VF3_recal, day_since_start == 5 & VF == 3) 


ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "grey") +
  geom_sf(data = day5Vf3, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-24")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day5Vf3_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 4b. Day 5 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "blue") +
  geom_sf(data = day5Vf3, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-24")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day5Vf3_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 4c. Day 5 balck and white and problem cow overlayed Q26, Q36, Q29

day5Vf3_problem_cow <- filter(day5Vf3,
                              animal_ID == "Q26" |
                                animal_ID == "Q36" |
                                animal_ID == "Q29")

unique(day5Vf3_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "blue") +
  geom_sf(data = day5Vf3, alpha = 0.01)+
  geom_sf(data = day5Vf3_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-24")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day5Vf3_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")








####################################################################################################################################
### 6a. Day 9 with vf 4 facet wrap



day9Vf4 <- filter(VF4_recal, day_since_start == 9 & VF == 4) 


ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "grey") +
  geom_sf(data = day9Vf4, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-28")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day9Vf4_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 6b. Day 9 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "blue") +
  geom_sf(data = day9Vf4, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-28")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day9Vf4_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 6c. Day 9 balck and white and problem cow overlayed Q26, Q36, Q29

day9Vf4_problem_cow <- filter(day9Vf4,
                              animal_ID == "Q26" |
                                animal_ID == "Q36" |
                                animal_ID == "Q29")

unique(day9Vf4_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "blue") +
  geom_sf(data = day9Vf4, alpha = 0.01)+
  geom_sf(data = day9Vf4_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-28")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day9Vf4_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")





####################################################################################################################################
### 7a. Day 10 with vf 4 facet wrap



day10Vf4 <- filter(VF4_recal, day_since_start == 10 & VF == 4) 


ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "grey") +
  geom_sf(data = day10Vf4, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-29")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day10Vf4_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 7b. Day 10 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "blue") +
  geom_sf(data = day10Vf4, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-29")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day10Vf4_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 6c. Day 9 balck and white and problem cow overlayed Q26, Q36, Q29

day10Vf4_problem_cow <- filter(day10Vf4,
                              animal_ID == "Q26" |
                                animal_ID == "Q36" |
                                animal_ID == "Q29")

unique(day10Vf4_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "blue") +
  geom_sf(data = day10Vf4, alpha = 0.01)+
  geom_sf(data = day10Vf4_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-05-29")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day10Vf4_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")



####################################################################################################################################
### 8a. Day 15 with vf 5 facet wrap



day15Vf5 <- filter(VF5_recal, day_since_start == 15 & VF == 5) 


ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence5, color = "grey") +
  geom_sf(data = day15Vf5, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-06-03")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day15Vf5_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 7b. Day 15 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence5, color = "blue") +
  geom_sf(data = day15Vf5, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-06-03")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day15Vf5_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 6c. Day 15 balck and white and problem cow overlayed Q26, Q36, Q29

day15Vf5_problem_cow <- filter(day15Vf5,
                               animal_ID == "Q26" |
                                 animal_ID == "Q36" |
                                 animal_ID == "Q29")

unique(day15Vf5_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence5, color = "blue") +
  geom_sf(data = day15Vf5, alpha = 0.01)+
  geom_sf(data = day15Vf5_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-06-03")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day15Vf5_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")



####################################################################################################################################
### 9a. Day 16 with vf 5 facet wrap



day16Vf5 <- filter(VF5_recal, day_since_start == 16 & VF == 5) 


ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence5, color = "grey") +
  geom_sf(data = day16Vf5, aes(colour = animal_ID)) +
  facet_wrap(.~animal_ID)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-06-04")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day16Vf5_facet_wrap.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 8b. Day 16 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence5, color = "blue") +
  geom_sf(data = day16Vf5, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-06-04")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day16Vf5_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 7c. Day 15 balck and white and problem cow overlayed Q26, Q36, Q29

day16Vf5_problem_cow <- filter(day16Vf5,
                               animal_ID == "Q26" |
                                 animal_ID == "Q36" |
                                 animal_ID == "Q29")

unique(day16Vf5_problem_cow$animal_ID)

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence5, color = "blue") +
  geom_sf(data = day16Vf5, alpha = 0.01)+
  geom_sf(data = day16Vf5_problem_cow, alpha = 0.01, aes(colour = animal_ID))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "2019-06-04")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day16Vf5_Problem_cows.png", device = "png" ,
       width = 20, height = 12, units = "cm")


########################################################################################################

#Animation giff day 1 

#having a spot of trouble here? not sure why

p4a <- ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = day1Vf1, aes(colour = animal_ID))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p4a  
p4b <- p4a +
  labs( #title =   'Date:  {format(as_datetime(frame_time ), tz="GMT")}',
        title =   'Date:  {format(as_datetime(frame_time, "%b %e"), tz="GMT")}',
        #subtitle = 'Hour: {format(as_datetime(frame_time, "%H"), tz="GMT")}',
        caption = "Frame {frame} of {nframes} ({progress * 100}%)") +
  transition_time(time) +
  shadow_wake(0.3)
animation_20thVf1 <- animate(p4b, duration = 60) 
animation_20thVf1
anim_save(animation = animation_20thVf1 , filename = "animation_20thVf1.gif")




########################################################################################################
###########   

day1_3Vf2 <- filter(VF2_recal, between(VF2_recal$day_since_start,1,3) & VF == 2)
unique(day1_3Vf2$day_since_start) 



### 1b. Day 1-3 balck and white
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence2, color = "blue") +
  geom_sf(data = day1_3Vf2, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Day 1, 2, 3 VF 2")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day1_3Vf2_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 1c. Day 4-8 balck and white
unique(VF3_recal$day_since_start) 

day4_5Vf3 <- filter(VF3_recal, between(VF3_recal$day_since_start,4,8) & VF == 3)
unique(day4_5Vf3$day_since_start) 

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "blue") +
  geom_sf(data = day4_5Vf3, alpha = 0.01)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Day 4 to 8 VF 3")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day4_8Vf3_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")


### 1c. Day 9-14 balck and white
unique(VF5_recal$day_since_start) 
unique(VF5_recal$date)
unique(VF5_recal$week_number)
#something wrong here

day4_5Vf3 <- filter(VF3_recal, between(VF3_recal$day_since_start,4,8) & VF == 3)
unique(day4_5Vf3$day_since_start) 

ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "blue") +
  geom_sf(data = day4_5Vf3, alpha = 0.001)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Day 4 to 8 VF 3")+
  xlab("") +
  ylab("") 

ggsave(path= graph_path, filename = "day4_8Vf3_BW.png", device = "png" ,
       width = 20, height = 12, units = "cm")

