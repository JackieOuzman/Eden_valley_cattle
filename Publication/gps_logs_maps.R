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

#---------------------------------------------------------------------------------------------------------
#set up location of data input and output



input_data <- file.path("W:", "VF", "Eden_valley", "logged_VF_data", "Jax_Dec_2019_processing")
output_data <- file.path("W:", "VF", "Eden_valley", "logged_VF_data", "Jax_Dec_2019_processing", "New Folder")

#---------------------------------------------------------------------------------------------------------
#bring in data file as csv

 VF1_recal <- read_csv(paste0(input_data, "/VF1_recal.csv"))
 VF2_recal <- read_csv(paste0(input_data, "/VF2_recal.csv"))
 VF3_recal <- read_csv(paste0(input_data, "/VF3_recal.csv"))
 VF4_recal <- read_csv(paste0(input_data, "/VF4_recal.csv"))
VF5_recal <- read_csv(paste0(input_data, "/VF5_recal.csv"))

#add clm with the VF 
VF1_recal <- mutate(VF1_recal, VF = 1)
VF2_recal <- mutate(VF2_recal, VF = 2)
VF3_recal <- mutate(VF3_recal, VF = 3)
VF4_recal <- mutate(VF4_recal, VF = 4)
VF5_recal <- mutate(VF5_recal, VF = 5)

str(VF5_recal)
unique(VF5_recal$week_number)


#---------------------------------------------------------------------------------------------------------
#select the row and clms for the maps / graphs



## first map day 1 to 3 only
day1_3Vf2 <- filter(VF2_recal, between(VF2_recal$day_since_start,1,3) & VF == 2)
day1_3Vf2 <- select(day1_3Vf2,
                           X ,
                           Y,
                           time,
                           event,
                           collar_ID,
                           date,
                           animal_ID,
                           day_since_start,
                           week_number,
                           non_graz,
                           distance_VF)



write_csv(day1_3Vf2, 
          path = paste0(output_data, "/day1_3Vf2.csv"))

unique(day1_3Vf2$day_since_start)
dim(day1_3Vf2)
###---------------------------------------------------------------------------------------------------------
# map day 4 to 8 only
day4_8Vf3 <- filter(VF3_recal, between(VF3_recal$day_since_start,4,8) & VF == 3)
day4_8Vf3 <- select(day4_8Vf3,
                    X ,
                    Y,
                    time,
                    event,
                    collar_ID,
                    date,
                    animal_ID,
                    day_since_start,
                    week_number,
                    non_graz,
                    distance_VF)



write_csv(day4_8Vf3, 
          path = paste0(output_data, "/day4_8Vf3.csv"))

unique(day4_8Vf3$day_since_start)
dim(day4_8Vf3)
###---------------------------------------------------------------------------------------------------------
# map day 9 to 14 only
day9_14Vf4 <- filter(VF4_recal, between(VF4_recal$day_since_start,9,14) & VF == 4)
day9_14Vf4 <- select(day9_14Vf4,
                    X ,
                    Y,
                    time,
                    event,
                    collar_ID,
                    date,
                    animal_ID,
                    day_since_start,
                    week_number,
                    non_graz,
                    distance_VF)



write_csv(day9_14Vf4, 
          path = paste0(output_data, "/day9_14Vf4.csv"))
unique(day9_14Vf4$day_since_start)

#---------------------------------------------------------------------------------------------------------
#week 3
VF5_recal_week_3 <- filter(VF5_recal, week_number == 3 & VF == 5)
VF5_recal_week_3 <- select(VF5_recal_week_3,
                           X ,
                           Y,
                           time,
                           event,
                           collar_ID,
                           date,
                           animal_ID,
                           day_since_start,
                           week_number,
                           non_graz,
                           distance_VF)
getwd()
str(VF5_recal_week_3)

write_csv(VF5_recal_week_3, 
          path = paste0(output_data, "/VF5_recal_week_3.csv"))

unique(VF5_recal_week_3$day_since_start)

#---------------------------------------------------------------------------------------------------------
#week 4

VF5_recal_week_4 <- filter(VF5_recal, week_number == 4 & VF == 5)
VF5_recal_week_4 <- select(VF5_recal_week_4,
                           X ,
                           Y,
                           collar_ID,
                           date,
                           day_since_start,
                           week_number,
                           non_graz,
                           distance_VF)
write_csv(VF5_recal_week_4, 
          path = paste0(output_data, "/VF5_recal_week_4.csv"))
unique(VF5_recal_week_4$day_since_start)
dim(VF5_recal_week_4)

#---------------------------------------------------------------------------------------------------------
#week 5
VF5_recal_week_5 <- filter(VF5_recal, week_number == 5 & VF == 5)
VF5_recal_week_5 <- select(VF5_recal_week_5,
                           X ,
                           Y,
                           time,
                           event,
                           collar_ID,
                           date,
                           animal_ID,
                           day_since_start,
                           week_number,
                           non_graz,
                           distance_VF)
write_csv(VF5_recal_week_5, 
          path = paste0(output_data, "/VF5_recal_week_5.csv"))

unique(VF5_recal_week_5$day_since_start)
dim(VF5_recal_week_5)
#---------------------------------------------------------------------------------------------------------
#week 6
VF5_recal_week_6 <- filter(VF5_recal, week_number == 6 & VF == 5)
VF5_recal_week_6 <- select(VF5_recal_week_6,
                           X ,
                           Y,
                           time,
                           event,
                           collar_ID,
                           date,
                           animal_ID,
                           day_since_start,
                           week_number,
                           non_graz,
                           distance_VF)
write_csv(VF5_recal_week_6, 
          path = paste0(output_data, "/VF5_recal_week_6.csv"))
unique(VF5_recal_week_6$day_since_start)
dim(VF5_recal_week_6)
#---------------------------------------------------------------------------------------------------------
#week 7
VF5_recal_week_7 <- filter(VF5_recal, week_number == 7 & VF == 5)
VF5_recal_week_7 <- select(VF5_recal_week_7,
                           X ,
                           Y,
                           time,
                           event,
                           collar_ID,
                           date,
                           animal_ID,
                           day_since_start,
                           week_number,
                           non_graz,
                           distance_VF)
write_csv(VF5_recal_week_7, 
          path = paste0(output_data, "/VF5_recal_week_7.csv"))

unique(VF5_recal_week_7$day_since_start)


#---------------------------------------------------------------------------------------------------------
# MAP week 3 for fence 5

eden_valley <- st_read("W:/VF/Eden_Valley/VF_Boundary/EdenValley_site1GDA_a.shp")


#assign a coord ref epsg
st_crs(eden_valley) <- 28354

#make this the epsg that will be used for all data
the_crs <- st_crs(eden_valley, asText = TRUE)

### 9b. VF lines

# fence1 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence1.shp")
# fence2 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence2.shp")
# fence3 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence3.shp")
# fence4 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence4a.shp")
fence5 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence5.shp")

# st_crs(fence1) <- 28354
# st_crs(fence2) <- 28354
# st_crs(fence3) <- 28354
# st_crs(fence4) <- 28354
st_crs(fence5) <- 28354
week3 <- ggplot()+
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence5, color = "blue") 
week3
str(VF5_recal_week_3)
temp <- head(VF5_recal_week_3, 50)
str(temp)

#try to plot the gps points....
ggplot(VF5_recal_week_3, aes(X, Y))+
  geom_point()+
  geom_hex(data = VF5_recal_week_3, aes(X, Y))
  geom_sf(data = eden_valley, color = "black", fill = NA)

---------------------------------------------------------------------------------------------
    
#log of animals that 
str(VF2_recal)
  
    
table_VF1_a <- group_by(VF1_recal, date, animal_ID) %>% 
  count()
table_VF2_a <- group_by(VF2_recal, date, animal_ID) %>% 
  count()
table_VF3_a <- group_by(VF3_recal, date, animal_ID) %>% 
  count()
table_VF4_a <- group_by(VF4_recal, date, animal_ID) %>% 
  count()
table_VF5_a <- group_by(VF5_recal, date, animal_ID) %>% 
  count()  
table_VF1_5_a  <- rbind(table_VF1_a, table_VF2_a, table_VF3_a,
                        table_VF4_a, table_VF5_a)
  

#some duplication of dates
table_VF1_5_a <- group_by(table_VF1_5_a, date, animal_ID) %>% 
  summarise(n = sum(n)) 
table_VF1_5_a <- spread(table_VF1_5_a, animal_ID, n)
table_VF1_5_a
getwd()
write_csv(table_VF1_5_a, 
path = "W:/VF/Eden_Valley/logged_VF_data/Jax_Dec_2019_processing/New Folder/table_VF1_5_a.csv")
