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

# VF1_recal <- read_csv(paste0(input_data, "/VF1_recal.csv"))
 VF2_recal <- read_csv(paste0(input_data, "/VF2_recal.csv"))
 VF3_recal <- read_csv(paste0(input_data, "/VF3_recal.csv"))
 VF4_recal <- read_csv(paste0(input_data, "/VF4_recal.csv"))
VF5_recal <- read_csv(paste0(input_data, "/VF5_recal.csv"))

#add clm with the VF 
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

#---------------------------------------------------------------------------------------------------------
#week 4

VF5_recal_week_4 <- filter(VF5_recal, week_number == 4 & VF == 5)
VF5_recal_week_4 <- select(VF5_recal_week_4,
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
write_csv(VF5_recal_week_4, 
          path = paste0(output_data, "/VF5_recal_week_4.csv"))
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


