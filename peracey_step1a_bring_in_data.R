
#This is a copy / version of the step 1 to be run on peracey
#second stage of data need to be here too
################################################################################################################
#######################    #This is a copy / version of the step 1 to be run on peracey       ################
#######################     rmarkdown file has issues on pearcey      #########################################
################################################################################################################


################################################################################################################
######################## Chuck 1 setup install packages #######################
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
#install.packages("tmap")
library(tmap)
################################################################################################################
########################setting up functions to import the data########################
library(readr)
read_csv_FUN <- function(file ){
  the_data <- read_csv(file, col_types = cols(value = col_character()))
}


import_function <- function(mydir){
  
  myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
  #1a########### Get file names
  filenames <- myfiles
  collars <- str_extract(myfiles, "[a-z]+\\d{1,6}")
  
  #1b########## Get length of each csv
  file_lengths <- unlist(lapply(lapply(filenames, read_csv), nrow))
  #file_lengths
  #1c########## Repeat collars using lengths
  file_names <- rep(collars,file_lengths)
  
  #1d######### Create table
  tbl <- lapply(filenames, read_csv_FUN) %>% #this call the function read_csv_FUN I made outside this import_function
    bind_rows()
  #1e######### Combine file_names and tbl
  VF <- cbind(tbl, collar_ID = file_names)
  #glimpse(VF)
  
  ################## Step 2 extra clms for  raw logged data   ##############################################
  VF <- VF %>% 
    separate(collar_ID,into =  c("collar", "date"),  sep = "_", remove = FALSE ) %>% 
    mutate(hms = hms::as.hms(time, tz="GMT"),
           date= date(time),
           month = month(time),
           day = day(time))
  #write_csv(VF, path = paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/", "VF_", mydir, ".csv")) 
}




# step 2 Use the function to import the data and combine
#It would be better if this was a loop.
################################################################################################################
################## chuck 3 use function bring n data  ################## 

##### Use the function to bring in data for one day that is specified ######
getwd()
#setwd("collar logs_download2/")

VF_20190607 <- import_function("20190607")
VF_20190606 <- import_function("20190606")
VF_20190605 <- import_function("20190605")
VF_20190604 <- import_function("20190604")
VF_20190603 <- import_function("20190603")
VF_20190602 <- import_function("20190602") #not written r bind - is now
VF_20190601 <- import_function("20190601")
VF_20190531 <- import_function("20190531")
VF_20190530 <- import_function("20190530")
VF_20190529 <- import_function("20190529")
VF_20190528 <- import_function("20190528")
VF_20190527 <- import_function("20190527")
VF_20190526 <- import_function("20190526")
VF_20190525 <- import_function("20190525")
VF_20190524 <- import_function("20190524")
VF_20190523 <- import_function("20190523")
VF_20190522 <- import_function("20190522")
VF_20190521 <- import_function("20190521")
VF_20190520 <- import_function("20190520")
VF_20190519 <- import_function("20190519")
VF_20190518 <- import_function("20190518")
VF_20190517 <- import_function("20190517")

VF_week1 <- rbind(VF_20190517, VF_20190518, VF_20190519,
                  VF_20190520, VF_20190521, VF_20190522, VF_20190523)
VF_week2 <- rbind(VF_20190524, VF_20190525, VF_20190526,
                  VF_20190527, VF_20190528, VF_20190529, VF_20190530)
VF_week3 <- rbind(VF_20190531, VF_20190601, 
                  VF_20190602, 
                  VF_20190603, VF_20190604, VF_20190605, VF_20190606)

################################################################################################################
##########       Merge this all togther   ##########       


VF_week1_2_3 <- rbind(VF_week1, VF_week2, VF_week3)
#saveRDS(VF_week1_2_3,  paste0("download2_R_output/","VF_week1_2_3.rds"))
################################################################################################################
#########    Remove the NA   ##########
VF_week1_2_3 <- VF_week1_2_3 %>% filter(!is.na(lat) | !is.na(lon))

summary(VF_week1_2_3$lat)
summary(VF_week1_2_3$lon)

##########       ensure the column value is a number - double    ##########
VF_week1_2_3_InclusionBord <- filter(VF_week1_2_3, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value))
saveRDS(VF_week1_2_3_InclusionBord,  "download2_R_output/VF_week1_2_3_InclusionBord.rds")

################################################################################################################
##################           Divide up the data into VF chuncks                               ##################    
################################################################################################################
#What is the max time?
max_time_df <- as_datetime(max(VF_week1_2_3_InclusionBord$time), tz="GMT") 
print(max_time_df)
#Fence 1 called training fence eden valley
VF1_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                          as_datetime('2019-05-20 14:40:00', tz="GMT")))
#Fence 2 called training mk1
VF2_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(time, as_datetime('2019-05-20 14:50:00', tz="GMT"),
                                    as_datetime('2019-05-23 08:30:00', tz="GMT")))

#Fence 3 called bron next traing fence
VF3_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(time, as_datetime('2019-05-23 08:30:00', tz="GMT"),
                                    as_datetime('2019-05-28 11:00:00', tz="GMT")))

#Fence 4 called bron next traing fence check that the time range is working
VF4_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(time, as_datetime('2019-05-28 11:15:00', tz="GMT"),
                                    as_datetime('2019-06-03 09:30:00', tz="GMT")))

#Fence 5 called bron next traing fence Cant don this yet without the full data set
#VF5_InclusionBord <- filter(xxxx, 
#                            between(time, as_datetime('2019-06-03 09:31:00', tz="GMT"),
#                                    as_datetime('2019-07-02 06:11:00', tz="GMT")))







