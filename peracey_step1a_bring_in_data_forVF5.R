
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
#setwd("updated collar logs/")
VF_20190607 <- import_function("20190607")
VF_20190608 <- import_function("20190608")
VF_20190609 <- import_function("20190609")
VF_20190610 <- import_function("20190610")
VF_20190611 <- import_function("20190611")
VF_20190612 <- import_function("20190612")
VF_20190613 <- import_function("20190613") 
VF_20190614 <- import_function("20190614")
VF_20190615 <- import_function("20190615")
VF_20190616 <- import_function("20190616")
VF_20190617 <- import_function("20190617")
VF_20190618 <- import_function("20190618")
VF_20190619 <- import_function("20190619")
VF_20190620 <- import_function("20190620")
VF_20190621 <- import_function("20190621")
VF_20190622 <- import_function("20190622")
VF_20190623 <- import_function("20190623")
VF_20190624 <- import_function("20190624")
VF_20190625 <- import_function("20190625")
VF_20190626 <- import_function("20190626")
VF_20190627 <- import_function("20190627")
VF_20190628 <- import_function("20190628")
VF_20190629 <- import_function("20190629")
VF_20190630 <- import_function("20190630")
VF_20190701 <- import_function("20190701")
VF_20190702 <- import_function("20190702")

VF_week4 <- rbind(VF_20190607, VF_20190608, VF_20190609, VF_20190610,VF_20190611,
                  VF_20190612,VF_20190613)
                  
VF_week5 <- rbind(VF_20190614, VF_20190615, VF_20190616, VF_20190617,
                  VF_20190618, VF_20190619, VF_20190620)
VF_week6 <- rbind(VF_20190621, VF_20190622, VF_20190623, VF_20190624, 
                  VF_20190625, VF_20190626, VF_20190627)
VF_week7 <- rbind(VF_20190628, VF_20190629, VF_20190630, VF_20190631, 
                  VF_20190701, VF_20190702)

################################################################################################################
##########       Merge this all togther   ##########       


VF_week4_5_6_7 <- rbind(VF_week4, VF_week5, VF_week6, VF_week7)
head(VF_week4_5_6_7)
getwd()
saveRDS(VF_week4_5_6_7,  "VF_week4_5_6_7.rds")
write_csv(VF_week4_5_6_7, "VF_week4_5_6_7.csv")
#just for checking

################################################################################################################
#########    Remove the NA   ##########
VF_week4_5_6_7 <- VF_week4_5_6_7 %>% filter(!is.na(lat) | !is.na(lon))

summary(VF_week4_5_6_7$lat)
summary(VF_week4_5_6_7$lon)

##########       ensure the column value is a number - double    ##########
VF_week4_5_6_7_InclusionBord <- filter(VF_week4_5_6_7, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value))
saveRDS(VF_week4_5_6_7_InclusionBord,  "download2_R_output/VF_week4_5_6_7_InclusionBord.rds")

################################################################################################################
##################           Divide up the data into VF chuncks                               ##################    
################################################################################################################
#What is the max time?
max_time_df <- as_datetime(max(VF_week4_5_6_7_InclusionBord$time), tz="GMT") 
print(max_time_df)



#Fence 5 called bron next traing fence Cant do this yet without the full data set
#VF5_InclusionBord <- filter(VF_week4_5_6_7_InclusionBord, 
#                            between(time, as_datetime('2019-06-03 09:31:00', tz="GMT"),
#                                    as_datetime('2019-07-02 06:11:00', tz="GMT")))







