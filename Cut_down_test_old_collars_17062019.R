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

############################# second download before new collar were fitted #####################




#### streamline approach with function#####

setwd("W:/VF/Eden_Valley/logged_VF_data/collar logs_download2/")

#function 1 - just defining how the csv files should be imported
#this needs to be done for this data set because the data column 'value' is a mix of numbers and text
#but in some years its just one or the other
read_csv_FUN <- function(file ){
  the_data <- read_csv(file, col_types = cols(value = col_character()))
}
#mydir = "20190528" I will use this for function input



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
  write_csv(VF, path = paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/", "VF_", mydir, ".csv")) 
}



##### Use the function to bring in data for one day that is specified ######
VF_20190607 <- import_function("20190607")
VF_20190606 <- import_function("20190606")
VF_20190605 <- import_function("20190605")
VF_20190604 <- import_function("20190604")
VF_20190603 <- import_function("20190603")
VF_20190602 <- import_function("20190602") #not written r bind 
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
                  VF_20190602, #not written ? not sure why
                  VF_20190603, VF_20190604, VF_20190605, VF_20190606)

write_csv(VF_week1, path = paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/", "VF_week1.csv"))
write_csv(VF_week2, path = paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/", "VF_week2.csv"))
write_csv(VF_week3, path = paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/", "VF_week3.csv"))
glimpse(VF_20190607)

########################################################################################################################
#1.filter out data that is just for the InclusionBorder_m
#2.change the value to double
#3.do projections

#1.filter out data that is just for the InclusionBorder_m
#and                                       
#2.change the value to double         


VF_week1_InclusionBord <- filter(VF_week1, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value)) 

VF_week2_InclusionBord <- filter(VF_week2, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value)) 
VF_week3_InclusionBord <- filter(VF_week3, event == "InclusionBorder_m") %>%   
  mutate( value = as.double(value)) 


glimpse(VF_week1_InclusionBord)
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



########################  display data week 1######################## 
VF_week1_InclusionBord %>% 
  filter(date == "2019-05-17") %>% 
ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-17",
       x= "Time of day",
       y = "Distance (m) from VF")
  
VF_week1_InclusionBord %>% 
  filter(date == "2019-05-18") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-18",
       x= "Time of day",
       y = "Distance (m) from VF")
VF_week1_InclusionBord %>% 
  filter(date == "2019-05-19") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-19",
       x= "Time of day",
       y = "Distance (m) from VF")
VF_week1_InclusionBord %>% 
  filter(date == "2019-05-20") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-20",
       x= "Time of day",
       y = "Distance (m) from VF")
VF_week1_InclusionBord %>% 
  filter(date == "2019-05-21") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-21",
       x= "Time of day",
       y = "Distance (m) from VF")
 
VF_week1_InclusionBord %>% 
  filter(date == "2019-05-22") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-22",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week1_InclusionBord %>% 
  filter(date == "2019-05-23") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week1 2019-05-23",
       x= "Time of day",
       y = "Distance (m) from VF")



########################  display data week 2 ######################## 
VF_week2_InclusionBord %>% 
  filter(date == "2019-05-24") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week2 2019-05-24",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week2_InclusionBord %>% 
  filter(date == "2019-05-25") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week2 2019-05-25",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week2_InclusionBord %>% 
  filter(date == "2019-05-26") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week2 2019-05-26",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week2_InclusionBord %>% 
  filter(date == "2019-05-27") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week2 2019-05-27",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week2_InclusionBord %>% 
  filter(date == "2019-05-28") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week2 2019-05-28",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week2_InclusionBord %>% 
  filter(date == "2019-05-29") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week2 2019-05-29",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week2_InclusionBord %>% 
  filter(date == "2019-05-30") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week2 2019-05-30",
       x= "Time of day",
       y = "Distance (m) from VF")


########################  display data week 3 ######################## 


VF_week3_InclusionBord %>% 
  filter(date == "2019-05-31") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week3 2019-05-31",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week3_InclusionBord %>% 
  filter(date == "2019-06-01") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week3 2019-06-01",
       x= "Time of day",
       y = "Distance (m) from VF")

##### MISSING THIS DATA SOMETHING WRONG WITH IMPORT #######
#test <- VF_week3_InclusionBord %>% 
#  filter(date == "2019-06-02") 
#glimpse(test)
#ggplot(test, aes(x = hms, y = value, colour = collar))+
#  geom_point()+
#  facet_wrap(.~collar)
#summary(test)  
  
VF_week3_InclusionBord %>% 
  filter(date == "2019-06-02") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week3 2019-06-02",
       x= "Time of day",
       y = "Distance (m) from VF")


VF_week3_InclusionBord %>% 
  filter(date == "2019-06-03") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week3 2019-06-03",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week3_InclusionBord %>% 
  filter(date == "2019-06-04") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week3 2019-06-04",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week3_InclusionBord %>% 
  filter(date == "2019-06-05") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week3 2019-06-05",
       x= "Time of day",
       y = "Distance (m) from VF")

VF_week3_InclusionBord %>% 
  filter(date == "2019-06-06") %>% 
  ggplot(aes(x = hms, y = value, colour = collar))+
  geom_point()+
  facet_wrap(.~collar)+
  facet_wrap(.~collar)+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")+
  labs(title= "week3 2019-06-06",
       x= "Time of day",
       y = "Distance (m) from VF")


