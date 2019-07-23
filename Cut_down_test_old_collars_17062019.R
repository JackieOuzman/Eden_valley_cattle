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

############################# second download before new collar were fitted #####################




#### streamline approach with function#####



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
  write_csv(VF, path = paste0("download2_R_output/", "VF_", mydir, ".csv")) 
}



##### Use the function to bring in data for one day that is specified ######

VF_20190523 <- import_function("20190523")
VF_20190522 <- import_function("20190522")
VF_20190521 <- import_function("20190521")
VF_20190520 <- import_function("20190520")
VF_20190519 <- import_function("20190519")
VF_20190518 <- import_function("20190518")
VF_20190517 <- import_function("20190517")

VF_week1 <- rbind(VF_20190517, VF_20190518, VF_20190519,
                  VF_20190520, VF_20190521, VF_20190522, VF_20190523)


write_csv(VF_week1, path = paste0("/download2_R_output/", 
                                  "VF_week1.csv"))

glimpse(VF_20190607)
########################################################################################################################
VF_week1 <- read_csv(file = "download2_R_output/VF_week1.csv")

########################################################################################################################
#1.filter out data that is just for the InclusionBorder_m
#2.change the value to double
#3.do projections

#1.filter out data that is just for the InclusionBorder_m
#and                                       
#2.change the value to double         


VF_week1_InclusionBord <- filter(VF_week1, event == "InclusionBorder_m") %>%   
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
  
