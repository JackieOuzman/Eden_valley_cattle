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

########################### set up coods ##################################  

#https://spatialreference.org/ref/epsg/gda94-mga-zone-56/
#epsg projection 28356

mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs


################## Step 1 bring in the raw logged data and make file name  ##############################################


setwd("W:/VF/Eden_Valley/logged_VF_data/Converted data/collar logs/")
mydir = "20190518"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

#1a########### Get file names
filenames <- myfiles
collars <- str_extract(myfiles, "[a-z]+\\d{1,6}")

#1b########## Get length of each csv
file_lengths <- unlist(lapply(lapply(filenames, read_csv), nrow))
file_lengths
#1c########## Repeat collars using lengths
file_names <- rep(collars,file_lengths)

#1d######### Create table
tbl <- lapply(filenames, read_csv) %>% 
  bind_rows()

#1e######### Combine file_names and tbl
VF2019_05_18 <- cbind(tbl, collar_ID = file_names)
glimpse(VF2019_05_18)


################## Step 2 extra clms for  raw logged data   ##############################################
VF2019_05_18 <- VF2019_05_18 %>% 
  separate(collar_ID,into =  c("collar", "date"),  sep = "_", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time, tz="GMT"),
         date= date(time),
         month = month(time),
         day = day(time))
glimpse(VF2019_05_18)

#################### step 4 convert lat and longs to x and Y    ##########################################
coordinates(VF2019_05_18) <- ~ lon + lat
proj4string(VF2019_05_18) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF2019_05_18_1 <- spTransform(VF2019_05_18, mapCRS)
#make new df_1
VF2019_05_18 = as.data.frame(VF2019_05_18_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF2019_05_18 <- mutate(VF2019_05_18,POINT_X = lon,  POINT_Y = lat )
glimpse(VF2019_05_18)
#############################    step 3 plot data                        #########################################
ggplot (VF2019_05_18, aes( hms, value))+
  geom_point()+
  facet_wrap(day~ collar)






################## Step 1 bring in the raw logged data and make file name  ##############################################

#"W:/VF/Eden_Valley/logged_VF_data/Converted data/collar logs/20190528"
setwd("W:/VF/Eden_Valley/logged_VF_data/Converted data/collar logs/")
mydir = "20190528"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles
#1a########### Get file names
filenames <- myfiles
collars <- str_extract(myfiles, "[a-z]+\\d{1,6}")

#1b########## Get length of each csv
file_lengths <- unlist(lapply(lapply(filenames, read_csv), nrow)) #I have error meassage here?? do I need to be explicit in how the files are brought in?
#file_lengths <- unlist(lapply(lapply(filenames, read_csv(col_types = cols(event = col_character(), 
                                                                          #hdop = col_double(), heading = col_double(), 
                                                                          #lat = col_double(), lon = col_double(), 
                                                                          #`m/s` = col_double(), value = col_double()))), nrow))
file_lengths


#1c########## Repeat collars using lengths
file_names <- rep(collars,file_lengths)

#1d######### Create table
tbl <- lapply(filenames, read_csv) %>% 
  bind_rows()
tbl
#1e######### Combine file_names and tbl
VF2019_05_28 <- cbind(tbl, collar_ID = file_names)
#tbl <- cbind(tbl, collar_ID = file_names)
glimpse(VF2019_05_28)


################## Step 2 extra clms for  raw logged data   ##############################################
VF2019_05_28 <- VF2019_05_28 %>% 
  separate(collar_ID,into =  c("collar", "date"),  sep = "_", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time, tz="GMT"),
         date= date(time),
         month = month(time),
         day = day(time))
glimpse(VF2019_05_28)

#################### step 4 convert lat and longs to x and Y    ##########################################
coordinates(VF2019_05_28) <- ~ lon + lat
proj4string(VF2019_05_28) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF2019_05_28_1 <- spTransform(VF2019_05_28, mapCRS)
#make new df_1
VF2019_05_28 = as.data.frame(VF2019_05_28_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF2019_05_28 <- mutate(VF2019_05_28,POINT_X = lon,  POINT_Y = lat )
glimpse(VF2019_05_28)
#############################    step 3 plot data                        #########################################
ggplot (VF2019_05_28, aes( hms, value))+
  geom_point()+
  facet_wrap(day~ collar)












