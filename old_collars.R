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






#### try a more streamline approach ? make a function??#####
setwd("W:/VF/Eden_Valley/logged_VF_data/Converted data/collar logs/")
#create a function with importing setting that you want eg with a defined data type for value column

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
write_csv(VF, path = paste0("W:/VF/Eden_Valley/logged_VF_data/Converted data/collar logs/test/", "VF_", mydir, ".csv")) 
}

VF_20190528 <- import_function("20190528")


glimpse(VF_20190528)

########################################################################################################################
#1.filter out data that is just for the InclusionBorder_m
#2.change the value to double
#3.do projections

#1.filter out data that is just for the InclusionBorder_m
#and                                       
#2.change the value to double         


VF_20190528_InclusionBord <- filter(VF_20190528, event == "InclusionBorder_m") %>% 
                              mutate( value = as.double(value)) 

#3.do projections
########################## set up coods ##################################  

#https://spatialreference.org/ref/epsg/gda94-mga-zone-56/
#epsg projection 28356

mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

####################  convert lat and longs to x and Y    ##########################################
coordinates(VF_20190528_InclusionBord) <- ~ lon + lat
proj4string(VF_20190528_InclusionBord) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_20190528_InclusionBord_1 <- spTransform(VF_20190528_InclusionBord, mapCRS)
#make new df_1
VF_20190528_InclusionBord = as.data.frame(VF_20190528_InclusionBord_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_20190528_InclusionBord <- mutate(VF_20190528_InclusionBord,POINT_X = lon,  POINT_Y = lat )
glimpse(VF_20190528_InclusionBord)



########################  display data ######################## 
ggplot (VF_20190528_InclusionBord, aes( hms, value))+
  geom_point()+
  facet_grid(.~ collar)
