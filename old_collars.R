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
#######2019 05 17 ########
ac213_20190517<- read_csv("P:/Damian/Jim Eden Valley/Converted data/collar logs/20190517/ac213.csv") %>% 
  mutate(file_name = "ac213_20190517") 
ad3396_20190517<- read_csv("P:/Damian/Jim Eden Valley/Converted data/collar logs/20190517/ad3396.csv") %>% 
  mutate(file_name = "ad3396_20190517")
ac219_20190517<- read_csv("P:/Damian/Jim Eden Valley/Converted data/collar logs/20190517/ac219.csv") %>% 
  mutate(file_name = "ac219_20190517")

#######2019 05 18 ########
ac213_20190518<- read_csv("P:/Damian/Jim Eden Valley/Converted data/collar logs/20190518/ac213.csv") %>% 
  mutate(file_name = "ac213_20190518") 
ad3396_20190518<- read_csv("P:/Damian/Jim Eden Valley/Converted data/collar logs/20190518/ad3396.csv") %>% 
  mutate(file_name = "ad3396_20190518")
ac219_20190518<- read_csv("P:/Damian/Jim Eden Valley/Converted data/collar logs/20190518/ac219.csv") %>% 
  mutate(file_name = "ac219_20190518")
################## Step 2 join the raw logged data   ##############################################
cattle_merge20190517_8 <- rbind(ac213_20190517, ad3396_20190517, ac219_20190517,
                              ac213_20190518, ad3396_20190518, ac219_20190518)

################## Step 3 extra clms for  raw logged data   ##############################################
cattle_merge20190517_8 <- cattle_merge20190517_8 %>% 
  separate(file_name,into =  c("collar", "date"),  sep = "_", remove = FALSE ) %>% 
  mutate(hms = hms::as.hms(time, tz="GMT"),
         date= date(time),
         month = month(time),
         day = day(time))
glimpse(cattle_merge20190517_8)

#################### step 4 convert lat and longs to x and Y    ##########################################
coordinates(cattle_merge20190517_8) <- ~ lon + lat
proj4string(cattle_merge20190517_8) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
cattle_merge20190517_8_1 <- spTransform(cattle_merge20190517_8, mapCRS)
#make new df_1
cattle_merge20190517_8 = as.data.frame(cattle_merge20190517_8_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
cattle_merge20190517_8 <- mutate(cattle_merge20190517_8,POINT_X = lon,  POINT_Y = lat )
glimpse(cattle_merge20190517_8)
#############################    step 3 plot data                        #########################################
ggplot (cattle_merge20190517_8, aes( hms, value))+
  geom_point()+
  facet_wrap(day~ collar)



####### Mess about bringing many files ####
#library(plyr)
#library(readr)
#"P:/Damian/Jim Eden Valley/Converted data/collar logs/20190518"



setwd("P:/Damian/Jim Eden Valley/Converted data/collar logs/")
mydir = "20190518"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

#dat_csv = ldply(myfiles, read_csv)
#dat_csv

### Get file names
filenames <- myfiles
collars <- str_extract(myfiles, "[a-z]+\\d{1,6}")

### Get length of each csv
file_lengths <- unlist(lapply(lapply(filenames, read_csv), nrow))

### Repeat sites using lengths
file_names <- rep(collars,file_lengths)

### Create table
tbl <- lapply(filenames, read_csv) %>% 
  bind_rows()

### Combine file_names and tbl
VF2019_05_18 <- cbind(tbl, collar_ID = file_names)
glimpse(VF2019_05_18)
