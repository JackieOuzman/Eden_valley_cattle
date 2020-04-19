
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
library(parallel)


################################################################################################################
########################setting up functions to import the data  ########################

#########################################################################################
#### 1. RAW DATA FOLDER AND WORKING DIRECTORY

#locate the raw data folder
#The location of my data will depend on the computer I am working on...

what_computer1 <- function(computer) {
  if_else(
    computer == "not_pearacey",
    name_of_path <-
      file.path("W:", "VF", "Eden_valley", "logged_VF_data", "updated collar logs"),
    name_of_path <-
      file.path(
        "home",
        "ouz001",
        "VF_cattle",
        "catlle_pearcey_recal_dist",
        "Re_cal"
      )
  )
  #print(name_of_path)
}

name_of_path <- what_computer1("not_pearacey")
#name_of_path <- what_computer1("pearacey")

setwd(name_of_path)
getwd()

#########################################################################################
#### 2. SET UP FUNCTION TO IMPORT THE RAW DATA

### Function 1 read_CSV#####

read_csv_FUN <- function(file) {
  the_data <-
    read_csv(file, col_types = cols(value = col_character()))
}


#### Function 2 import the data with correct names ####

import_function <- function(mydir) {
  myfiles = list.files(path = mydir,
                       pattern = "*.csv",
                       full.names = TRUE)
  #1a########### Get file names
  filenames <- myfiles
  collars <- str_extract(myfiles, "[a-z]+\\d{1,6}")
  
  #1b########## Get length of each csv
  file_lengths <- unlist(lapply(lapply(filenames, read_csv), nrow))
  #file_lengths
  #1c########## Repeat collars using lengths
  file_names <- rep(collars, file_lengths)
  
  #1d######### Create table
  tbl <-
    lapply(filenames, read_csv_FUN) %>% #this call the function read_csv_FUN I made outside this import_function
    bind_rows()
  #1e######### Combine file_names and tbl
  VF <- cbind(tbl, collar_ID = file_names)
  #glimpse(VF)
  
  #### Step 2 extra clms for  raw logged data   
  VF <- VF %>%
    separate(
      collar_ID,
      into =  c("collar", "date"),
      sep = "_",
      remove = FALSE
    ) %>%
    mutate(
      hms = hms::as.hms(time, tz = "GMT"),
      date = date(time),
      month = month(time),
      day = day(time)
    )
}


#########################################################################################
#### 3. USE THE FUNCTION TO BRING IN RAW DATA

### 3a. group the files together - here its by fence 

Fence1 <-
  c(#"20190517",
    #"20190518",
    #"20190519",
    "20190520",
    "20190521")
Fence2 <-
  c("20190521",
    "20190522",
    "20190523")
Fence3 <-
  c(
    "20190523",
    "20190524",
    "20190525",
    "20190526",
    "20190527",
    "20190528")
Fence4 <-
  c(
    "20190528",
    "20190529",
    "20190530",
    "20190531",
    "20190601",
    "20190602",
    "20190603")

Fence5 <-
  c(
    "20190603",
    "20190604",
    "20190605",
    "20190606",
    "20190607",
    "20190608",
    "20190609",
    "20190610",
    "20190611",
    "20190612",
    "20190613",
    "20190614",
    "20190615",
    "20190616",
    "20190617",
    "20190618",
    "20190619",
    "20190620",
    "20190621",
    "20190622",
    "20190623",
    "20190624",
    "20190625",
    "20190626",
    "20190627",
    "20190628",
    "20190629",
    "20190630",
    "20190701",
    "20190702"
    )



### 3b. set up computer to run code in non parellael / and parallel

#Not parallel
Fence1_import <- import_function(Fence1)
Fence2_import <- import_function(Fence2)
Fence3_import <- import_function(Fence3)
Fence4_import <- import_function(Fence4)
Fence5_import <- import_function(Fence5)



detectCores() #I have 8
#specfiy the number of cores and create a cluster object
cl <- makeCluster(6) #also could do 7 or 8
#export the functions

clusterExport(cl, c("read_csv_FUN", "import_function"))
clusterEvalQ(cl, {
  library(magrittr)
  library(readr)
  library(tidyverse)
  library(dplyr)
  library(lubridate)
  library(readxl)
  library(parallel)
})



#swap to parallel version of function This is not working now??

# Fence1_import <- data.frame(unlist(parLapply(cl, Fence1, import_function)))
# stopCluster(cl)
# Fence2_import  <- data.frame(unlist(parLapply(cl, Fence2, import_function)))
# stopCluster(cl)
# Fence3_import  <- data.frame(unlist(parLapply(cl, Fence3, import_function)))
# stopCluster(cl)
# Fence4_import  <- data.frame(unlist(parLapply(cl, Fence4, import_function)))
# stopCluster(cl)
# Fence5_import  <- data.frame(unlist(parLapply(cl, Fence5, import_function)))
# stopCluster(cl)


#########################################################################################
#### 4. DIVIDE THE DATA INTO FENCES to the correct time of change

#What is the max time?
max_time_df <- as_datetime(max(Fence1_import$time), tz="GMT") 
print(max_time_df)


#Fence 1 called training fence eden valley
Fence1_time <- filter(Fence1_import,
                      between(
                        time,
                        as_datetime('2019-05-20 10:15:00', tz = "GMT"),
                        as_datetime('2019-05-20 14:40:00', tz = "GMT")
                      )) %>%
                        mutate(VF = "VF1")

#Fence 2 called training mk1
Fence2_time <- filter(Fence2_import,
                      between(
                        time,
                        as_datetime('2019-05-20 14:50:00', tz = "GMT"),
                        as_datetime('2019-05-23 08:30:00', tz =
                                      "GMT")
                      ) %>%
                        mutate(VF = "VF2"))

#Fence 3 called bron next traing fence
Fence3_time <- filter(Fence3_import,
                      between(
                        time,
                        as_datetime('2019-05-23 08:30:00', tz = "GMT"),
                        as_datetime('2019-05-28 11:00:00', tz =
                                      "GMT")
                      ) %>%
                        mutate(VF = "VF3"))

#Fence 4 called bron next traing fence check that the time range is working
Fence4_time <- filter(Fence4_import,
                      between(
                        time,
                        as_datetime('2019-05-28 11:15:00', tz = "GMT"),
                        as_datetime('2019-06-03 09:30:00', tz =
                                      "GMT")
                      ) %>%
                        mutate(VF = "VF4"))

#Fence 5 called bron next traing fence Cant don this yet without the full data set
Fence5_time <- filter(Fence5_import,
                      between(
                        time,
                        as_datetime('2019-06-03 09:31:00', tz = "GMT"),
                        as_datetime('2019-07-02 06:11:00', tz =
                                      "GMT")
                      ) %>%
                        mutate(VF = "VF5"))

#########################################################################################
#### 5. Remove the NA in the data AND keep only data with "InclusionBorder_m" and set as double

#list of data
list_data <- c(Fence1_time,
               Fence2_time,
               Fence3_time,
               Fence4_time,
               Fence5_time)
#function
remove_na_InclusionBorad_m <- function(list_data){
  list_data %>% filter(!is.na(lat) | !is.na(lon)) %>% 
    filter( event == "InclusionBorder_m") %>%   
    mutate( value = as.double(value))
}

#apply function without lapply
Fence1_Incl_board_m <- remove_na_InclusionBorad_m(Fence1_time)


#lapply to apply function to elements in my list
lapply(list_data, remove_na_InclusionBorad_m)
#if this works change it to parallel
#not sure the returned outout is what I want...??
list_data <- data.frame(unlist(parLapply(cl, list_data, remove_na_InclusionBorad_m)))

# VF_week1_2_3 <- VF_week1_2_3 %>% filter(!is.na(lat) | !is.na(lon))
# 
# summary(VF_week1_2_3$lat)
# summary(VF_week1_2_3$lon)






