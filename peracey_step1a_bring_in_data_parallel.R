
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
                        as_datetime('2019-05-23 08:30:00', tz = "GMT")
                      )) %>%
                        mutate(VF = "VF2")

#Fence 3 called bron next traing fence
Fence3_time <- filter(Fence3_import,
                      between(
                        time,
                        as_datetime('2019-05-23 08:30:00', tz = "GMT"),
                        as_datetime('2019-05-28 11:00:00', tz = "GMT")
                      )) %>%
                        mutate(VF = "VF3")

#Fence 4 called bron next traing fence check that the time range is working
Fence4_time <- filter(Fence4_import,
                      between(
                        time,
                        as_datetime('2019-05-28 11:15:00', tz = "GMT"),
                        as_datetime('2019-06-03 09:30:00', tz = "GMT")
                      )) %>%
                        mutate(VF = "VF4")

#Fence 5 called bron next traing fence Cant don this yet without the full data set
Fence5_time <- filter(Fence5_import,
                      between(
                        time,
                        as_datetime('2019-06-03 09:31:00', tz = "GMT"),
                        as_datetime('2019-07-02 06:11:00', tz = "GMT")
                      )) %>%
                        mutate(VF = "VF5")



#################################################################################################
# 5. recode the collar ID to animal ID
# 5a. VF 1
Fence1_Incl_animalID <- mutate(Fence1_time,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-28 06:44:00', tz="GMT")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac219" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-25 11:10:00', tz="GMT"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(time, as_datetime('2019-05-25 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:18', tz="GMT"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-27 16:19:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

# 5b. VF 2
Fence2_Incl_animalID <- mutate(Fence2_time,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-28 06:44:00', tz="GMT")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac219" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-25 11:10:00', tz="GMT"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(time, as_datetime('2019-05-25 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:18', tz="GMT"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-27 16:19:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

# 5c. VF 3
Fence3_Inc_animalID <- mutate(Fence3_time,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-28 07:00:00', tz="GMT")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac219" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-25 11:20:00', tz="GMT"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(time, as_datetime('2019-05-25 10:55:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:18', tz="GMT"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-27 16:25:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

# 5d. VF 4
Fence4_Inc_animalID <- mutate(Fence4_time,
                                        animal_ID = case_when(
                                          collar_ID == "ac138"
                                          ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-28 06:44:00', tz="GMT")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac219" &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-25 11:10:00', tz="GMT"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(time, as_datetime('2019-05-25 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:18', tz="GMT"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                                    as_datetime('2019-05-27 16:19:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

# 5e. VF 5
# 5e.i The collar were changed over on the 7/6/2019 at 15:34, before they were changed over the collar were deactivated
# This means the logging events that occured between 6/6/2019 16:35 and 7/6/2019 15:35 should be removed

Fence5_time_test <- filter(Fence5_time,  !between(
  time,as_datetime('2019-06-06 16:35:00', tz = "GMT"),as_datetime('2019-06-07 16:35:00', tz =
                                                                      "GMT")
))
# # other data that needs removing according to notes

Fence5_Inc_animalID <- mutate(Fence5_time_test,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac207" &
                                            between(time, as_datetime('2019-06-03 09:30:00', tz="GMT"),
                                                    as_datetime('2019-06-07 16:30:00', tz="GMT")) ~ "Q42" ,
                                          
                                          collar_ID == "ac209"  &
                                            between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac220" &
                                            between(time, as_datetime('2019-05-25 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:18', tz="GMT"))~ "Q10",
                                          collar_ID == "ac320" &
                                            between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                    as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                          collar_ID == "ac325" &
                                            between(time, as_datetime('2019-06-03 09:30:00', tz="GMT"),
                                                    as_datetime('2019-06-07 16:30:00', tz="GMT")) ~ "Q9" ,
                                          
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" &
                                            between(time, as_datetime('2019-06-03 09:34:00', tz="GMT"),
                                                    as_datetime('2019-06-07 16:30:00', tz="GMT")) ~ "Q75" ,
                                          collar_ID == "ad2633" ~ "Q8",
                                          collar_ID == "ad2634" ~ "Q11",
                                          collar_ID == "ad2635" ~ "Q47",
                                          collar_ID == "ad2637" ~ "Q108",
                                          collar_ID == "ad2638" ~ "Q27",
                                          collar_ID == "ad2639" ~ "Q51",
                                          
                                          #collar_ID == "ad2640" ~ "Q36",
                                          collar_ID == "ad2640" &
                                            between(time, as_datetime('2019-06-07 19:07:00', tz="GMT"),
                                                    as_datetime('2019-06-21 10:00:00', tz="GMT")) ~ "Q36" ,
                                          collar_ID == "ad2640" &
                                            between(time, as_datetime('2019-06-21 14:05:00', tz="GMT"),
                                                    as_datetime('2019-06-24 16:06:00', tz="GMT")) ~ "Q36" ,
                                          
                                          collar_ID == "ad2643" ~ "Q36",
                                          collar_ID == "ad2644" ~ "Q46",
                                          collar_ID == "ad2645" ~ "Q42",
                                          collar_ID == "ad2646" ~ "Q9",
                                          collar_ID == "ad2647" ~ "Q10",
                                          collar_ID == "ad2648" ~ "Q109",
                                          collar_ID == "ad2649" ~ "Q29",
                                          collar_ID == "ad2650" ~ "Q26",
                                          collar_ID == "ad2651" ~ "Q15",
                                          collar_ID == "ad2653" ~ "Q75",
                                          collar_ID == "ad2654" ~ "Q45",
                                          collar_ID == "ad2655" &
                                            between(time, as_datetime('2019-06-07 16:35:00', tz="GMT"),
                                                    as_datetime('2019-06-24 14:04:00', tz="GMT")) ~ "Q26" ,
                                          collar_ID == "ad2656" ~ "Q28",
                                          collar_ID == "ad2657" ~ "Q110",
                                          collar_ID == "ad2658" ~ "Q2",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          collar_ID == "ad3502" ~ "Q8",
                                          
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3374" ~ "Q11",
                                          
                                           
                                         
                                          
                                          TRUE ~ "NA"))


###########################################################################################################
# 6. add a colm for day of exp
# 6a. write a function to do this

date_of_trial <- function(df){
  mutate(df, 
         day_since_start = case_when(
           date == "2019-05-20" ~ 1,
           date == "2019-05-21" ~ 2,
           date == "2019-05-22" ~ 3,
           date == "2019-05-23" ~ 4,
           date == "2019-05-24" ~ 5,
           date == "2019-05-25" ~ 6,
           date == "2019-05-26" ~ 7,
           date == "2019-05-27" ~ 8,
           date == "2019-05-28" ~ 9,
           date == "2019-05-29" ~ 10,
           date == "2019-05-30" ~ 11,
           date == "2019-05-31" ~ 12,
           date == "2019-06-01" ~ 13,
           date == "2019-06-02" ~ 14,
           date == "2019-06-03" ~ 15,
           date == "2019-06-04" ~ 16,
           date == "2019-06-05" ~ 17,
           date == "2019-06-06" ~ 18,
           date == "2019-06-07" ~ 19,
           date == "2019-06-08" ~ 20,
           date == "2019-06-09" ~ 21,
           date == "2019-06-10" ~ 22,
           date == "2019-06-11" ~ 23,
           date == "2019-06-12" ~ 24,
           date == "2019-06-13" ~ 25,
           date == "2019-06-14" ~ 26,
           date == "2019-06-15" ~ 27,
           date == "2019-06-16" ~ 28,
           date == "2019-06-17" ~ 29,
           date == "2019-06-18" ~ 30,
           date == "2019-06-19" ~ 31,
           date == "2019-06-20" ~ 32,
           date == "2019-06-21" ~ 33,
           date == "2019-06-22" ~ 34,
           date == "2019-06-23" ~ 35,
           date == "2019-06-24" ~ 36,
           date == "2019-06-25" ~ 37,
           date == "2019-06-26" ~ 38,
           date == "2019-06-27" ~ 39,
           date == "2019-06-28" ~ 40,
           date == "2019-06-29" ~ 41,
           date == "2019-06-30" ~ 42,
           date == "2019-07-01" ~ 43,
           date == "2019-07-02" ~ 44
         ))
}

# 6b. call function to add day of trial
Fence1_Incl_animalID <- date_of_trial(Fence1_Incl_animalID)
Fence2_Incl_animalID <- date_of_trial(Fence2_Incl_animalID)
Fence3_Incl_animalID <- date_of_trial(Fence3_Inc_animalID)
Fence4_Incl_animalID <- date_of_trial(Fence4_Inc_animalID)
Fence5_Incl_animalID <- date_of_trial(Fence5_Inc_animalID)

#########################################################################################
#### 7. add in a clm for week of trial

week_of_trial <- function(df) {
  mutate(
    df,
    week_number = case_when(
      day_since_start  < 7 ~ 1,
      day_since_start  < 14 ~ 2,
      day_since_start   < 21 ~ 3,
      day_since_start   < 28 ~ 4,
      day_since_start   < 35 ~ 5,
      day_since_start    < 42 ~ 6,
      day_since_start   < 45 ~ 7
    )
  )
}

Fence1_Incl_animalID <- week_of_trial(Fence1_Incl_animalID)
Fence2_Incl_animalID <- week_of_trial(Fence2_Incl_animalID)
Fence3_Incl_animalID <- week_of_trial(Fence3_Incl_animalID)
Fence4_Incl_animalID <- week_of_trial(Fence4_Incl_animalID)
Fence5_Incl_animalID <- week_of_trial(Fence5_Incl_animalID)

tail(Fence5_Incl_animalID)

#########################################################################################
#### 8. Remove the NA in the data AND keep only data with "InclusionBorder_m" and set as double

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

Fence1_Incl <- remove_na_InclusionBorad_m(Fence1_time)
Fence2_Incl <- remove_na_InclusionBorad_m(Fence2_time)
Fence3_Incl <- remove_na_InclusionBorad_m(Fence3_time)
Fence4_Incl <- remove_na_InclusionBorad_m(Fence4_time)
Fence5_Incl <- remove_na_InclusionBorad_m(Fence5_time)

#write out files
output_folder <- file.path("W:", "VF", "Eden_valley", "logged_VF_data", "Jax_Dec_2019_processing")
Fence1 <- paste0(output_folder,"/Fence1_incl.rds")
Fence2 <- paste0(output_folder,"/Fence2_incl.rds")
Fence3 <- paste0(output_folder,"/Fence3_incl.rds")
Fence4 <- paste0(output_folder,"/Fence4_incl.rds")
Fence5 <- paste0(output_folder,"/Fence5_incl.rds")

write_rds(Fence1_Incl, Fence1)
write_rds(Fence2_Incl, Fence2)
write_rds(Fence3_Incl, Fence3)
write_rds(Fence4_Incl, Fence4)
write_rds(Fence5_Incl, Fence5)
