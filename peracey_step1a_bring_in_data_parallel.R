
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



###Function 1 read_CSV#####
read_csv_FUN <- function(file ){
  the_data <- read_csv(file, col_types = cols(value = col_character()))
}

#location_raw_data <- "//FSSA2-ADL/clw-share1/Microlab/VF/Eden_Valley/logged_VF_data/collar logs_download2"
#check <- paste0(location_raw_data)
#check
#setwd(check)
#getwd()

####Function 2 import the data with correct names ####
import_function <- function(location_raw_data, mydir){
  
  #enter the correct path for where raw data sits
  
  setwd(paste0(location_raw_data)) #check that this works
  
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

#VF_week1 <- c("20190517", "20190518", "20190519",
#                  "20190520", "20190521", "20190522", "20190523")
VF_week1 <- c("20190517", "20190518")


VF_week2 <- c("20190524", "20190525", "20190526",
                  "20190527", "20190528", "20190529", "20190530")
VF_week3 <- c("20190531", "20190601", 
                  "20190602", 
                  "20190603", "20190604", "20190605", "20190606")

system.time(week1 <- data.frame(unlist(lapply(VF_week1, import_function))))
#can I turn this into pararell?

detectCores() #I have 8
#specfiy the number of cores and create a cluster object
cl <- makeCluster(6) #also could do 7 or 8
#export the functions

#clusterExport(cl, c("read_csv_FUN", "import_function", "str_extract", "read_csv"))
clusterExport(cl, c("read_csv_FUN", "import_function"))
clusterEvalQ(cl, {
  library(magrittr)
  library(readr)
  library(tidyverse)
  library(dplyr)
  library(lubridate)
  library(readxl)
  
})
#swap tp parallel version of function
system.time(week1_a <- data.frame(unlist(parLapply(cl, VF_week1, import_function))))
stopCluster(cl)

##### Use the function to bring in data for one day that is specified ######
getwd()
#setwd("collar logs_download2/")
setwd("//FSSA2-ADL/clw-share1/Microlab/VF/Eden_Valley/logged_VF_data/collar logs_download2")




################################################################################################################
##########       Merge this all togther   ##########       


VF_week1_2_3 <- rbind(VF_week1, VF_week2, VF_week3)
head(VF_week1_2_3)
getwd()
saveRDS(VF_week1_2_3,  "VF_week1_2_3.rds")
write_csv(VF_week1_2_3, "VF_week1_2_3.csv")
#just for checking
#Fence 1 called training fence eden valley
VF1_check_data <- filter(VF_week1_2_3, 
                            between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                    as_datetime('2019-05-20 14:40:00', tz="GMT")))
write_csv(VF1_check_data, "VF1_check_data.csv")
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







