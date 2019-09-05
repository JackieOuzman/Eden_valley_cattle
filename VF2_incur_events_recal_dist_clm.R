##################################################################################################################################

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

## The aim here is create a clm and then maybe a subset of data for an incursion event
# this is when the animal has moved into the exclusion zone.

#bring in my data

##################             VF2         ###################################################################
VF2_InclusionBord_animalID <- read.csv(
  "//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/catlle_pearcey_recal_dist/Re_cal/sp_VF2_InclusionBord_animalID.csv")
#head(VF2_InclusionBord_animalID)

#assign WGS EPSG for coords for each of the VF dataframes
VF2_InclusionBord_animalID$date <- as_date(VF2_InclusionBord_animalID$date)
VF2_InclusionBord_animalID$time <- as_datetime(VF2_InclusionBord_animalID$time, tz="GMT")
VF2_InclusionBord_animalID <- mutate(VF2_InclusionBord_animalID,
                                     hms = hms::as.hms(time, tz="GMT"))
#assign coords for each of the VF dataframes
sp_VF2_InclusionBord_animalID <- st_as_sf(VF2_InclusionBord_animalID, 
                                          coords = c("X", "Y"), 
                                          crs = the_crs)
#############################################################################################################


#1) arrange function 

#arrange by time
sp_VF2_InclusionBord_animalID <- arrange(sp_VF2_InclusionBord_animalID, animal_ID, time )
# define a satrt and end time for event
sp_VF2_InclusionBord_animalID <- mutate(sp_VF2_InclusionBord_animalID,
                       start_end = case_when(
                         distance_VF >=0  & lag(distance_VF <=0) ~ "start", 
                         distance_VF <=0  & lag(distance_VF >=0) ~ "end"),
                       start_end_no_fill = case_when(
                         distance_VF >=0  & lag(distance_VF <=0) ~ "start", 
                         distance_VF <=0  & lag(distance_VF >=0) ~ "end"))


dim(sp_VF2_InclusionBord_animalID)
#arrange data on time and animal
sp_VF2_InclusionBord_animalID <- arrange(sp_VF2_InclusionBord_animalID, animal_ID, time )
#fill in missing values from above
sp_VF2_InclusionBord_animalID <- fill(sp_VF2_InclusionBord_animalID, start_end, .direction = "down")
head(sp_VF2_InclusionBord_animalID)
#check that the fill function has worked
table(sp_VF2_InclusionBord_animalID$start_end_no_fill)
table(sp_VF2_InclusionBord_animalID$start_end)


#need to code my na as other I need as na for the fill function above
sp_VF2_InclusionBord_animalID$start_end <- replace_na(sp_VF2_InclusionBord_animalID$start_end, "temp")

sp_VF2_InclusionBord_animalID <- mutate(sp_VF2_InclusionBord_animalID,
                       event = case_when(
                         start_end == "start" ~ "exclusion_zone",
                         start_end == "temp" ~ "grazing_zone",
                         start_end == "end" ~ "grazing_zone"))

#tidy up
head(sp_VF2_InclusionBord_animalID)
sp_VF2_InclusionBord_animalID <- select(sp_VF2_InclusionBord_animalID,
                      time, event, value, hdop, heading, m.s, collar_ID, collar,
                       date, month, day, dist, animal_ID, non_graz, distance_VF, start_end, start_end_no_fill, geometry)
table(sp_VF2_InclusionBord_animalID$event)
with(sp_VF2_InclusionBord_animalID,table(event,non_graz)) 
##########################################################################################################################
################              Create a new clm caled event number             ###################################
##########################################################################################################################
#this makes a new df with new clm called Index that index the start and end values using no fill clm
str(sp_VF2_InclusionBord_animalID)

#This is indexing all animal and then the event start - not sure if I need to add day here too?
#should I add arrange here?
sp_VF2_InclusionBord_animalID <-  group_by(sp_VF2_InclusionBord_animalID, animal_ID, start_end_no_fill) %>% 
  mutate(Index=1:n())  


sp_VF2_InclusionBord_animalID <- mutate(sp_VF2_InclusionBord_animalID,
                       index_start = case_when(
                         start_end_no_fill == "start" ~ as.character(Index)))

sp_VF2_InclusionBord_animalID <- data.frame(ungroup(sp_VF2_InclusionBord_animalID) )

sp_VF2_InclusionBord_animalID <- arrange(sp_VF2_InclusionBord_animalID, animal_ID, time )

sp_VF2_InclusionBord_animalID <- fill(sp_VF2_InclusionBord_animalID, index_start, .direction = "down")


sp_VF2_InclusionBord_animalID <- mutate(sp_VF2_InclusionBord_animalID,
                       event_number = case_when(
                         start_end == "temp"  ~ "-999", 
                         start_end == "end"  ~ "-999",
                         start_end ==  "start"  ~ index_start))
#check 
with(sp_VF2_InclusionBord_animalID,table(event,index_start))
table(sp_VF2_InclusionBord_animalID$index_start)
summary(sp_VF2_InclusionBord_animalID$index_start)
check <- filter(sp_VF2_InclusionBord_animalID, event_number == "-999")
check2 <- filter(sp_VF2_InclusionBord_animalID, event_number != "-999") #no na here
#### remove the working out clms
sp_VF2_InclusionBord_animalID_sub <- select(sp_VF2_InclusionBord_animalID, 
                                            -index_start, -Index, -start_end_no_fill, -start_end)

sp_VF2_InclusionBord_animalID_sub$event_number <- na_if(sp_VF2_InclusionBord_animalID_sub$event_number, "-999") #changed all these values to na
#add in the hms
sp_VF2_InclusionBord_animalID_sub <- mutate(sp_VF2_InclusionBord_animalID_sub,
                           hms = hms::as.hms(time, tz="GMT"))
head(sp_VF2_InclusionBord_animalID_sub)
###############################################################################################################
############ End data set ####################################################################################

#VF2
#write.csv(sp_VF2_InclusionBord_animalID_sub,"VF2_incur_events.csv" )
#write.csv(sp_VF2_InclusionBord_animalID_sub, 
#  "//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/catlle_pearcey_recal_dist/Re_cal/VF2_incur_events.csv")



st_write(sp_VF2_InclusionBord_animalID_sub,
         "//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/catlle_pearcey_recal_dist/Re_cal/VF2_incur_events.csv",
         layer_options = "GEOMETRY=AS_XY")

#####    summary stats ###################################################################################################################

#just a chcek but I have this in the step 3 graphs


event_sum <- filter(sp_VF2_InclusionBord_animalID_sub, event_number != "NA") %>% 
  group_by( day, animal_ID, event_number) %>% 
  summarise(max_dist = max(distance_VF ), 
            mean_dis = mean(distance_VF ),
            max_time = max(as_datetime(time, tz="GMT")), 
            min_time = min(as_datetime(time, tz="GMT")),
            period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))




print(event_sum)

##################### END ################################################################################################























