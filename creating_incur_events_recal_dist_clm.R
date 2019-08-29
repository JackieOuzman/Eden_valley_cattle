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
getwd()
VF1_data_pts <- read.csv("//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/Cattle_pearcey/collar logs_download2/sp_VF1_InclusionBord_animalID.csv")
#make it generic 

VFX_data_pts <- VF1_data_pts
#set the date clm as date time
VFX_data_pts <- mutate(VFX_data_pts,
                       time = as_datetime(time))

#some chcek on the data.
summary(VFX_data_pts)
head(VFX_data_pts)
table(VFX_data_pts$animal_ID)
str(VFX_data_pts$time)





#########         create a subset of data to play with - just one animal
#sub_VFX_data_pts <- VFX_data_pts %>% 
#  filter(animal_ID == "Q46")
#head(sub_VFX_data_pts)

#sub2_VFX_data_pts <- VFX_data_pts %>% 
#  filter(animal_ID == "Q46"|
#         animal_ID == "Q10")
#head(sub2_VFX_data_pts)
#table(sub2_VFX_data_pts$animal_ID)
####################################################################################################################################
################                          TRy something a bit different                 ############################################
####################################################################################################################################
#I want to create a clm called start this will have value in the row > 0 and value in the previous row <0
#I want to create a clm called end this will have value in the row > 0 and value in the previous row >0
#then I will do occurance search on start and end 


# this first bit looks for values indicating cattle have moved in to exclusion zone
# then checks if the value before was negative was the animal in the grazing zone


###############################################################################################################################################
###############################################################################################################################################
############################ will this work with multiple animal? 
##############################################################################################################################################

#1) arrange function 

#arrange by time
VFX_data_pts <- arrange(VFX_data_pts, animal_ID, time )
# define a satrt and end time for event
VFX_data_pts <- mutate(VFX_data_pts,
                         start_end = case_when(
                           distance_VF >=0  & lag(distance_VF <=0) ~ "start", 
                           distance_VF <=0  & lag(distance_VF >=0) ~ "end"),
                         start_end_no_fill = case_when(
                           distance_VF >=0  & lag(distance_VF <=0) ~ "start", 
                           distance_VF <=0  & lag(distance_VF >=0) ~ "end"))


dim(VFX_data_pts)
#arrange data on time and animal
VFX_data_pts <- arrange(VFX_data_pts, animal_ID, time )
#fill in missing values from above
VFX_data_pts <- fill(VFX_data_pts, start_end, .direction = "down")
head(VFX_data_pts)
#check that the fill function has worked
table(VFX_data_pts$start_end_no_fill)
table(VFX_data_pts$start_end)


#need to code my na as other I need as na for the fill function above
VFX_data_pts$start_end <- replace_na(VFX_data_pts$start_end, "temp")

VFX_data_pts <- mutate(VFX_data_pts,
                event = case_when(
                  start_end == "start" ~ "exclusion_zone",
                  start_end == "temp" ~ "grazing_zone",
                  start_end == "end" ~ "grazing_zone"))

#tidy up
head(VFX_data_pts)
VFX_data_pts <- select(VFX_data_pts,
                       X, Y, time, event, value, hdop, heading, m.s, collar_ID, collar,
                       date, month, day, dist, animal_ID, non_graz, distance_VF, start_end, start_end_no_fill)
table(VFX_data_pts$event)
with(VFX_data_pts,table(event,non_graz)) 
##########################################################################################################################
################              Create a new clm caled event number             ###################################
##########################################################################################################################
#this makes a new df with new clm called Index that index the start and end values using no fill clm
str(VFX_data_pts)

#This is indexing all animal and then the event start - not sure if I need to add day here too?
#should I add arrange here?
VFX_data_pts <-  group_by(VFX_data_pts, animal_ID, start_end_no_fill) %>% 
  mutate(Index=1:n())  


VFX_data_pts <- mutate(VFX_data_pts,
                index_start = case_when(
                  start_end_no_fill == "start" ~ as.character(Index)))

VFX_data_pts <- data.frame(ungroup(VFX_data_pts) )

VFX_data_pts <- arrange(VFX_data_pts, animal_ID, time )

VFX_data_pts <- fill(VFX_data_pts, index_start, .direction = "down")


VFX_data_pts <- mutate(VFX_data_pts,
                event_number = case_when(
                  start_end == "temp"  ~ "-999", 
                  start_end == "end"  ~ "-999",
                  start_end ==  "start"  ~ index_start))
#check 
with(VFX_data_pts,table(event,index_start))
table(VFX_data_pts$index_start)
summary(VFX_data_pts$index_start)
check <- filter(VFX_data_pts, event_number == "-999")
check2 <- filter(VFX_data_pts, event_number != "-999") #no na here
#### remove the working out clms
VFX_data_pts_sub <- select(VFX_data_pts, -index_start, -Index, -start_end_no_fill, -start_end)

VFX_data_pts_sub$event_number <- na_if(VFX_data_pts_sub$event_number, "-999") #changed all these values to na
#add in the hms
VFX_data_pts_sub <- mutate(VFX_data_pts_sub,
                    hms = hms::as.hms(time, tz="GMT"))
head(VFX_data_pts_sub)
###############################################################################################################
############ End data set ####################################################################################

#VF1
write.csv(VFX_data_pts_sub,VF1_InclusionBord_animalID_inc_event.csv )
#VF2
write.csv(VFX_data_pts_sub,VF2_InclusionBord_animalID_inc_event.csv )
#VF3
write.csv(VFX_data_pts_sub,VF3_InclusionBord_animalID_inc_event.csv )
#VF4
write.csv(VFX_data_pts_sub,VF4_InclusionBord_animalID_inc_event.csv )
#VF5
write.csv(VFX_data_pts_sub,VF5_InclusionBord_animalID_inc_event.csv )

#####    summary stats ###################################################################################################################




head(VFX_data_pts_sub)

event_sum <- filter(VFX_data_pts_sub, event_number != "NA") %>% 
            group_by( day, animal_ID, event_number) %>% 
            summarise(max_dist = max(distance_VF ), 
            mean_dis = mean(distance_VF ),
            max_time = max(as_datetime(time, tz="GMT")), 
            min_time = min(as_datetime(time, tz="GMT")),
            period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))



            
print(event_sum)
