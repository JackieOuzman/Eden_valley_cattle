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


head(VF_week1_2_3)
###########################################################################################################################################################
#############################            create a df for the audio                                                  #############################
###########################################################################################################################################################
#bring in the VF1 data with all the event data
table(VF_week1_2_3$event)

"Audio started"
"Audio started [simulated]"
"Audio started (short)"
"Audio started (short) [simulated]"
"Pulse started"
"Pulse started [simulated]"

Audio <- filter(VF_week1_2_3, event == "Audio started" | 
                                           event == "Audio started [simulated]" |
                                           event == "Audio started (short)" |
                                           event == "Audio started (short) [simulated]")
head(Audio)


#########    Remove the NA   ##########

Audio <- Audio %>% filter(!is.na(lat) | !is.na(lon))

summary(Audio$lat)
summary(Audio$lon)
#saveRDS(Audio,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3_Audio.rds"))


################################################################################################################
##################           Divide up the data into VF chuncks                               ##################    
################################################################################################################
#What is the max time?
max_time_Audio <- as_datetime(max(Audio$time), tz="GMT") 
print(max_time_Audio)
#Fence 1 called training fence eden valley
VF1_Audio <- filter(Audio, 
                    between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                                  as_datetime('2019-05-20 14:40:00', tz="GMT")))
#Fence 2 called training mk1
VF2_Audio <- filter(Audio, 
                    between(time, as_datetime('2019-05-20 14:50:00', tz="GMT"),
                                  as_datetime('2019-05-23 08:30:00', tz="GMT")))

#Fence 3 called bron next traing fence
VF3_Audio <- filter(Audio, 
                    between(time, as_datetime('2019-05-23 08:30:00', tz="GMT"),
                    as_datetime('2019-05-28 11:00:00', tz="GMT")))

#Fence 4 called bron next traing fence check that the time range is working
VF4_Audio <- filter(Audio, 
                    between(time, as_datetime('2019-05-28 11:15:00', tz="GMT"),
                                  as_datetime('2019-06-03 09:30:00', tz="GMT")))

#Fence 5 called bron next traing fence Cant don this yet without the full data set
#VF5_Audio <- filter(Audio, 
#                     between(time, as_datetime('2019-06-03 09:31:00', tz="GMT"),
#                                   as_datetime('2019-07-02 06:11:00', tz="GMT")))



################################################################################################################
##################           make data into spatial object                               ##################    
################################################################################################################


#bring in the boundary file as spatial data fram with sf package

getwd()
eden_valley <- st_read("EdenValley_site1GDA_a.shp")

#assign a coord ref epsg
st_crs(eden_valley) <- 28354
st_crs(eden_valley)
plot(st_geometry(eden_valley))
#make this the epsg that will be used for all data
the_crs <- st_crs(eden_valley, asText = TRUE)
#make the inclusion data into a spatial object

#assign WGS EPSG for coords for each of the VF dataframes
sp_VF1_Audio <- st_as_sf(VF1_Audio, coords = c("lon", "lat"), crs = 4326, agr = "constant")
head(sp_VF1_Audio)
str(sp_VF1_Audio)
# plot it 
sp_VF1_Audio_geom <- st_geometry(sp_VF1_Audio)
plot(sp_VF1_Audio_geom, col = "grey") 


#transfor the cattle data so its in the same data frame as the paddock boundary
sp_VF1_Audio_trans <- st_transform(sp_VF1_Audio, crs = 28354)
head(sp_VF1_Audio_trans)
head(eden_valley)
plot(st_geometry(sp_VF1_Audio_trans))
sp_VF1_Audio_trans_geo <- st_geometry(sp_VF1_Audio_trans)#data frame that is just points no attributes
#check that I have done this - looking good!looking for points to be displayed in paddock
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = sp_VF1_Audio_trans_geo)


############################   Now clip ################################################################

sp_VF1_Audio_trans_clip <- st_intersection(sp_VF1_Audio_trans, eden_valley) #message about assumed spatial consistant

#check I have done what I want Looks good

plot(st_geometry(sp_VF1_Audio_trans_clip))
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA)+
  geom_sf(data = sp_VF1_Audio_trans_clip)


#########################################################################################################
#############    Recal the distance from VF line ########################################################

#bring in the VF 
fence1 <- st_read("Fence1.shp")
st_crs(fence1) <- 28354
st_crs(fence1)
plot(st_geometry(fence1))
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)
#Try distance tool

sp_VF1_Audio_trans_clip <- mutate(sp_VF1_Audio_trans_clip, 
                                    dist = st_distance(sp_VF1_Audio_trans_clip, fence1))
head(sp_VF1_Audio_trans_clip)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)+
  geom_sf(data = sp_VF1_Audio_trans_clip)

head(sp_VF1_Audio_trans_clip$dis)  
class(sp_VF1_Audio_trans_clip$dis)
sp_VF1_Audio_trans_clip$dist <- as.double(sp_VF1_Audio_trans_clip$dist)

#########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################




sp_VF1_Audio_clip_animalID <- mutate(sp_VF1_Audio_trans_clip,
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

#check we are assignining all the collar ID to animal names
head(sp_VF1_Audio_clip_animalID)
with(sp_VF1_Audio_clip_animalID, table(date, animal_ID))

#the location of the NA
#NA_sp_VF1_Audio_clip_animalID <- filter(sp_VF1_Audio_clip_animalID,
 #                                          animal_ID == "NA")
#with(NA_sp_VF1_Audio_clip_animalID, table(date, collar_ID))

summary(sp_VF1_Audio_clip_animalID)

##################################################################################################################################
#################                         Select data points inside non grazing zone                 #############################
##################################################################################################################################
getwd()
VF1_NonGraz <- st_read("VF1_NonGraz.shp")
st_crs(VF1_NonGraz) <- 28354
st_crs(VF1_NonGraz)
plot(st_geometry(VF1_NonGraz))

#won't need this on pearcy but get the data into spatial format
#assign WGS EPSG for coords for each of the VF dataframes
#head(sp_VF1_InclusionBord_animalID)
#sp_VF1_InclusionBord_animalID <- st_as_sf(sp_VF1_InclusionBord_animalID, coords = c("X", "Y"), crs = 28354, agr = "constant")
head(sp_VF1_Audio_clip_animalID)
str(sp_VF1_Audio_clip_animalID)
dim(sp_VF1_Audio_clip_animalID)



# which points fall inside a polygon? Create a new clm for this non_graz when true its in the non grazing zone
sp_VF1_Audio_clip_animalID <- mutate(sp_VF1_Audio_clip_animalID,
                                        non_graz = apply((st_intersects(sp_VF1_Audio_clip_animalID, 
                                                                        VF1_NonGraz, sparse = FALSE)), 1, any))
head(sp_VF1_Audio_clip_animalID)
sp_VF1_Audio_clip_animalID_TRUE <- filter(sp_VF1_Audio_clip_animalID, non_graz == TRUE)
sp_VF1_Audio_clip_animalID_FALSE <- filter(sp_VF1_Audio_clip_animalID, non_graz == FALSE)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)+
  #geom_sf(data = sp_VF1_Audio_clip_animalID_FALSE)
  geom_sf(data = sp_VF1_Audio_clip_animalID_TRUE)
# add extra clm that distance from VF False values become negative and are in the grazing zone
sp_VF1_Audio_clip_animalID <- mutate(sp_VF1_Audio_clip_animalID,
                                        distance_VF =  ifelse(non_graz == FALSE, (dist*-1), dist))

head(sp_VF1_Audio_clip_animalID)
##########       Final output here is  sp_VF1_Audio_clip_animalID                        ####################
st_write(sp_VF1_Audio_clip_animalID, "sp_VF1_Audio_clip_animalID.csv", layer_options = "GEOMETRY=AS_XY")

############################################################################################################################################################
#####################################              check that this is useful ##############################################################################
############################################################################################################################################################

#### Can I just stick this to the end of my InclusionBord_c_animalID_clean dataset?
head(sp_VF1_InclusionBord_animalID)
head(sp_VF1_Audio_clip_animalID)

VF_1_InclusionBord_Audio <- rbind(sp_VF1_InclusionBord_animalID,
                                  sp_VF1_Audio_clip_animalID)


head(VF_1_InclusionBord_Audio)

Aduio_sum1 <- group_by(VF_1_InclusionBord_Audio, date, animal_ID, event) %>% 
  summarise(count = n())
   
print(Aduio_sum1)

############################################################################################################################################################



###########################################################################################################################################################
#############################            create a df for the electrical pulse                                                  #############################
###########################################################################################################################################################


#bring in the VF1 data with all the event data
table(VF_week1_2_3$event)

"Audio started"
"Audio started [simulated]"
"Audio started (short)"
"Audio started (short) [simulated]"
"Pulse started"
"Pulse started [simulated]"
Pulse <- filter(VF_week1_2_3, event == "Pulse started" | 
                               event == "Pulse started [simulated]" )

head(Pulse)


#########    Remove the NA   ##########

Pulse <- Pulse %>% filter(!is.na(lat) | !is.na(lon))

summary(Pulse$lat)
summary(Pulse$lon)
#saveRDS(Pulse,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3_Pulse.rds"))


################################################################################################################
##################           Divide up the data into VF chuncks                               ##################    
################################################################################################################
#What is the max time?
max_time_Pulse <- as_datetime(max(Pulse$time), tz="GMT") 
print(max_time_Pulse)
#Fence 1 called training fence eden valley
VF1_Pulse <- filter(Pulse, 
                    between(time, as_datetime('2019-05-20 10:15:00', tz="GMT"),
                            as_datetime('2019-05-20 14:40:00', tz="GMT")))
#Fence 2 called training mk1
VF2_Pulse <- filter(Pulse, 
                    between(time, as_datetime('2019-05-20 14:50:00', tz="GMT"),
                            as_datetime('2019-05-23 08:30:00', tz="GMT")))

#Fence 3 called bron next traing fence
VF3_Pulse <- filter(Pulse, 
                    between(time, as_datetime('2019-05-23 08:30:00', tz="GMT"),
                            as_datetime('2019-05-28 11:00:00', tz="GMT")))

#Fence 4 called bron next traing fence check that the time range is working
VF4_Pulse <- filter(Pulse, 
                    between(time, as_datetime('2019-05-28 11:15:00', tz="GMT"),
                            as_datetime('2019-06-03 09:30:00', tz="GMT")))

#Fence 5 called bron next traing fence Cant don this yet without the full data set
#VF5_Pulse <- filter(Pulse, 
#                     between(time, as_datetime('2019-06-03 09:31:00', tz="GMT"),
#                                   as_datetime('2019-07-02 06:11:00', tz="GMT")))




###############################################################################################################
##################           make data into spatial object                               ##################    
################################################################################################################


#bring in the boundary file as spatial data fram with sf package

getwd()
eden_valley <- st_read("EdenValley_site1GDA_a.shp")

#assign a coord ref epsg
st_crs(eden_valley) <- 28354
st_crs(eden_valley)
plot(st_geometry(eden_valley))
#make this the epsg that will be used for all data
the_crs <- st_crs(eden_valley, asText = TRUE)
#make the inclusion data into a spatial object

#assign WGS EPSG for coords for each of the VF dataframes
sp_VF1_Pulse <- st_as_sf(VF1_Pulse, coords = c("lon", "lat"), crs = 4326, agr = "constant")
head(sp_VF1_Pulse)
str(sp_VF1_Pulse)
# plot it 
sp_VF1_Pulse_geom <- st_geometry(sp_VF1_Pulse)
plot(sp_VF1_Pulse_geom, col = "grey") 


#transfor the cattle data so its in the same data frame as the paddock boundary
sp_VF1_Pulse_trans <- st_transform(sp_VF1_Pulse, crs = 28354)
head(sp_VF1_Pulse_trans)
head(eden_valley)
plot(st_geometry(sp_VF1_Audio_trans))
sp_VF1_Pulse_trans_geo <- st_geometry(sp_VF1_Pulse_trans)#data frame that is just points no attributes
#check that I have done this - looking good!looking for points to be displayed in paddock
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = sp_VF1_Pulse_trans_geo)


############################   Now clip ################################################################

sp_VF1_Pulse_trans_clip <- st_intersection(sp_VF1_Pulse_trans, eden_valley) #message about assumed spatial consistant

#check I have done what I want Looks good

plot(st_geometry(sp_VF1_Pulse_trans_clip))
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA)+
  geom_sf(data = sp_VF1_Pulse_trans_clip)


#########################################################################################################
#############    Recal the distance from VF line ########################################################

#bring in the VF 
fence1 <- st_read("Fence1.shp")
st_crs(fence1) <- 28354
st_crs(fence1)
plot(st_geometry(fence1))
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)
#Try distance tool

sp_VF1_Pulse_trans_clip <- mutate(sp_VF1_Pulse_trans_clip, 
                                  dist = st_distance(sp_VF1_Pulse_trans_clip, fence1))
head(sp_VF1_Pulse_trans_clip)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)+
  geom_sf(data = sp_VF1_Pulse_trans_clip)

head(sp_VF1_Pulse_trans_clip$dis)  
class(sp_VF1_Pulse_trans_clip$dis)
sp_VF1_Pulse_trans_clip$dist <- as.double(sp_VF1_Pulse_trans_clip$dist)


########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################




sp_VF1_Pluse_clip_animalID <- mutate(sp_VF1_Pulse_trans_clip,
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

#check we are assignining all the collar ID to animal names
head(sp_VF1_Pluse_clip_animalID)
with(sp_VF1_Pluse_clip_animalID, table(date, animal_ID))

#

summary(sp_VF1_Pluse_clip_animalID)

##################################################################################################################################
#################                         Select data points inside non grazing zone                 #############################
##################################################################################################################################
getwd()
VF1_NonGraz <- st_read("VF1_NonGraz.shp")
st_crs(VF1_NonGraz) <- 28354
st_crs(VF1_NonGraz)
plot(st_geometry(VF1_NonGraz))

#won't need this on pearcy but get the data into spatial format
#assign WGS EPSG for coords for each of the VF dataframes
#head(sp_VF1_InclusionBord_animalID)
#sp_VF1_InclusionBord_animalID <- st_as_sf(sp_VF1_InclusionBord_animalID, coords = c("X", "Y"), crs = 28354, agr = "constant")
head(sp_VF1_Pluse_clip_animalID)
str(sp_VF1_Pluse_clip_animalID)
dim(sp_VF1_Pluse_clip_animalID)



# which points fall inside a polygon? Create a new clm for this non_graz when true its in the non grazing zone
sp_VF1_Pluse_clip_animalID <- mutate(sp_VF1_Pluse_clip_animalID,
                                     non_graz = apply((st_intersects(sp_VF1_Pluse_clip_animalID, 
                                                                     VF1_NonGraz, sparse = FALSE)), 1, any))
head(sp_VF1_Pluse_clip_animalID)
sp_VF1_Pluse_clip_animalID_TRUE <- filter(sp_VF1_Pluse_clip_animalID, non_graz == TRUE)
sp_VF1_Pluse_clip_animalID_FALSE <- filter(sp_VF1_Pluse_clip_animalID, non_graz == FALSE)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "green", fill = NA)+
  #geom_sf(data = sp_VF1_Audio_clip_animalID_FALSE)
  geom_sf(data = sp_VF1_Pluse_clip_animalID_TRUE)
# add extra clm that distance from VF False values become negative and are in the grazing zone
sp_VF1_Pluse_clip_animalID <- mutate(sp_VF1_Pluse_clip_animalID,
                                     distance_VF =  ifelse(non_graz == FALSE, (dist*-1), dist))

head(sp_VF1_Pluse_clip_animalID)
##########       Final output here is  sp_VF1_Audio_clip_animalID                        ####################
st_write(sp_VF1_Pluse_clip_animalID, "sp_VF1_Pluse_clip_animalID.csv", layer_options = "GEOMETRY=AS_XY")





###########################################################################################################################################################
#####################################              check that this is useful ##############################################################################
############################################################################################################################################################

#### Can I just stick this to the end of my InclusionBord_ dataset?
head(sp_VF1_InclusionBord_animalID)
head(sp_VF1_Pluse_clip_animalID)

VF_1_InclusionBord_Pulse <- rbind(sp_VF1_InclusionBord_animalID,
                                  sp_VF1_Pluse_clip_animalID)


head(VF_1_InclusionBord_Pulse)

Pulse_sum1 <- group_by(VF_1_InclusionBord_Pulse, date, animal_ID, event) %>% 
  summarise(count = n())

print(Pulse_sum1)





VF_dates=data.frame(date=as.Date(c("2019-05-20", "2019-05-23", "2019-05-28", "2019-06-03")), 
             event=c("VF1&2", "VF3", "VF4", "VF5"))
filter(Pulse_sum1, count <50) %>% 
ggplot( aes(x = date, y = count, colour = event))+
  #geom_boxplot(alpha = 0.2)+
  geom_point()+
  theme_bw()+
  facet_wrap(. ~ animal_ID)+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.2) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Week1 to 3",
       x= "Date",
       y = "Counts of pulse")

### Audio
print(Aduio_sum1)
filter(Aduio_sum1, count <50 ) %>% 
  ggplot( aes(x = date, y = event))+
  #geom_boxplot(alpha = 0.2)+
  geom_point()+
  theme_bw()+
  facet_wrap(. ~ animal_ID)+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.2) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Week1 to 3",
       x= "Date",
       y = "Counts of cues")

############################################################################################################
#### Can I just stick this to the end of my InclusionBord_ dataset?
head(sp_VF1_InclusionBord_animalID)
head(sp_VF1_Pluse_clip_animalID)
head(sp_VF1_Audio_clip_animalID)

VF_1_Audio_Pulse <- rbind(sp_VF1_Audio_clip_animalID,
                                  sp_VF1_Pluse_clip_animalID)

##########       Final output here is  VF_1_Audio_Pulse                        ####################
st_write(VF_1_Audio_Pulse, "VF_1_Audio_Pulse.csv", layer_options = "GEOMETRY=AS_XY")
