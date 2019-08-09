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

### This is the inclusionBord data
Check_df_VF_week1_2_3_InclusionBord_c_animalID <- read_rds("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/df_VF_week1_2_3_InclusionBord_c_animalID_clean.rds")
dim(Check_df_VF_week1_2_3_InclusionBord_c_animalID)


head(Check_df_VF_week1_2_3_InclusionBord_c_animalID)

##### Don't need these steps
#VF_week1 <- read_csv("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/VF_week1.csv")
#VF_week2 <- read_csv("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/VF_week2.csv")
#VF_week3 <- read_csv("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/VF_week3.csv")

##########       Merge this all togther   ##########       
VF_week1_2_3 <- rbind(VF_week1, VF_week2, VF_week3)
saveRDS(VF_week1_2_3,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3.rds"))

###########################################################################################################################################################
#############################            create a df for the audio                                                  #############################
###########################################################################################################################################################
table(VF_week1_2_3$event)

"Audio started"
"Audio started [simulated]"
"Audio started (short)"
"Audio started (short) [simulated]"
"Pulse started"
"Pulse started [simulated]"

VF_week1_2_3_Audio <- filter(VF_week1_2_3, event == "Audio started" | 
                                           event == "Audio started [simulated]" |
                                           event == "Audio started (short)" |
                                           event == "Audio started (short) [simulated]")
head(VF_week1_2_3_Audio)


#########    Remove the NA   ##########

VF_week1_2_3_Audio <- VF_week1_2_3_Audio %>% filter(!is.na(lat) | !is.na(lon))

summary(VF_week1_2_3_Audio$lat)
summary(VF_week1_2_3_Audio$lon)
saveRDS(VF_week1_2_3_Audio,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3_Audio.rds"))

mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

####################  convert lat and longs to x and Y     ##########################################
coordinates(VF_week1_2_3_Audio) <- ~ lon + lat
proj4string(VF_week1_2_3_Audio) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_week1_2_3_Audio_1 <- spTransform(VF_week1_2_3_Audio, mapCRS)
#make new df_1
head(VF_week1_2_3_Audio_1,3)
VF_week1_2_3_Audio = as.data.frame(VF_week1_2_3_Audio_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_week1_2_3_Audio <- mutate(VF_week1_2_3_Audio,POINT_X = lon,  POINT_Y = lat )
head(VF_week1_2_3_Audio)
mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54

VF_week1_2_3_Audio_spatial <- st_as_sf(VF_week1_2_3_Audio, coords = c("POINT_X", "POINT_Y"), crs = mapCRS)
glimpse(VF_week1_2_3_Audio_spatial)
###bring in the polygon file
eden_valley <- st_read(
  "W:/VF/Eden_Valley/VF_Boundary/EdenValley_site1GDA_a.shp")
VF_week1_2_3_Audio_clip <- st_intersection(VF_week1_2_3_Audio_spatial, eden_valley)
head(VF_week1_2_3_Audio_clip, 3)


head(VF_week1_2_3_Audio_clip ,3)
VF_week1_2_3_Audio_clip <- data.frame(VF_week1_2_3_Audio_clip)
VF_week1_2_3_Audio_clip <- mutate(VF_week1_2_3_Audio_clip,
                                             POINT_X = lon,  POINT_Y = lat )
head(VF_week1_2_3_Audio_clip ,3)

VF_week1_2_3_Audio_clip_c <- select(VF_week1_2_3_Audio_clip,
                                          -geometry,
                                          -OID_,
                                          -Name,
                                          -FolderPath, 
                                          -SymbolID, 
                                          -AltMode, 
                                          -Base, 
                                          -Clamped, 
                                          -Extruded, 
                                          -Snippet, 
                                          -PopupInfo, 
                                          -Shape_Leng,
                                          -Shape_Area,
                                          -lon,
                                          -lat)

head(VF_week1_2_3_Audio_clip_c ,3)
#############  Remove Pre trial readings ##############

VF_week1_2_3_Audio_clip_c <- filter(VF_week1_2_3_Audio_clip_c, time > as_datetime('2019-05-20 12:30:00', tz="GMT"))

VF_week1_2_3_Audio_clip_c_animalID <- mutate(VF_week1_2_3_Audio_clip_c,
                                                   animal_ID = case_when(
                                                     collar_ID == "ac138" ~ "Q46",
                                                     collar_ID == "ac187" ~ "Q36",
                                                     collar_ID == "ac204" ~ "Q108",
                                                     collar_ID == "ac207" ~ "Q42",
                                                     collar_ID == "ac212" ~ "Q29",
                                                     collar_ID == "ac213" &
                                                       between(time, as_datetime('2019-05-20 12:30:00', tz="GMT"),
                                                               as_datetime('2019-05-28 06:44:00', tz="GMT")) ~ "Q47",
                                                     collar_ID == "ac320" &
                                                       between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                               as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                                     collar_ID == "ac217" ~ "Q27",
                                                     collar_ID == "ac218" ~ "Q2",
                                                     collar_ID == "ac219" &
                                                       between(time, as_datetime('2019-05-20 12:32:03', tz="GMT"),
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
                                                       between(time, as_datetime('2019-05-20 12:30:00', tz="GMT"),
                                                               as_datetime('2019-05-27 16:19:00', tz="GMT"))~ "Q45",
                                                     collar_ID == "ac209"  &
                                                       between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                               as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                                     collar_ID == "ad3471" ~ "Q15",
                                                     collar_ID == "ad3502" ~ "Q8",
                                                     collar_ID == "ad3925" ~ "Q110",
                                                     TRUE ~ "NA"))

############################################################################################################################################################
#####################################              check that this is useful ##############################################################################
############################################################################################################################################################

summary(VF_week1_2_3_Audio_clip_c_animalID)

saveRDS(VF_week1_2_3_Audio_clip_c_animalID,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3_Audio_clip_c_animalID.rds"))    


#### Can I just stick this to the end of my InclusionBord_c_animalID_clean dataset?
VF_week1_2_3_InclusionBord_Audio <- rbind(Check_df_VF_week1_2_3_InclusionBord_c_animalID,
                                          VF_week1_2_3_Audio_clip_c_animalID)

head(VF_week1_2_3_Audio_clip_c_animalID)
Aduio_sum <- group_by(VF_week1_2_3_Audio_clip_c_animalID, animal_ID) %>% 
  summarise(
            max_time = max(as_datetime(time, tz="GMT")), 
            min_time = min(as_datetime(time, tz="GMT")),
            period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))
print(Aduio_sum)

summary(VF_week1_2_3_Audio_clip_c_animalID$date)
Aduio_sum1 <- group_by(VF_week1_2_3_Audio_clip_c_animalID, date, animal_ID, event) %>% 
  summarise(count = n())
   
print(Aduio_sum1)

############################################################################################################################################################



###########################################################################################################################################################
#############################            create a df for the electrical pulse                                                  #############################
###########################################################################################################################################################
table(VF_week1_2_3$event)

"Pulse started"
"Pulse started [simulated]"


VF_week1_2_3_Pulse <- filter(VF_week1_2_3, event == "Pulse started" | 
                               event == "Pulse started [simulated]" )
head(VF_week1_2_3_Pulse)


#########    Remove the NA   ##########

VF_week1_2_3_Pulse <- VF_week1_2_3_Pulse %>% filter(!is.na(lat) | !is.na(lon))

summary(VF_week1_2_3_Pulse$lat)
summary(VF_week1_2_3_Pulse$lon)
saveRDS(VF_week1_2_3_Pulse,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3_Pulse.rds"))

#mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54
#wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

####################  convert lat and longs to x and Y     ##########################################
coordinates(VF_week1_2_3_Pulse) <- ~ lon + lat
proj4string(VF_week1_2_3_Pulse) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_week1_2_3_Pulse_1 <- spTransform(VF_week1_2_3_Pulse, mapCRS)
#make new df_1
head(VF_week1_2_3_Pulse_1,3)
VF_week1_2_3_Pulse = as.data.frame(VF_week1_2_3_Pulse_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_week1_2_3_Pulse <- mutate(VF_week1_2_3_Pulse,POINT_X = lon,  POINT_Y = lat )
head(VF_week1_2_3_Pulse)
mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54

VF_week1_2_3_Pulse_spatial <- st_as_sf(VF_week1_2_3_Pulse, coords = c("POINT_X", "POINT_Y"), crs = mapCRS)
glimpse(VF_week1_2_3_Pulse_spatial)
###bring in the polygon file

VF_week1_2_3_Pulse_clip <- st_intersection(VF_week1_2_3_Pulse_spatial, eden_valley)
head(VF_week1_2_3_Pulse_clip, 3)


head(VF_week1_2_3_Pulse_clip ,3)
VF_week1_2_3_Pulse_clip <- data.frame(VF_week1_2_3_Pulse_clip)
VF_week1_2_3_Pulse_clip <- mutate(VF_week1_2_3_Pulse_clip,
                                  POINT_X = lon,  POINT_Y = lat )
head(VF_week1_2_3_Pulse_clip ,3)

VF_week1_2_3_Pulse_clip_c <- select(VF_week1_2_3_Pulse_clip,
                                    -geometry,
                                    -OID_,
                                    -Name,
                                    -FolderPath, 
                                    -SymbolID, 
                                    -AltMode, 
                                    -Base, 
                                    -Clamped, 
                                    -Extruded, 
                                    -Snippet, 
                                    -PopupInfo, 
                                    -Shape_Leng,
                                    -Shape_Area,
                                    -lon,
                                    -lat)

head(VF_week1_2_3_Pulse_clip_c ,3)
#############  Remove Pre trial readings ##############

VF_week1_2_3_Pulse_clip_c <- filter(VF_week1_2_3_Pulse_clip_c, time > as_datetime('2019-05-20 12:30:00', tz="GMT"))

VF_week1_2_3_Pulse_clip_c_animalID <- mutate(VF_week1_2_3_Pulse_clip_c,
                                             animal_ID = case_when(
                                               collar_ID == "ac138" ~ "Q46",
                                               collar_ID == "ac187" ~ "Q36",
                                               collar_ID == "ac204" ~ "Q108",
                                               collar_ID == "ac207" ~ "Q42",
                                               collar_ID == "ac212" ~ "Q29",
                                               collar_ID == "ac213" &
                                                 between(time, as_datetime('2019-05-20 12:30:00', tz="GMT"),
                                                         as_datetime('2019-05-28 06:44:00', tz="GMT")) ~ "Q47",
                                               collar_ID == "ac320" &
                                                 between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
                                                         as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
                                               collar_ID == "ac217" ~ "Q27",
                                               collar_ID == "ac218" ~ "Q2",
                                               collar_ID == "ac219" &
                                                 between(time, as_datetime('2019-05-20 12:32:03', tz="GMT"),
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
                                                 between(time, as_datetime('2019-05-20 12:30:00', tz="GMT"),
                                                         as_datetime('2019-05-27 16:19:00', tz="GMT"))~ "Q45",
                                               collar_ID == "ac209"  &
                                                 between(time, as_datetime('2019-05-28 11:11:00', tz="GMT"),
                                                         as_datetime('2019-06-06 17:00:00', tz="GMT"))~ "Q45",
                                               collar_ID == "ad3471" ~ "Q15",
                                               collar_ID == "ad3502" ~ "Q8",
                                               collar_ID == "ad3925" ~ "Q110",
                                               TRUE ~ "NA"))
summary(VF_week1_2_3_Pulse_clip_c_animalID)

saveRDS(VF_week1_2_3_Pulse_clip_c_animalID,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3_Pulse_clip_c_animalID.rds"))    


#### Can I just stick this to the end of my InclusionBord_c_animalID_clean dataset?
VF_week1_2_3_InclusionBord_Audio_pulse <- rbind(Check_df_VF_week1_2_3_InclusionBord_c_animalID,
                                          VF_week1_2_3_Audio_clip_c_animalID,
                                          VF_week1_2_3_Pulse_clip_c_animalID)

saveRDS(VF_week1_2_3_InclusionBord_Audio_pulse,  paste0("W:/VF/Eden_Valley/logged_VF_data/download2_R_output/","VF_week1_2_3_InclusionBord_Audio_pulse.rds")) 

table(VF_week1_2_3_InclusionBord_Audio_pulse$event)

VF_week1_2_3_Audio_pulse <- filter(VF_week1_2_3_InclusionBord_Audio_pulse, event != "InclusionBorder_m" )
table(VF_week1_2_3_Audio_pulse$event)

VF_week1_2_3_Audio_pulse <- mutate(VF_week1_2_3_Audio_pulse,
                                   audio_pulse = case_when(
                                     event == "Audio started" ~ "Audio",
                                     event == "Audio started (short)" ~ "Audio",
                                     event == "Audio started (short) [simulated]" ~ "Audio",
                                     event == "Audio started [simulated]" ~ "Audio",
                                     event == "Pulse started" ~ "Pulse",
                                     event == "Pulse started [simulated]" ~ "Pulse"))

Aduio_sum1 <- group_by(VF_week1_2_3_Audio_pulse, date, animal_ID, audio_pulse) %>% 
  summarise(count = n())

print(Aduio_sum1)
#Aduio_sum1$day_factor <- as.factor(Aduio_sum1$day)


VF_dates=data.frame(date=as.Date(c("2019-05-20", "2019-05-23", "2019-05-28", "2019-06-03")), 
             event=c("VF1&2", "VF3", "VF4", "VF5"))
filter(Aduio_sum1, count <50) %>% 
ggplot( aes(x = date, y = count, colour = audio_pulse))+
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

### Audio
filter(Aduio_sum1, count <50 & audio_pulse == "Audio") %>% 
  ggplot( aes(x = date, y = count))+
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

### pulse
filter(Aduio_sum1, count <50 & audio_pulse == "Pulse") %>% 
  ggplot( aes(x = date, y = count))+
  #geom_boxplot(alpha = 0.2)+
  geom_point()+
  theme_bw()+
  #facet_wrap(. ~ animal_ID)+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Week1 to 3",
       x= "Date",
       y = "Counts of cues")
