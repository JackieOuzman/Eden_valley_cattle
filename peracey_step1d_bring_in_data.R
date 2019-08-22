


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
################################################################################################################
#######################                 VF 3 data                    ###########################################
################################################################################################################
################################################################################################################



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
sp_VF3_InclusionBord <- st_as_sf(VF3_InclusionBord, coords = c("lon", "lat"), crs = 4326, agr = "constant")
head(sp_VF3_InclusionBord)
str(sp_VF3_InclusionBord)
# plot it 
sp_VF3_InclusionBord_geo <- st_geometry(sp_VF3_InclusionBord)
plot(sp_VF3_InclusionBord_geo, col = "grey") 


#transfor the cattle data so its in the same data frame as the paddock boundary
sp_VF3_InclusionBord_trans <- st_transform(sp_VF3_InclusionBord, crs = 28354)
head(sp_VF3_InclusionBord_trans)
head(eden_valley)
plot(st_geometry(sp_VF3_InclusionBord_trans))
sp_VF3_InclusionBord_trans_geo <- st_geometry(sp_VF3_InclusionBord_trans)#data frame that is just points no attributes
#check that I have done this - looking good!looking for points to be displayed in paddock
ggplot() +
geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = sp_VF3_InclusionBord_trans_geo)


############################   Now clip ################################################################

sp_VF3_InclusionBord_clip <- st_intersection(sp_VF3_InclusionBord_trans, eden_valley) #message about assumed spatial consistant

#check I have done what I want Looks good

plot(st_geometry(sp_VF3_InclusionBord_clip))
#ggplot() +
# geom_sf(data = eden_valley, color = "black", fill = NA)+
#geom_sf(data = sp_VF3_InclusionBord_clip)


##########################################################################################################
#############    Recal the distance from VF line ########################################################

#bring in the VF 
fence3 <- st_read("Fence3.shp")
st_crs(fence3) <- 28354
st_crs(fence3)
plot(st_geometry(fence3))
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "green", fill = NA)
#Try distance tool

sp_VF3_InclusionBord_clip <- mutate(sp_VF3_InclusionBord_clip, 
               dist = st_distance(sp_VF3_InclusionBord_clip, fence3))
head(sp_VF3_InclusionBord_clip)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "green", fill = NA)+
  geom_sf(data = sp_VF3_InclusionBord_clip)
#st_write(sp_VF3_InclusionBord_clip, "sp_VF3_InclusionBord_clip.csv", layer_options = "GEOMETRY=AS_XY")

##########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################
#bring in the sp_VF3_InclusionBord_clip if needed.

#sp_VF3_InclusionBord_clip <- read_csv(file = "//pearceyhome.csiro.au/HOME_INTEL/ouz001/VF_cattle/Cattle_pearcey/collar logs_download2/sp_VF3_InclusionBord_clip.csv")
#head(sp_VF3_InclusionBord_clip)  
#class(sp_VF3_InclusionBord_clip$time)
sp_VF3_InclusionBord_clip$dist <- as.double(sp_VF3_InclusionBord_clip$dist)
class(sp_VF3_InclusionBord_clip$time)
class(sp_VF3_InclusionBord_clip$dist)

#sp_VF3_InclusionBord_clip <- mutate(sp_VF3_InclusionBord_clip,
#                                    time = as_datetime(time))



### changed the start time for could of renames

sp_VF3_InclusionBord_animalID <- mutate(sp_VF3_InclusionBord_clip,
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
head(sp_VF3_InclusionBord_animalID)
with(sp_VF3_InclusionBord_animalID, table(date, animal_ID))

#the location of the NA
NA_sp_VF3_InclusionBord_animalID <- filter(sp_VF3_InclusionBord_animalID,
                                           animal_ID == "NA")
with(NA_sp_VF3_InclusionBord_animalID, table(date, collar_ID))

##################################################################################################################################
#################                         Select data points inside non grazing zone                 #############################
##################################################################################################################################
getwd()
VF3_NonGraz <- st_read("VF3_NonGraz.shp")
st_crs(VF3_NonGraz) <- 28354
st_crs(VF3_NonGraz)
plot(st_geometry(VF3_NonGraz))

#won't need this on pearcy but get the data into spatial format
#assign WGS EPSG for coords for each of the VF dataframes
head(sp_VF3_InclusionBord_animalID)
#sp_VF3_InclusionBord_animalID <- st_as_sf(sp_VF3_InclusionBord_animalID, coords = c("X", "Y"), crs = 28354, agr = "constant")
#head(sp_VF3_InclusionBord_animalID)
str(sp_VF3_InclusionBord_animalID)
dim(sp_VF3_InclusionBord_animalID)



# which points fall inside a polygon? Create a new clm for this non_graz when true its in the non grazing zone
sp_VF3_InclusionBord_animalID <- mutate(sp_VF3_InclusionBord_animalID,
               non_graz = apply((st_intersects(sp_VF3_InclusionBord_animalID, VF3_NonGraz, sparse = FALSE)), 1, any))
head(sp_VF3_InclusionBord_animalID)
sp_VF3_InclusionBord_animalID_TRUE <- filter(sp_VF3_InclusionBord_animalID, non_graz == TRUE)
sp_VF3_InclusionBord_animalID_FALSE <- filter(sp_VF3_InclusionBord_animalID, non_graz == FALSE)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence3, color = "green", fill = NA)+
  #geom_sf(data = sp_VF3_InclusionBord_animalID_FALSE)
  geom_sf(data = sp_VF3_InclusionBord_animalID_TRUE)
# add extra clm that distance from VF False values become negative and are in the grazing zone
sp_VF3_InclusionBord_animalID <- mutate(sp_VF3_InclusionBord_animalID,
                distance_VF =  ifelse(non_graz == FALSE, (dist*-1), dist))


##########       Final output here is  sp_VF3_InclusionBord_animalID                        ####################
st_write(sp_VF3_InclusionBord_animalID, "sp_VF3_InclusionBord_animalID.csv", layer_options = "GEOMETRY=AS_XY")
