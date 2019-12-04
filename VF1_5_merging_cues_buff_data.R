###                 This uses the fence line that should have been used ####

head(VF_week1_2_3)
###########################################################################################################################################################
#############################            create a df for the audio                                                  #############################
###########################################################################################################################################################
### 1. the data I will use - this the same as the Fence1_data_clean.rds 
### W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence1_data_clean.rds


head(Fence1_Incl_animalID)
head(Fence2_Incl_animalID)
head(Fence3_Incl_animalID)
head(Fence4_Incl_animalID)
head(Fence5_Incl_animalID)

##what is in the event clm that I want
table(Fence1_Incl_animalID$event)

"Audio started"
"Audio started [simulated]"
"Audio started (short)"
"Audio started (short) [simulated]"
"Pulse started"
"Pulse started [simulated]"


### 2.Just keep the cue data and check it
function_cue_data <- function(df){
  filter(df,
    event == "Audio started" |
    event == "Audio ceased(short)" |
    event == "Audio ceased: detected animal reaction" |
    event == "Audio started [simulated]" |
    event == "Audio started (short)" |
    event == "Audio started (short) [simulated]" |
    event == "Audio ceased: T-audio" |
    event == "PulseResult" |
    event == "Pulse started" |
    event == "Pulse ceased" |
    event == "Pulse started [simulated]"
  ) %>% 
    filter(!is.na(lat) | !is.na(lon))
  }
Fence1_cue_data <- function_cue_data(Fence1_Incl_animalID)
Fence2_cue_data <- function_cue_data(Fence2_Incl_animalID)
Fence3_cue_data <- function_cue_data(Fence3_Incl_animalID)
Fence4_cue_data <- function_cue_data(Fence4_Incl_animalID)
Fence5_cue_data <- function_cue_data(Fence5_Incl_animalID)
 # head(Fence1_cue_data)  
 # table(Fence1_cue_data$event)
 # table(Fence1_Incl_animalID$event)
# summary(Fence4_cue_data$lat)
# summary(Fence4_cue_data$lon)



######################################################################################
#### 3. Bring in spatial data bounadries and VF with sf package

### 3a. paddock bounadry
eden_valley <- st_read("W:/VF/Eden_Valley/VF_Boundary/EdenValley_site1GDA_a.shp")
#assign a coord ref epsg
st_crs(eden_valley) <- 28354

#make this the epsg that will be used for all data
the_crs <- st_crs(eden_valley, asText = TRUE)

### 3b. VF lines

fence1 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence1.shp")
fence2 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence2.shp")
fence3 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence3.shp")
fence4 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence4a.shp")
fence5 <- st_read("W:/VF/Eden_Valley/VF_Boundary/Fence5.shp")

st_crs(fence1) <- 28354
st_crs(fence2) <- 28354
st_crs(fence3) <- 28354
st_crs(fence4) <- 28354
st_crs(fence5) <- 28354


### 3c. Non Grazing ploygons

VF1_NonGraz <- st_read("W:/VF/Eden_Valley/VF_Boundary/VF1_NonGraz.shp")
VF2_NonGraz <- st_read("W:/VF/Eden_Valley/VF_Boundary/VF2_NonGraz.shp")
VF3_NonGraz <- st_read("W:/VF/Eden_Valley/VF_Boundary/VF3_NonGraz.shp")
VF4_NonGraz <- st_read("W:/VF/Eden_Valley/VF_Boundary/VF4_NonGraz.shp")
VF5_NonGraz <- st_read("W:/VF/Eden_Valley/VF_Boundary/VF5_NonGraz.shp")

st_crs(VF1_NonGraz) <- 28354
st_crs(VF2_NonGraz) <- 28354
st_crs(VF3_NonGraz) <- 28354
st_crs(VF4_NonGraz) <- 28354
st_crs(VF5_NonGraz) <- 28354


########################################################################################################
### 4. create Inclusion data

### 4a. Function to recal the distance

cue_recal_function <-
  function(df, VF_line, VF_nonGraz_polygon) {
    #assign WGS EPSG for coords for each of the VF dataframes
    sp_data <-
      st_as_sf(df,
               coords = c("lon", "lat"),
               crs = 4326,
               agr = "constant")
    sp_data_trans <-
      st_transform(sp_data, crs = 28354)
    
    sp_data_trans_clip <-
      st_intersection(sp_data_trans, eden_valley)
    
    sp_data_trans_clip <- mutate(sp_data_trans_clip,
                                         dist = st_distance(sp_data_trans_clip, VF_line))
    sp_data_trans_clip$dist <-
      as.double(sp_data_trans_clip$dist)
    
    
    
    # which points fall inside a polygon? Create a new clm for this non_graz when true its in the non grazing zone
    sp_data_trans_clip <- mutate(sp_data_trans_clip,
                                         non_graz = apply((
                                           st_intersects(sp_data_trans_clip,
                                                         VF_nonGraz_polygon, sparse = FALSE)
                                         ), 1, any))
    
    
    # add extra clm that distance from VF False values become negative and are in the grazing zone
    sp_data_trans_clip <- mutate(sp_data_trans_clip,
                                         distance_VF =  ifelse(non_graz == FALSE, (dist *
                                                                                     -1), dist))
  }

### 4b. use function to recal the distance

Fence1_cue_recal <- cue_recal_function(Fence1_cue_data, fence1, VF1_NonGraz)
Fence2_cue_recal <- cue_recal_function(Fence2_cue_data, fence2, VF2_NonGraz)
Fence3_cue_recal <- cue_recal_function(Fence3_cue_data, fence3, VF3_NonGraz)
Fence4_cue_recal <- cue_recal_function(Fence4_cue_data, fence4, VF4_NonGraz)
Fence5_cue_recal <- cue_recal_function(Fence5_cue_data, fence5, VF5_NonGraz)



head(Fence5_cue_recal)


#### 5. drop geometry?
Fence1_cue_recal_drop_geom <- st_set_geometry(Fence1_cue_recal, NULL)
Fence2_cue_recal_drop_geom <- st_set_geometry(Fence2_cue_recal, NULL)
Fence3_cue_recal_drop_geom <- st_set_geometry(Fence3_cue_recal, NULL)
Fence4_cue_recal_drop_geom <- st_set_geometry(Fence4_cue_recal, NULL)
Fence5_cue_recal_drop_geom <- st_set_geometry(Fence5_cue_recal, NULL)












############################################################################################################################################################
#####################################              check that this is useful ##############################################################################
############################################################################################################################################################

#### Can I just stick this to the end of my InclusionBord_c_animalID_clean dataset?
head(sp_VF4_InclusionBord_animalID)
head(sp_VF4_Audio_clip_animalID_buff)
sp_VF4_Audio_clip_animalID_buff <- select(sp_VF4_Audio_clip_animalID_buff,
                                  -Id,
                                  -BUFF_DIST,
                                  -ORIG_FID)

VF_4_InclusionBord_Audio_buff <- rbind(sp_VF4_InclusionBord_animalID,
                                  sp_VF4_Audio_clip_animalID_buff)


head(VF_4_InclusionBord_Audio_buff)

Aduio_sum4 <- group_by(VF_4_InclusionBord_Audio_buff, date, animal_ID, event) %>% 
  summarise(count = n())
   
print(Aduio_sum4)

############################################################################################################################################################



###########################################################################################################################################################
#############################            create a df for the electrical pulse                                                  #############################
###########################################################################################################################################################


#bring in the VF4 data with all the event data
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


#Fence 4 called bron next traing fence check that the time range is working
VF4_Pulse <- filter(Pulse, 
                    between(time, as_datetime('2019-05-28 11:15:00', tz="GMT"),
                            as_datetime('2019-06-03 09:30:00', tz="GMT")))





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
sp_VF4_Pulse <- st_as_sf(VF4_Pulse, coords = c("lon", "lat"), crs = 4326, agr = "constant")
head(sp_VF4_Pulse)
str(sp_VF4_Pulse)
# plot it 
sp_VF4_Pulse_geom <- st_geometry(sp_VF4_Pulse)
plot(sp_VF4_Pulse_geom, col = "grey") 


#transfor the cattle data so its in the same data frame as the paddock boundary
sp_VF4_Pulse_trans <- st_transform(sp_VF4_Pulse, crs = 28354)
head(sp_VF4_Pulse_trans)
head(eden_valley)
plot(st_geometry(sp_VF4_Audio_trans))
sp_VF4_Pulse_trans_geo <- st_geometry(sp_VF4_Pulse_trans)#data frame that is just points no attributes
#check that I have done this - looking good!looking for points to be displayed in paddock
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = sp_VF4_Pulse_trans_geo)


############################   Now clip ################################################################

sp_VF4_Pulse_trans_clip <- st_intersection(sp_VF4_Pulse_trans, eden_valley) #message about assumed spatial consistant

#check I have done what I want Looks good

plot(st_geometry(sp_VF4_Pulse_trans_clip))
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA)+
  geom_sf(data = sp_VF4_Pulse_trans_clip)


#########################################################################################################
#############    Recal the distance from VF line ########################################################

#bring in the VF 
fence4 <- st_read("Fence4a.shp")
st_crs(fence4) <- 28354
st_crs(fence4)
plot(st_geometry(fence4))
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "green", fill = NA)
#Try distance tool

sp_VF4_Pulse_trans_clip <- mutate(sp_VF4_Pulse_trans_clip, 
                                  dist = st_distance(sp_VF4_Pulse_trans_clip, fence4))
head(sp_VF4_Pulse_trans_clip)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "green", fill = NA)+
  geom_sf(data = sp_VF4_Pulse_trans_clip)

head(sp_VF4_Pulse_trans_clip$dis)  
class(sp_VF4_Pulse_trans_clip$dis)
sp_VF4_Pulse_trans_clip$dist <- as.double(sp_VF4_Pulse_trans_clip$dist)


########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################




sp_VF4_Pluse_clip_animalID <- mutate(sp_VF4_Pulse_trans_clip,
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
head(sp_VF4_Pluse_clip_animalID)
with(sp_VF4_Pluse_clip_animalID, table(date, animal_ID))

#

summary(sp_VF4_Pluse_clip_animalID)

##################################################################################################################################
#################                         Select data points inside non grazing zone                 #############################
##################################################################################################################################
getwd()
VF4_NonGraz <- st_read("VF4_NonGraz.shp")
st_crs(VF4_NonGraz) <- 28354
st_crs(VF4_NonGraz)
plot(st_geometry(VF4_NonGraz))

#won't need this on pearcy but get the data into spatial format
#assign WGS EPSG for coords for each of the VF dataframes
#head(sp_VF4_InclusionBord_animalID)
#sp_VF4_InclusionBord_animalID <- st_as_sf(sp_VF4_InclusionBord_animalID, coords = c("X", "Y"), crs = 28354, agr = "constant")
head(sp_VF4_Pluse_clip_animalID)
str(sp_VF4_Pluse_clip_animalID)
dim(sp_VF4_Pluse_clip_animalID)



# which points fall inside a polygon? Create a new clm for this non_graz when true its in the non grazing zone
sp_VF4_Pluse_clip_animalID <- mutate(sp_VF4_Pluse_clip_animalID,
                                     non_graz = apply((st_intersects(sp_VF4_Pluse_clip_animalID, 
                                                                     VF4_NonGraz, sparse = FALSE)), 1, any))
head(sp_VF4_Pluse_clip_animalID)
sp_VF4_Pluse_clip_animalID_TRUE <- filter(sp_VF4_Pluse_clip_animalID, non_graz == TRUE)
sp_VF4_Pluse_clip_animalID_FALSE <- filter(sp_VF4_Pluse_clip_animalID, non_graz == FALSE)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "green", fill = NA)+
  #geom_sf(data = sp_VF4_Audio_clip_animalID_FALSE)
  geom_sf(data = sp_VF4_Pluse_clip_animalID_TRUE)
# add extra clm that distance from VF False values become negative and are in the grazing zone
sp_VF4_Pluse_clip_animalID <- mutate(sp_VF4_Pluse_clip_animalID,
                                     distance_VF =  ifelse(non_graz == FALSE, (dist*-1), dist))

head(sp_VF4_Pluse_clip_animalID)

############# Add in extra step to buffer the data #########
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "green", fill = NA)+
  geom_sf(data = sp_VF4_Pluse_clip_animalID)
#bring in the buffer
fence4_buff <- st_read("Fence4a_buffer25.shp")
st_crs(fence4_buff) <- 28354
st_crs(fence4_buff)
plot(st_geometry(fence4_buff))
#buffer Clip
sp_VF4_Pluse_clip_animalID_buff <- st_intersection(sp_VF4_Pluse_clip_animalID, fence4_buff)
ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence4, color = "green", fill = NA)+
  geom_sf(data = sp_VF4_Pluse_clip_animalID_buff)


##########       Final output here is  sp_VF4_Pluse_clip_animalID_buff                        ####################
st_write(sp_VF4_Pluse_clip_animalID_buff, "sp_VF4_Pluse_clip_animalID_buff.csv", layer_options = "GEOMETRY=AS_XY")





###########################################################################################################################################################
#####################################              check that this is useful ##############################################################################
############################################################################################################################################################

#### Can I just stick this to the end of my InclusionBord_ dataset?
head(sp_VF4_InclusionBord_animalID)
head(sp_VF4_Pluse_clip_animalID_buff)
sp_VF4_Pluse_clip_animalID_buff <- select(sp_VF4_Pluse_clip_animalID_buff,
                                          -Id,
                                          -BUFF_DIST,
                                          -ORIG_FID)

VF_4_InclusionBord_Pulse_buff <- rbind(sp_VF4_InclusionBord_animalID,
                                  sp_VF4_Pluse_clip_animalID_buff)


head(VF_4_InclusionBord_Pulse_buff)

Pulse_sum4 <- group_by(VF_4_InclusionBord_Pulse_buff, date, animal_ID, event) %>% 
  summarise(count = n())

print(Pulse_sum4)





VF_dates=data.frame(date=as.Date(c("2019-05-20", "2019-05-23", "2019-05-28", "2019-06-03")), 
             event=c("VF1&2", "VF3", "VF4", "VF5"))
filter(Pulse_sum4, count <50) %>% 
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
print(Aduio_sum4)
filter(Aduio_sum4, count <50 ) %>% 
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
head(sp_VF4_InclusionBord_animalID)
head(sp_VF4_Pluse_clip_animalID_buff)
head(sp_VF4_Audio_clip_animalID_buff)

VF_4_Audio_Pulse_buff <- rbind(sp_VF4_Audio_clip_animalID_buff,
                               sp_VF4_Pluse_clip_animalID_buff)

##########       Final output here is  VF_4_Audio_Pulse                        ####################
st_write(VF_4_Audio_Pulse_buff, "VF_4_Audio_Pulse_buff.csv", layer_options = "GEOMETRY=AS_XY")
st_write(VF_4_Audio_Pulse_buff, "VF_4_Audio_Pulse_buff.shp")
