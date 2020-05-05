###                 This uses the fence line that should have been used ####

head(VF_week1_2_3)
###########################################################################################################################################################
#############################            create a df for the audio                                                  #############################
###########################################################################################################################################################
### 1. the data I will use - this the same as the Fence1_data_clean.rds 
### W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence1_data_clean.rds
Fence1_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence1_data_clean.rds", refhook = NULL)
Fence2_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence2_data_clean.rds", refhook = NULL)
Fence3_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence3_data_clean.rds", refhook = NULL)
Fence4_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence4_data_clean.rds", refhook = NULL)
Fence5_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence5_data_clean.rds", refhook = NULL)




head(Fence1_Incl_animalID)
head(Fence2_Incl_animalID)
head(Fence3_Incl_animalID)
head(Fence4_Incl_animalID)
head(Fence5_Incl_animalID)

### Dana said that some animal may be receieving a weak signal and that we should check this
### looking for a low value or entry that says low.... I think I have removed the text???

unique(Fence1_Incl_animalID$value)


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
### 4. Recaldistance to VF

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

### Note that fence 4 is the problem one, I only want to keep redaing that are within 10m of the VF4 modified.
Fence4_cue_recal <- Fence4_cue_recal %>% 
  filter(distance_VF > -10)



#### 5. drop geometry?
Fence1_cue_recal_drop_geom <- st_set_geometry(Fence1_cue_recal, NULL)
Fence2_cue_recal_drop_geom <- st_set_geometry(Fence2_cue_recal, NULL)
Fence3_cue_recal_drop_geom <- st_set_geometry(Fence3_cue_recal, NULL)
Fence4_cue_recal_drop_geom <- st_set_geometry(Fence4_cue_recal, NULL)
Fence5_cue_recal_drop_geom <- st_set_geometry(Fence5_cue_recal, NULL)


head(Fence1_cue_recal_drop_geom)

#### 6. subset data to keep only audio started and pulse started as per Dana suggestion and merge all data together

Fence1_5_started_aduio_pulse <- rbind(Fence1_cue_recal_drop_geom,
                                      Fence2_cue_recal_drop_geom,
                                      Fence3_cue_recal_drop_geom,
                                      Fence4_cue_recal_drop_geom,
                                      Fence5_cue_recal_drop_geom) %>% 
  filter(event == "Audio started" |
           event == "Pulse started"
           )


dim(Fence1_5_started_aduio_pulse)
#### 7. summaries and graph it and save in...
#### 7a. path
graph_path <- file.path("W:", "VF", "Eden_valley", "graphs")
#### 7a. summary for animal
Fence1_5_started_aduio_pulse_summary <- Fence1_5_started_aduio_pulse %>% 
  group_by(event, animal_ID) %>% 
  summarise(count_cues = n()) 
            
#### 7a. graph counts per animal            
#Fence1_5_started_aduio_pulse_summary

Fence1_5_started_aduio_pulse_summary$event <- factor(Fence1_5_started_aduio_pulse_summary$event, 
                                                     levels=c("Audio started", "Pulse started"))

filter(Fence1_5_started_aduio_pulse_summary, animal_ID != "NA") %>% 
ggplot( aes(x = animal_ID, y = count_cues))+
  facet_wrap(.~event)+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Number of cues per animal",
       subtitle = "All days included",
       #caption = "cues are start aduio or start pulse",
       x= "animals",
       y = "count of cues")

ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_summary_animal1.png", device = "png" ,
       width = 20, height = 12, units = "cm")


#### 7a. graph counts per animal - stacked

Fence1_5_started_aduio_pulse_summary$event <- factor(Fence1_5_started_aduio_pulse_summary$event, 
                                                     levels=c("Pulse started", "Audio started"))

filter(Fence1_5_started_aduio_pulse_summary, animal_ID != "NA") %>% 
  ggplot( aes(x = animal_ID, y = count_cues))+
  #facet_wrap(.~event)+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col(aes(fill= event))+
  scale_fill_manual(values = c("red", "orange")) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "bottom",
        legend.title=element_blank())+
  labs(title= "Number of cues per animal",
       subtitle = "All days included",
       #caption = "cues are start aduio or start pulse",
       x= "animals",
       y = "count of cues")
ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_summary_animal_stacked.png", device = "png",
       width = 20, height = 12, units = "cm")





#### 7b. summary for day
head(Fence1_5_started_aduio_pulse)
Fence1_5_started_aduio_pulse_summary2 <- 
  filter(Fence1_5_started_aduio_pulse, animal_ID != "NA") %>% 
  group_by(event, day_since_start) %>% 
  summarise(count_cues = n()) 

head(filter(Fence1_5_started_aduio_pulse_summary2,day_since_start==9 ))


  ggplot(Fence1_5_started_aduio_pulse_summary2,aes(x = day_since_start, y = count_cues))+
  facet_wrap(.~event)+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Number of cues per day since start of trial",
       subtitle = "All animals included",
       #caption = "cues are start aduio or start pulse",
       x= "days since start of trail",
       y = "count of cues")

ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_summary_days1.png", device = "png" ,
       width = 20, height = 12, units = "cm")

Fence1_5_started_aduio_pulse_summary2$event <- factor(Fence1_5_started_aduio_pulse_summary2$event, 
                                                     levels=c("Pulse started", "Audio started"))

### stacked per day

  ggplot(Fence1_5_started_aduio_pulse_summary2, aes(x = day_since_start, y = count_cues))+
  #facet_wrap(.~event)+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col(aes(fill= event))+
  scale_fill_manual(values = c( "red", "orange")) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "bottom",
        legend.title=element_blank())+
    labs(title= "Number of cues per day since start of trial",
         subtitle = "All animals included",
         #caption = "cues are start aduio or start pulse",
         x= "days since start of trial",
         y = "count of cues")
ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_summary_day_stacked.png", device = "png",
       width = 20, height = 12, units = "cm")


##################################################################################
#### Extra calulations ratio of pulse to audio and audio - pulse data

head(Fence1_5_started_aduio_pulse)
head(Fence1_5_started_aduio_pulse_summary)
library(tidyr)
pivot_wider
#### Animal summary
Fence1_5_started_aduio_pulse_ratio <- Fence1_5_started_aduio_pulse_summary %>% 
  spread(event, count_cues ) %>% 
  mutate(ratio = `Pulse started`/ `Audio started`,
         subtract = `Audio started` - `Pulse started`)

head(Fence1_5_started_aduio_pulse_ratio)

filter(Fence1_5_started_aduio_pulse_ratio, animal_ID != "NA") %>% 
ggplot(aes(x = animal_ID, y = ratio))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Ratio of cues per animal",
       subtitle = "All days included",
       #caption = "cues are start aduio or start pulse",
       x= "animals",
       y = "ratio of pulse to audio cues")


ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_ratio_animals1.png", device = "png" ,
       width = 20, height = 12, units = "cm")

head(Fence1_5_started_aduio_pulse_ratio)
filter(Fence1_5_started_aduio_pulse_ratio, animal_ID != "NA") %>% 
  ggplot(aes(x = animal_ID, y = subtract))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Audio cues minus pulse cues per animal",
       subtitle = "All days included",
       #caption = "cues are start aduio or start pulse",
       x= "animals",
       y = "Difference between pulse and audio cues")


ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_Diff_animals1.png", device = "png" ,
       width = 20, height = 12, units = "cm")




#### days from trial summary
head(Fence1_5_started_aduio_pulse_summary2)
Fence1_5_started_aduio_pulse_ratio_day <- Fence1_5_started_aduio_pulse_summary2 %>% 
  spread(event, count_cues ) %>% 
  mutate(ratio = `Pulse started`/ `Audio started`,
         subtract = `Audio started` - `Pulse started`)

head(Fence1_5_started_aduio_pulse_ratio_day)


  ggplot(Fence1_5_started_aduio_pulse_ratio_day, aes(x = day_since_start, y = ratio))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Ratio of cues per day",
       subtitle = "All animals included",
       #caption = "cues are start aduio or start pulse",
       x= "days since start of trial",
       y = "ratio of pulse to audio cues")


ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_ratio_days1.png", device = "png" ,
       width = 20, height = 12, units = "cm")

ggplot(Fence1_5_started_aduio_pulse_ratio_day, aes(x = day_since_start, y = subtract))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Audio cues minus pulse cues per day",
       subtitle = "All animals included",
       #caption = "cues are start aduio or start pulse",
       x= "days since start of trial",
       y = "Difference between pulse and audio cues")


ggsave(path= graph_path, filename = "Fence1_5_started_aduio_pulse_diff_days1.png", device = "png" ,
       width = 20, height = 12, units = "cm")



#####################################################################################################################
#### DAY 9 problem - now fixed with removing data that was more than 10 meter from revised VF4

Fence3_4_cue_recal <- rbind(Fence3_cue_recal, Fence4_cue_recal) %>%
  filter(day_since_start == 9) %>%
  filter(event == "Audio started" |
           event == "Pulse started")
head(Fence3_4_cue_recal)


output_folder <- file.path("W:", "VF", "Eden_valley", "logged_VF_data", "Jax_Dec_2019_processing")
Fence3_4_cue_recal_day9csv <- paste0(output_folder,"/Fence3_4_cue_recal_day9.csv")
st_write(Fence3_4_cue_recal, Fence3_4_cue_recal_day9csv, layer_options = "GEOMETRY=AS_XY")



####################################################################################################################
####### Ratio of cues to pulses for the paper

head(Fence1_5_started_aduio_pulse)
unique(Fence1_5_started_aduio_pulse$day_since_start)
#remove the animals with NA
unique(Fence1_5_started_aduio_pulse$animal_ID)
ratio_for_paper <- filter(Fence1_5_started_aduio_pulse, animal_ID != "NA")
unique(ratio_for_paper$animal_ID)
unique(ratio_for_paper$day_since_start)
#remove problem cows for days 36-39


ratio_for_paper_remove_prob_cows <- filter(ratio_for_paper,between(day_since_start, 1,35) |  day_since_start > 39 & !animal_ID %in% c("Q26", "Q29", "Q36")) 
unique(ratio_for_paper_remove_prob_cows$day_since_start)
test_filter <- filter(ratio_for_paper_remove_prob_cows,animal_ID == "Q36" )
unique(test_filter$day_since_start)
unique(test_filter$animal_ID)

##### what do I want to event summaries?

unique(ratio_for_paper_remove_prob_cows$event)

#### Ratio of cues per animal
ratio_for_paper_remove_prob_cows_summary <- ratio_for_paper_remove_prob_cows %>% 
  group_by(event, animal_ID) %>% 
  summarise(count_cues = n()) 

ratio_for_paper_remove_prob_cows_summary
ratio_for_paper_remove_prob_cows_summary_wide <- spread(ratio_for_paper_remove_prob_cows_summary, event, count_cues)

ratio_for_paper_remove_prob_cows_summary_wide <- mutate(ratio_for_paper_remove_prob_cows_summary_wide,
                                                        ratio = (`Pulse started`/`Audio started`)*100)

ratio_for_paper_remove_prob_cows_summary_wide
write.csv(ratio_for_paper_remove_prob_cows_summary_wide, file = "W:/VF/Eden_Valley/temp_graphs/ratio_remove_prob_cows_summary_per_cow.csv")

#### Ratio of cues per day
ratio_for_paper_remove_prob_cows_summary_per_day <- ratio_for_paper_remove_prob_cows %>% 
  group_by(event, day_since_start) %>% 
  summarise(count_cues = n()) 

ratio_for_paper_remove_prob_cows_summary_per_day
ratio_for_paper_remove_prob_cows_summary_per_day_wide <- spread(ratio_for_paper_remove_prob_cows_summary_per_day, event, count_cues)

ratio_for_paper_remove_prob_cows_summary_per_day_wide <- mutate(ratio_for_paper_remove_prob_cows_summary_per_day_wide,
                                                        ratio = (`Pulse started`/`Audio started`)*100)

ratio_for_paper_remove_prob_cows_summary_per_day_wide
write.csv(ratio_for_paper_remove_prob_cows_summary_per_day_wide, file = "W:/VF/Eden_Valley/temp_graphs/ratio_remove_prob_cows_summary_per_day.csv")


#### Ratio of cues whole data set
ratio_for_paper_remove_prob_cows_summary <- ratio_for_paper_remove_prob_cows %>% 
  group_by(event) %>% 
  summarise(count_cues = n()) 

ratio_for_paper_remove_prob_cows_summary
ratio_for_paper_remove_prob_cows_summary_wide <- spread(ratio_for_paper_remove_prob_cows_summary, event, count_cues)
ratio_for_paper_remove_prob_cows_summary_wide
ratio_for_paper_remove_prob_cows_summary_wide <- mutate(ratio_for_paper_remove_prob_cows_summary_wide,
                                                                ratio = (`Pulse started`/`Audio started`)*100)

ratio_for_paper_remove_prob_cows_summary_wide
write.csv(ratio_for_paper_remove_prob_cows_summary_wide, file = "W:/VF/Eden_Valley/temp_graphs/ratio_for_paper_remove_prob_cows_summary_wide.csv")
