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

#Bring in files

### 9a. paddock bounadry
eden_valley <- st_read("W:/VF/Eden_Valley/VF_Boundary/EdenValley_site1GDA_a.shp")
#assign a coord ref epsg
st_crs(eden_valley) <- 28354

#make this the epsg that will be used for all data
the_crs <- st_crs(eden_valley, asText = TRUE)



input_file <- file.path("W:", "VF", "Eden_valley", "logged_VF_data", "Jax_Dec_2019_processing")
setwd(input_file)
getwd()


VF1_recal_exclsuion_only <- read_csv("VF1_recal_exclsuion_only.csv" )
VF2_recal_exclsuion_only <- read_csv("VF2_recal_exclsuion_only.csv" )
VF3_recal_exclsuion_only <- read_csv("VF3_recal_exclsuion_only.csv" )
VF4_recal_exclsuion_only <- read_csv("VF4_recal_exclsuion_only.csv" )
#VF5_recal_exclsuion_only <- read_csv("VF5_recal_exclsuion_only.csv" )
VF5_recal_exclsuion_only1 <- read_csv("VF5_recal_exclsuion_only1.csv" )


dim(VF1_recal_exclsuion_only)
dim(VF2_recal_exclsuion_only)
dim(VF3_recal_exclsuion_only)
dim(VF4_recal_exclsuion_only)
dim(VF5_recal_exclsuion_only)

# How many rows are missing animal ID?
dim(VF1_recal_exclsuion_only)  #2567 
dim(filter(VF1_recal_exclsuion_only, animal_ID != "NA")) #2567
dim(filter(VF1_recal_exclsuion_only, animal_ID == "NA")) # 0 all pts HAVE have animal ID
head(VF1_recal_exclsuion_only$animal_ID)
unique(VF1_recal_exclsuion_only$animal_ID)

dim(VF2_recal_exclsuion_only)  #2989 
dim(filter(VF2_recal_exclsuion_only, animal_ID != "NA")) #2989
dim(filter(VF2_recal_exclsuion_only, animal_ID == "NA")) # 0 all pts HAVE have animal ID
head(VF2_recal_exclsuion_only$animal_ID)
unique(VF2_recal_exclsuion_only$animal_ID)

dim(VF3_recal_exclsuion_only)  #4781 
dim(filter(VF3_recal_exclsuion_only, animal_ID != "NA")) #4781
dim(filter(VF3_recal_exclsuion_only, animal_ID == "NA")) # 0 all pts HAVE have animal ID
head(VF3_recal_exclsuion_only$animal_ID)
unique(VF3_recal_exclsuion_only$animal_ID)

dim(VF4_recal_exclsuion_only)  #7169 
dim(filter(VF4_recal_exclsuion_only, animal_ID != "NA")) #7169
dim(filter(VF4_recal_exclsuion_only, animal_ID == "NA")) # 0 all pts HAVE have animal ID
head(VF4_recal_exclsuion_only$animal_ID)
unique(VF4_recal_exclsuion_only$animal_ID)

dim(VF5_recal_exclsuion_only1)  #249083  
dim(filter(VF5_recal_exclsuion_only1, animal_ID != "NA")) #249083 
dim(filter(VF5_recal_exclsuion_only1, animal_ID == "NA")) # 0 all pts HAVE have animal ID
head(VF5_recal_exclsuion_only1$animal_ID)
unique(VF5_recal_exclsuion_only1$animal_ID)




#check of how the event number are caluated - looks like its per event
head(VF3_recal_exclsuion_only)
ggplot(VF3_recal_exclsuion_only, aes(x = animal_ID, y = event_number))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_point()+
  facet_wrap(.~ day_since_start)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))


VF1_5_recal_exclsuion_only <- rbind(VF1_recal_exclsuion_only,
                                 VF2_recal_exclsuion_only,
                                 VF3_recal_exclsuion_only,                                 VF4_recal_exclsuion_only,
                                 VF5_recal_exclsuion_only1)
unique(VF1_5_recal_exclsuion_only$animal_ID)

head(VF1_5_recal_exclsuion_only)

VF1_5_recal_exclsuion_only$date <- as_date(VF1_5_recal_exclsuion_only$date)
VF1_5_recal_exclsuion_only$time <- as_datetime(VF1_5_recal_exclsuion_only$time, tz="GMT")
#VF1_5_recal_incl_events <- mutate(VF1_5_recal_incl_events,
#                                     hms = hms::as.hms(time, tz="GMT"))
#assign  coords for each of the VF dataframes
# VF1_5_recal_incl_events <- st_as_sf(VF1_5_recal_incl_events, 
#                                           coords = c("X", "Y"), 
#                                           crs = the_crs)
# head(VF1_5_recal_incl_events)






##################################################################################################################
### 12. summaries the incursion events 

head(VF1_5_recal_exclsuion_only)

### 12 i. summaries the incursion events - what is the max many events per animal per day?
summary_incursion_max_animal_perday <- function(df){
  #  summaries the data 
  VF_inc_events_sum <- filter(df, event_number != "NA") %>% 
    group_by( animal_ID, day_since_start) %>% 
    summarise(count_events = n(), 
              max_value = max(event_number)
              )
 
  ###  if I have an NA value replace it with 0
  VF_inc_events_sum$count_events[is.na(VF_inc_events_sum$count_events)] <- 0
   return(VF_inc_events_sum)
}

Vf1_5summary_incursion_max_animal_perday<- summary_incursion_max_animal_perday(VF1_5_recal_exclsuion_only)
head(Vf1_5summary_incursion_max_animal_perday)

#now what is the sum of the max value per animal
Vf1_5sum_max_animal_perday <-
  Vf1_5summary_incursion_max_animal_perday %>%
  group_by(animal_ID) %>%
  summarise(sum_max_value = sum(max_value))

Vf1_5sum_max_animal_perday


ggplot(Vf1_5sum_max_animal_perday, aes(x = animal_ID, y = sum_max_value))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Number of events per animal",
       subtitle = "All days included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animals",
       y = "sum of max number of events per day")

#now what is the sum of the max value per day
Vf1_5sum_max_perday <-
  Vf1_5summary_incursion_max_animal_perday %>%
  group_by(day_since_start) %>%
  summarise(sum_max_value = sum(max_value))

Vf1_5sum_max_perday

ggplot(Vf1_5sum_max_perday, aes(x = day_since_start, y = sum_max_value))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Number of events per day",
       subtitle = "All animals included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "days since start of trial",
       y = "sum of max number of events per day")

###########################################################
### what about the time spent in the non grazing zone.
### for each animal per day per event what is the time spent?
#############################################################
head(VF1_5_recal_exclsuion_only)
VF_inc_events_time_period <- filter(VF1_5_recal_exclsuion_only,
                                    event_number != "NA") %>%
  group_by(day_since_start, animal_ID, event_number ) %>%
  summarise(max_time = max(as_datetime(time, tz="GMT")), 
            min_time = min(as_datetime(time, tz="GMT")),
            period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))
    
head(VF_inc_events_time_period)   
  
### This is the for each day and each animal and each event - the time period.
### So what is the avearge / sum of these time periods?

#now what is the sum of the period value per animal
Vf1_5sum_time_period_animal <-
  VF_inc_events_time_period %>%
  group_by(animal_ID) %>%
  summarise(sum_time_period = sum(period_time))

head(Vf1_5sum_time_period_animal)
Vf1_5sum_time_period_animal <- mutate(
  Vf1_5sum_time_period_animal,
  mins = 
  seconds_to_period(Vf1_5sum_time_period_animal$sum_time_period),
  minutes = floor(Vf1_5sum_time_period_animal$sum_time_period/60),
  hours = floor(Vf1_5sum_time_period_animal$sum_time_period/(60*60)))

head(Vf1_5sum_time_period_animal)

ggplot(Vf1_5sum_time_period_animal, aes(x = animal_ID, y = minutes))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Sum of event time per animal",
       subtitle = "All days included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animals",
       y = "sum of event time (minutes)")

#now what is the average of the period value per animal
Vf1_5av_time_period_animal <-
  VF_inc_events_time_period %>%
  group_by(animal_ID) %>%
  summarise(av_time_period = mean(period_time))

head(Vf1_5av_time_period_animal)
Vf1_5av_time_period_animal <- mutate(
  Vf1_5av_time_period_animal,
  mins = 
    seconds_to_period(Vf1_5av_time_period_animal$av_time_period),
  minutes = floor(Vf1_5av_time_period_animal$av_time_period/60),
  hours = floor(Vf1_5av_time_period_animal$av_time_period/(60*60)))

head(Vf1_5av_time_period_animal)

ggplot(Vf1_5av_time_period_animal, aes(x = animal_ID, y = av_time_period))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Average of event time per animal",
       subtitle = "All days included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animals",
       y = "average of event time (seconds)")

##############################################################
head(VF1_5_recal_exclsuion_only)
VF_inc_events_time_period <- filter(VF1_5_recal_exclsuion_only,
                                    event_number != "NA") %>%
  group_by(day_since_start, animal_ID, event_number ) %>%
  summarise(max_time = max(as_datetime(time, tz="GMT")), 
            min_time = min(as_datetime(time, tz="GMT")),
            period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))

head(VF_inc_events_time_period) 


#now what is the sum of the period value per DAY
Vf1_5sum_time_period_DAY <-
  VF_inc_events_time_period %>%
  group_by(day_since_start) %>%
  summarise(sum_time_period = sum(period_time))

head(Vf1_5sum_time_period_DAY)
Vf1_5sum_time_period_DAY <- mutate(
  Vf1_5sum_time_period_DAY,
  mins = 
    seconds_to_period(Vf1_5sum_time_period_DAY$sum_time_period),
  minutes = floor(Vf1_5sum_time_period_DAY$sum_time_period/60),
  hours = floor(Vf1_5sum_time_period_DAY$sum_time_period/(60*60)))

head(Vf1_5sum_time_period_DAY)

ggplot(Vf1_5sum_time_period_DAY, aes(x = day_since_start, y = sum_time_period))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Sum of event time per day",
       subtitle = "All animal included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "sum of event time (seconds)")


#now what is the avearge of the period value per DAY
Vf1_5av_time_period_DAY <-
  VF_inc_events_time_period %>%
  group_by(day_since_start) %>%
  summarise(av_time_period = mean(period_time))

head(Vf1_5av_time_period_DAY)
Vf1_5av_time_period_DAY <- mutate(
  Vf1_5av_time_period_DAY,
  mins = 
    seconds_to_period(Vf1_5av_time_period_DAY$av_time_period),
  minutes = floor(Vf1_5av_time_period_DAY$av_time_period/60),
  hours = floor(Vf1_5av_time_period_DAY$av_time_period/(60*60)))

head(Vf1_5av_time_period_DAY)

ggplot(Vf1_5av_time_period_DAY, aes(x = day_since_start, y = av_time_period))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Average of event time per day",
       subtitle = "All animal included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "average of event time (seconds)")


################################################################
### Animals / events at different distance from VF
head(VF1_5_recal_exclsuion_only)
max_event_dist <- group_by(VF1_5_recal_exclsuion_only,
                      day_since_start, animal_ID, event_number) %>%
  summarise(
    max_distance = max(
      distance_VF,
      na.rm = TRUE),
      max_time = max(as_datetime(time, tz = "GMT")),
      min_time = min(as_datetime(time, tz = "GMT")),
      period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))
head(max_event_dist)

#event_max$day_since_start_factor <- factor(event_max$day_since_start)

### 13b. keep values greater than 2m/5m/10m


  #create df with only keeping events with a certain distance from VF
  Max_dist_filter2m <- filter(max_event_dist,
                         max_distance > 2)
  Max_dist_filter5m <- filter(max_event_dist,
                         max_distance > 5)
  Max_dist_filter10m <- filter(max_event_dist,
                          max_distance > 10)
  Max_dist_filter20m <- filter(max_event_dist,
                          max_distance > 20)
  Max_dist_filter30m <- filter(max_event_dist,
                          max_distance > 30) 
  Max_dist_filter40m <- filter(max_event_dist,
                          max_distance > 40) 
  
  # Cal the avearge time for events 2m/5m/10m/20m/30m/40m
  
  Max_dist_filter2m_av <-  Max_dist_filter2m %>%
    group_by(day_since_start, animal_ID) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 2)
  
  Max_dist_filter5m_av <-  Max_dist_filter5m %>%
    group_by(day_since_start, animal_ID) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 5)
  
  Max_dist_filter10m_av <-  Max_dist_filter10m %>%
    group_by(day_since_start, animal_ID) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 10)
  
  Max_dist_filter20m_av <-  Max_dist_filter20m %>%
    group_by(day_since_start, animal_ID) %>%
    summarise(average_time = mean(period_time) ,
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 20)
  
  Max_dist_filter30m_av <-  Max_dist_filter30m %>%
    group_by(day_since_start, animal_ID) %>%
    summarise(average_time = mean(period_time) ,
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 30)
  
  Max_dist_filter40m_av <-  Max_dist_filter40m %>%
    group_by(day_since_start, animal_ID) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 40)
  
  #merge the output togther
  Max_dist_filter2_40m_av <- rbind( Max_dist_filter2m_av,
                                    Max_dist_filter5m_av,
                                   Max_dist_filter10m_av,
                                   Max_dist_filter30m_av,
                                   Max_dist_filter40m_av
                                  )
  
  Max_dist_filter2_40m_av$day_since_start_factor <-
    factor(Max_dist_filter2_40m_av$day_since_start)

head( Max_dist_filter2_40m_av)
########################################################
### another grouping animals ### CHECK THIS

Summary_Max_dist_filter2_40m_av <- Max_dist_filter2_40m_av %>%
  group_by(day_since_start, max_dist_filter) %>%
  summarise(
    sum_time = sum(sum_time),
    n = n())

Summary_Max_dist_filter2_40m_av 



ggplot(Summary_Max_dist_filter2_40m_av, aes(x = day_since_start, y = sum_time))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Sum of event time per day",
       subtitle = "Events for animal and days retained and summed
       for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "sum of event time (seconds)")



ggplot(Summary_Max_dist_filter2_40m_av, aes(x = day_since_start, y = n))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Number of events per day",
       subtitle = "Events for animal and days retained and counted
       for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "count of event ")
################################################################
#### Now by aniamls
Summary2_Max_dist_filter2_40m_av <- Max_dist_filter2_40m_av %>%
  group_by(animal_ID, max_dist_filter) %>%
  summarise(
    sum_time = sum(sum_time),
    n = n())

Summary2_Max_dist_filter2_40m_av 
ggplot(Summary2_Max_dist_filter2_40m_av, aes(x = animal_ID, y = sum_time))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Sum of event time per animal",
       subtitle = "Events for animal and days retained and summed
       for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animal",
       y = "sum of event time (seconds)")



ggplot(Summary2_Max_dist_filter2_40m_av, aes(x = animal_ID, y = n))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Number of events per animal",
       subtitle = "Events for animal and days retained and counted
       for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animal",
       y = "count of event ")





  






















##############################################################################
### 13. more alnalysis on incursion events, max distance from VF

## The output of 12. can be used as input for these two functions
## 13a. Cal the max distance per day for each event
## 13b. Cal the incursion events with defined max distance 

### 13a. Summary of max distance inside VF as a function
event_max <- function(VFx_summary_incursion_data) {
  
  event_max <- group_by(VFx_summary_incursion_data, day_since_start) %>%
    summarise(max_event = max(event_number,  na.rm = TRUE))
  event_max$day_since_start_factor <- factor(event_max$day_since_start)
  return(event_max)
}

VF1_event_max <- event_max(VF1_summary_incursion_data)
VF2_event_max <- event_max(VF2_summary_incursion_data)
VF3_event_max <- event_max(VF3_summary_incursion_data)
VF4_event_max <- event_max(VF4_summary_incursion_data)
VF5_event_max <- event_max(VF5_summary_incursion_data)

VF1_5_event_max <- rbind(VF1_event_max,
                         VF2_event_max,
                         VF3_event_max,
                         VF4_event_max,
                         VF5_event_max)

VF1_5_event_max

### 13b. keep values greater than 2m/5m/10m

filter_max_dist_inc <- function(VFx_summary_incursion_data){
  #create df with only keeping events with a certain distance from VF
  VF1_filter2m <- filter(VFx_summary_incursion_data,
                         max_dist > 2)
  VF1_filter5m <- filter(VFx_summary_incursion_data,
                         max_dist > 5)
  VF1_filter10m <- filter(VFx_summary_incursion_data,
                          max_dist > 10)
  VF1_filter20m <- filter(VFx_summary_incursion_data,
                          max_dist > 20)
  VF1_filter30m <- filter(VFx_summary_incursion_data,
                          max_dist > 30) 
  VF1_filter40m <- filter(VFx_summary_incursion_data,
                          max_dist > 40) 
  
  # Cal the avearge time for events 2m/5m/10m/20m/30m/40m
  
  VF1_filter2m_ave <-  VF1_filter2m %>%
    group_by(day_since_start) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 2)
  
  VF1_filter5m_ave <-  VF1_filter5m %>%
    group_by(day_since_start) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 5)
  
  VF1_filter10m_ave <-  VF1_filter10m %>%
    group_by(day_since_start) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 10)
  
  VF1_filter20m_ave <-  VF1_filter20m %>%
    group_by(day_since_start) %>%
    summarise(average_time = mean(period_time) ,
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 20)
  
  VF1_filter30m_ave <-  VF1_filter30m %>%
    group_by(day_since_start) %>%
    summarise(average_time = mean(period_time) ,
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 30)
  
  VF1_filter40m_ave <-  VF1_filter40m %>%
    group_by(day_since_start) %>%
    summarise(average_time = mean(period_time),
              sum_time = sum(period_time),
              n = n()) %>%
    mutate(max_dist_filter = 40)
  
  #merge the output togther
  VF1_filter_5to40m_ave <- rbind(VF1_filter2m_ave,
                                 VF1_filter5m_ave,
                                 VF1_filter10m_ave,
                                 VF1_filter20m_ave,
                                 VF1_filter30m_ave,
                                 VF1_filter40m_ave)
  
  VF1_filter_5to40m_ave$day_since_start_factor <-
    factor(VF1_filter_5to40m_ave$day_since_start)
  return(VF1_filter_5to40m_ave)
}

### 13b. use the function (which summaries of max distance inside VF)
VF1_filter_max_dist_inc <- filter_max_dist_inc(VF1_summary_incursion_data)
VF2_filter_max_dist_inc <- filter_max_dist_inc(VF2_summary_incursion_data)
VF3_filter_max_dist_inc <- filter_max_dist_inc(VF3_summary_incursion_data)
VF4_filter_max_dist_inc <- filter_max_dist_inc(VF4_summary_incursion_data)
VF5_filter_max_dist_inc <- filter_max_dist_inc(VF5_summary_incursion_data)

VF1_5_filter_max_dist_inc <- rbind(VF1_filter_max_dist_inc,
                                   VF2_filter_max_dist_inc,
                                   VF3_filter_max_dist_inc,
                                   VF4_filter_max_dist_inc,
                                   VF5_filter_max_dist_inc)

##################################################################################################################


head(VF1_5_filter_max_dist_inc)
#Plot data??
VF1_5_filter_max_dist_inc$max_dist_filter_factor <-
  factor(VF1_5_filter_max_dist_inc$max_dist_filter)

### max distance threshold vs number of event
ggplot(VF1_5_filter_max_dist_inc, aes(x = max_dist_filter_factor, y = n))+
  #geom_point()+
  geom_boxplot()+
  #facet_wrap(.~date_factor)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #scale_x_continuous(breaks =  c(2,5,10,20))+
  labs(title= "",
       x= "events that animal reached a max distance greater than",
       y = "number of events")

VF1_5_filter_max_dist_inc$day_since_start_factor

### days since start vs number of event
VF1_5_filter_max_dist_inc

ggplot(VF1_5_filter_max_dist_inc, aes(x = day_since_start, y = n))+
  facet_wrap(.~ max_dist_filter)+
  geom_point()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  labs(title= "",
       x= "days since start",
       y = "number of events")

### days since start vs average time of event
ggplot(VF1_5_filter_max_dist_inc, aes(x = day_since_start, y = average_time))+
  geom_point()+
  facet_wrap(.~max_dist_filter_factor)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  labs(title= "",
       x= "Day since start",
       y = "average time spent in non grazing zone per event")

### days since start vs sum time of event
ggplot(VF1_5_filter_max_dist_inc, aes(x = day_since_start, y = sum_time))+
  geom_point()+
  facet_wrap(.~max_dist_filter_factor)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  labs(title= "",
       x= "Day since start",
       y = "sum of time spent in non grazing zone per event (seconds)")
