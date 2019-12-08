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


#### 1. Bring in files
## 1a.locate the raw data folder
## The location of my data will depend on the computer I am working on...

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



### 1ba. bring in the paddock bounadry
eden_valley <- st_read("W:/VF/Eden_Valley/VF_Boundary/EdenValley_site1GDA_a.shp")
#assign a coord ref epsg
st_crs(eden_valley) <- 28354

#make this the epsg that will be used for all data
the_crs <- st_crs(eden_valley, asText = TRUE)

input_file <- file.path("W:", "VF", "Eden_valley", "logged_VF_data", "Jax_Dec_2019_processing")
setwd(input_file)
getwd()

### 1a. Might need this is I am bringing in the data from file - notused for now

#df$date <- as_date(df$date)
# df$time <- as_datetime(df$time, tz="GMT")
#df <- mutate(df,
#                                     hms = hms::as.hms(time, tz="GMT"))
#assign  coords for each of the VF dataframes
# df <- st_as_sf(df, 
#                                           coords = c("X", "Y"), 
#                                           crs = the_crs)

### 1c. check the data


head(VF1_5_recal_incl_events)
#dont need this at the monment its in the environments as VF1_5_recal_incl_events
# VF1_recal_exclsuion_only <- read_csv("Fence1_5exclsuion_only.csv")

#files seperate
# VF1_recal_exclsuion_only <- read_csv("VF1_recal_exclsuion_only.csv" )
# VF2_recal_exclsuion_only <- read_csv("VF2_recal_exclsuion_only.csv" )
# VF3_recal_exclsuion_only <- read_csv("VF3_recal_exclsuion_only.csv" )
# VF4_recal_exclsuion_only <- read_csv("VF4_recal_exclsuion_only.csv" )
# #VF5_recal_exclsuion_only <- read_csv("VF5_recal_exclsuion_only.csv" )
# VF5_recal_exclsuion_only1 <- read_csv("VF5_recal_exclsuion_only1.csv" )

dim(VF1_5_recal_incl_events)

# dim(VF1_recal_exclsuion_only)
# dim(VF2_recal_exclsuion_only)
# dim(VF3_recal_exclsuion_only)
# dim(VF4_recal_exclsuion_only)
# dim(VF5_recal_exclsuion_only)

# How many rows are missing animal ID?

dim(VF1_5_recal_incl_events)  #266589 
dim(filter(VF1_5_recal_incl_events, animal_ID != "NA")) #266589
dim(filter(VF1_5_recal_incl_events, animal_ID == "NA")) # 0 all pts HAVE have animal ID
head(VF1_5_recal_incl_events$animal_ID)
unique(VF1_5_recal_incl_events$animal_ID)
unique(VF1_5_recal_incl_events$day_since_start)

### 1d. check that the events are incrementing per day and not reset over VF - all good:)
max(VF1_5_recal_incl_events$event_number)

filter(VF1_5_recal_incl_events, day_since_start == 39) %>% 
  ggplot(aes(animal_ID, event_number))+
  geom_point()

filter(VF1_5_recal_incl_events, day_since_start == 2) %>% 
  ggplot(aes(animal_ID, event_number))+
  geom_point()

filter(VF1_5_recal_incl_events, day_since_start == 3) %>% 
  ggplot(aes(animal_ID, event_number))+
  geom_point()

filter(VF1_5_recal_incl_events, day_since_start == 4) %>% 
  ggplot(aes(animal_ID, event_number))+
  geom_point()

filter(VF1_5_recal_incl_events, day_since_start == 5) %>% 
  ggplot(aes(animal_ID, event_number))+
  geom_point()


### 1c. check of how the event number are caluated - looks like its per event
head(VF1_5_recal_incl_events)
ggplot(VF1_5_recal_incl_events, aes(x = animal_ID, y = event_number))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_point()+
  facet_wrap(.~ day_since_start)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))

# unique(VF5_recal_exclsuion_only1$animal_ID)

####################################################################################################################
### 2. set up for graphing..
### 2a. path for saving graphs
graph_path <- file.path("W:", "VF", "Eden_valley", "graphs")

##################################################################################################################
### 3-x. summaries the incursion events 

head(VF1_5_recal_incl_events)

### 3a. summaries the incursion events - what is the max events per animal per day?
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
### 3aa. running the function
Vf1_5summary_incursion_max_animal_perday<- summary_incursion_max_animal_perday(VF1_5_recal_incl_events)




unique(VF1_5_recal_incl_events$day_since_start)
head(Vf1_5summary_incursion_max_animal_perday)
#code max value as double
Vf1_5summary_incursion_max_animal_perday$max_value <- as.double(Vf1_5summary_incursion_max_animal_perday$max_value)

### 3b.Group again - what is the sum of the max value per animal
Vf1_5sum_max_perday <-
  Vf1_5summary_incursion_max_animal_perday %>%
  group_by(animal_ID) %>%
  summarise(sum_max_value = sum(max_value))

Vf1_5sum_max_perday


ggplot(Vf1_5sum_max_perday, aes(x = animal_ID, y = sum_max_value))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Number of events per animal",
       subtitle = "All days included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animals",
       y = "sum events")

ggsave(path= graph_path, filename = "Number_events_per_animal.png", device = "png" ,
       width = 20, height = 12, units = "cm")

########################################################################################################

### 3c.Group again - what is the sum of the max value per DAY

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
       y = "sum events per day")

ggsave(path= graph_path, filename = "Number_events_per_day.png", device = "png" ,
       width = 20, height = 12, units = "cm")


###########################################################
### 4a. what about the time spent in the non grazing zone.
### for each animal per day per event what is the time spent?


VF1_5_recal_incl_events
head(VF1_5_recal_incl_events)
VF_inc_events_time_period <- filter(VF1_5_recal_incl_events,
                                    event_number != "NA") %>%
  group_by(day_since_start, animal_ID, event_number ) %>%
  summarise(max_time = max(as_datetime(time, tz="GMT")), 
            min_time = min(as_datetime(time, tz="GMT")),
            period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))
    
head(VF_inc_events_time_period)   
  
### 4a. SUM of time spent in the non grazing zone per ANIMAL.

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

ggplot(Vf1_5sum_time_period_animal, aes(x = animal_ID, y = sum_time_period))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Sum of time per animal (seconds)",
       subtitle = "All days included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animals",
       y = "sum of time (second)")

ggsave(path= graph_path, filename = "sum_events_per_animal_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")

ggplot(Vf1_5sum_time_period_animal, aes(x = animal_ID, y = minutes))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title= "Sum of event time per animal (minutes)",
       subtitle = "All days included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animals",
       y = "sum time (minutes)")

ggsave(path= graph_path, filename = "sum_events_per_animal_mins.png", device = "png" ,
       width = 20, height = 12, units = "cm")

### 4b. AVERAGE of time spent in the non grazing zone per ANIMAL.

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
  labs(title= "Average of time per animal (seconds)",
       subtitle = "All days included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animals",
       y = "average of event time (seconds)")


ggsave(path= graph_path, filename = "average_events_per_animal_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")

##############################################################


### 4c. SUM of time spent in the non grazing zone per DAY 

head(VF_inc_events_time_period) #calulated at start of 4

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
  labs(title= "Sum of time per day (seconds)",
       subtitle = "All animal included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "sum of time (seconds)")

ggsave(path= graph_path, filename = "sum_time_per_day_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")

### 4d. Average of time spent in the non grazing zone per DAY 

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
  labs(title= "Average event time per day (seconds)",
       subtitle = "All animal included",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "average of event time (seconds)")

ggsave(path= graph_path, filename = "average_events_per_day_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")







#############################################################################################
### 5. subsetting the data into groups - Animals / events at different distance from VF

head(VF1_5_recal_incl_events) #start data
### 5a. group the data by day animal and event then get the max distance travelled from the vf for event
max_event_dist <- group_by(VF1_5_recal_incl_events,
                      day_since_start, animal_ID, event_number) %>%
  summarise(
    max_distance = max(
      distance_VF,
      na.rm = TRUE),
      max_time = max(as_datetime(time, tz = "GMT")),
      min_time = min(as_datetime(time, tz = "GMT")),
      period_time = round((time_in_exlusion_zone = max_time - min_time), digits = 1))
head(max_event_dist)
dim(max_event_dist)
#event_max$day_since_start_factor <- factor(event_max$day_since_start)

### 5b. subset data keeping values greater than 2m/5m/10m/20m/30/40m


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
  
### 5c. cal the avearge and sum and count of time per day and animal for events with max distance of grater than 2m/5m/10m/20m/30m/40m
  
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
                                   Max_dist_filter20m_av,
                                   Max_dist_filter30m_av,
                                   Max_dist_filter40m_av
                                  )
  
  Max_dist_filter2_40m_av$day_since_start_factor <-
    factor(Max_dist_filter2_40m_av$day_since_start)

head( Max_dist_filter2_40m_av)
dim(Max_dist_filter2_40m_av)



########################################################
### 6a. group the max distance from vf databy animals and sum


Summary_animal_Max_dist_filter2_40m_av <- Max_dist_filter2_40m_av %>%
  group_by(animal_ID, max_dist_filter) %>%
  summarise(
    sum_time = sum(sum_time),
    n = n())

Summary_animal_Max_dist_filter2_40m_av 

#just 2 and 5 
filter(Summary_animal_Max_dist_filter2_40m_av, max_dist_filter == 2 | max_dist_filter ==5 | max_dist_filter ==10) %>% 
ggplot( aes(x = animal_ID, y = sum_time))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  ylim(0, 800)+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Sum of event time per animal",
       #subtitle = "Events for animal and days retained and summed
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animal",
       y = "sum of event time (seconds)")

ggsave(path= graph_path, filename = "sum_time_animal_2_5_10m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")

#just 20 and 30 40 

filter(Summary_animal_Max_dist_filter2_40m_av, max_dist_filter == 20 | max_dist_filter ==30 | max_dist_filter ==40) %>% 
  ggplot( aes(x = animal_ID, y = sum_time))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  ylim(0, 400)+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Sum of event time per animal",
       #subtitle = "Events for animal and days retained and summed
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animal",
       y = "sum of event time (seconds)")

ggsave(path= graph_path, filename = "sum_time_animal_20_30_40m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")


filter(Summary_animal_Max_dist_filter2_40m_av, max_dist_filter == 2 | max_dist_filter ==5 | max_dist_filter ==10) %>%
ggplot( aes(x = animal_ID, y = n))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Number of events per animal",
       #subtitle = "Events for animal and days retained and counted
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animal",
       y = "count of event ")

ggsave(path= graph_path, filename = "count_time_animal_2_5_10m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")

filter(Summary_animal_Max_dist_filter2_40m_av, max_dist_filter == 20 | max_dist_filter ==30 | max_dist_filter ==40) %>%
  ggplot( aes(x = animal_ID, y = n))+
  #geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Number of events per animal",
       #subtitle = "Events for animal and days retained and counted
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "animal",
       y = "count of event ")

ggsave(path= graph_path, filename = "count_time_animal_20_30_40m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")



########################################################
### 6a. group the max distance from vf data by DAY and sum


Summary_Max_dist_filter2_40m_av <- Max_dist_filter2_40m_av %>%
  group_by(day_since_start, max_dist_filter) %>%
  summarise(
    sum_time = sum(sum_time),
    n = n())

Summary_Max_dist_filter2_40m_av 


filter(Summary_Max_dist_filter2_40m_av, max_dist_filter == 2 | max_dist_filter ==5 | max_dist_filter ==10) %>%
ggplot( aes(x = day_since_start, y = sum_time))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Sum of event time per day",
       #subtitle = "Events for animal and days retained and summed
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "sum of event time (seconds)")

ggsave(path= graph_path, filename = "sum_time_day_2_5_10m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")

filter(Summary_Max_dist_filter2_40m_av, max_dist_filter == 20 | max_dist_filter ==30 | max_dist_filter ==40) %>%
  ggplot( aes(x = day_since_start, y = sum_time))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Sum of event time per day",
       #subtitle = "Events for animal and days retained and summed
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "sum of event time (seconds)")

ggsave(path= graph_path, filename = "sum_time_day_20_30_40m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")







filter(Summary_Max_dist_filter2_40m_av, max_dist_filter == 2 | max_dist_filter ==5 | max_dist_filter ==10) %>%
ggplot( aes(x = day_since_start, y = n))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Number of events per day",
       #subtitle = "Events for animal and days retained and counted
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "count of event ")
ggsave(path= graph_path, filename = "count_time_day_2_5_10m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")



filter(Summary_Max_dist_filter2_40m_av, max_dist_filter == 20 | max_dist_filter ==30 | max_dist_filter ==40) %>%
  ggplot( aes(x = day_since_start, y = n))+
  geom_vline(xintercept= c(1,4,9,15), colour= "blue", alpha = 0.2) +
  geom_col()+
  facet_wrap(.~ max_dist_filter)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90,hjust=1),
        plot.subtitle= element_text(size = 8))+
  labs(title= "Number of events per day",
       #subtitle = "Events for animal and days retained and counted
       #for events with fixed distances greater than 2meters to 40meters",
       caption = "Events are defined a period of time the animal moved into the non grazing zone",
       "number of events",
       x= "day of trial",
       y = "count of event ")
ggsave(path= graph_path, filename = "count_time_day_20_30_40m_sec.png", device = "png" ,
       width = 20, height = 12, units = "cm")


##################### END ###########################################################
  




















