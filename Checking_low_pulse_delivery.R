
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
library(parallel)

graph_path <- file.path("W:", "VF", "Eden_Valley", "temp_graphs")


Fence1_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence1_data_clean.rds", refhook = NULL)
Fence2_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence2_data_clean.rds", refhook = NULL)
Fence3_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence3_data_clean.rds", refhook = NULL)
Fence4_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence4_data_clean.rds", refhook = NULL)
Fence5_Incl_animalID <- readRDS("W:/VF/Eden_valley/logged_VF_data/Jax_Dec_2019_processing/Fence5_data_clean.rds", refhook = NULL)


### Dana said that some animal may be receieving a weak signal and that we should check this
### looking for a low value or entry that says low.... I think I have removed the text???


unique(Fence1_Incl_animalID$event)# we have nothing that says low in event clm
unique(Fence1_Incl_animalID$value)


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

Fence1_cue_data_pulse <- filter(Fence1_cue_data,
                                  event == "PulseResult" |
                                  event == "Pulse started" |
                                  event == "Pulse ceased" |
                                  event == "Pulse started [simulated]")

Fence2_cue_data_pulse <- filter(Fence2_cue_data,
                                event == "PulseResult" |
                                  event == "Pulse started" |
                                  event == "Pulse ceased" |
                                  event == "Pulse started [simulated]")
Fence3_cue_data_pulse <- filter(Fence3_cue_data,
                                event == "PulseResult" |
                                  event == "Pulse started" |
                                  event == "Pulse ceased" |
                                  event == "Pulse started [simulated]")
Fence4_cue_data_pulse <- filter(Fence4_cue_data,
                                event == "PulseResult" |
                                  event == "Pulse started" |
                                  event == "Pulse ceased" |
                                  event == "Pulse started [simulated]")
Fence5_cue_data_pulse <- filter(Fence5_cue_data,
                                event == "PulseResult" |
                                  event == "Pulse started" |
                                  event == "Pulse ceased" |
                                  event == "Pulse started [simulated]")

#Dana wants to plot the pulse started but this has no value associated with it 
#I need to move the value reading from PulseResult row to the Pulse started row.


# Fence1_cue_data_pulse_fill <- mutate(Fence1_cue_data_pulse, 
#                                      value2 = value)
# Fence1_cue_data_pulse_fill <- fill(Fence1_cue_data_pulse_fill, value2, .direction = "up")
# #reordring my clms so I can see what I have done.
# Fence1_cue_data_pulse_fill <- Fence1_cue_data_pulse_fill %>%
#   select(value2, everything())


Fence1_cue_data_pulse_fill <- fill(Fence1_cue_data_pulse, value, .direction = "up")
Fence1_cue_data_pulse_fill_start <- filter(Fence1_cue_data_pulse_fill,
                                           event == "Pulse started")
Fence2_cue_data_pulse_fill <- fill(Fence2_cue_data_pulse, value, .direction = "up")
Fence2_cue_data_pulse_fill_start <- filter(Fence2_cue_data_pulse_fill,
                                           event == "Pulse started")
Fence3_cue_data_pulse_fill <- fill(Fence3_cue_data_pulse, value, .direction = "up")
Fence3_cue_data_pulse_fill_start <- filter(Fence3_cue_data_pulse_fill,
                                           event == "Pulse started")
Fence4_cue_data_pulse_fill <- fill(Fence4_cue_data_pulse, value, .direction = "up")
Fence4_cue_data_pulse_fill_start <- filter(Fence4_cue_data_pulse_fill,
                                           event == "Pulse started")
Fence5_cue_data_pulse_fill <- fill(Fence5_cue_data_pulse, value, .direction = "up")
Fence5_cue_data_pulse_fill_start <- filter(Fence5_cue_data_pulse_fill,
                                           event == "Pulse started")

str(Fence1_cue_data_pulse_fill_start)
#now I want to plot these values as a distribution???
pulse_started_values_fence1 <- ggplot(Fence1_cue_data_pulse_fill_start, aes(animal_ID, as.double(value)))+
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 1",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))
pulse_started_values_fence1
-------------------------------------------------------------------------------------------------------------
#Just the problem cows Q26, 29 36
unique(Fence1_cue_data_pulse_fill_start$animal_ID)

pulse_started_values_fence1_problem_cows <- filter(Fence1_cue_data_pulse_fill_start, animal_ID == "Q26" |
                                                     animal_ID == "Q29" |
                                                     animal_ID == "Q36" ) %>% 
  ggplot( aes(animal_ID, as.double(value)))+
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 1",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))

pulse_started_values_fence1_problem_cows
ggsave(path= graph_path, filename = "pulse_started_values_fence1_problem_cows.png", device = "png", 
       width = 21, height = 15, units = "cm")


-------------------------------------------------------------------------------------------------------------
pulse_started_values_fence2 <-   ggplot(Fence2_cue_data_pulse_fill_start, aes(animal_ID, as.double(value)))+
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 2",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))

pulse_started_values_fence2_problem_cows <-  filter(Fence2_cue_data_pulse_fill_start, animal_ID == "Q26" |
                                                         animal_ID == "Q29" |
                                                         animal_ID == "Q36" ) %>% 
  ggplot( aes(animal_ID, as.double(value)))+ 
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 2",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))
pulse_started_values_fence2_problem_cows
ggsave(path= graph_path, filename = "pulse_started_values_fence2_problem_cows.png", device = "png", 
       width = 21, height = 15, units = "cm")

------------------------------------------------------------------------------------------------------------

Fence3_cue_data_pulse_fill_start  <- filter(Fence3_cue_data_pulse_fill_start , animal_ID != "NA")
unique(Fence3_cue_data_pulse_fill_start$animal_ID)

pulse_started_values_fence3 <- ggplot( Fence3_cue_data_pulse_fill_start, aes(animal_ID, as.double(value)))+
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 3",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))

pulse_started_values_fence3_problem_cows <-  filter(Fence3_cue_data_pulse_fill_start, animal_ID == "Q26" |
                                                      animal_ID == "Q29" |
                                                      animal_ID == "Q36" ) %>% 
  ggplot( aes(animal_ID, as.double(value)))+ 
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 3",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))
pulse_started_values_fence3_problem_cows

ggsave(path= graph_path, filename = "pulse_started_values_fence3_problem_cows.png", device = "png", 
       width = 21, height = 15, units = "cm")
-----------------------------------------------------------------------------------------------------------
pulse_started_values_fence4 <- ggplot( Fence4_cue_data_pulse_fill_start, aes(animal_ID, as.double(value)))+
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 4",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))

pulse_started_values_fence4_problem_cows <-  filter(Fence4_cue_data_pulse_fill_start, animal_ID == "Q26" |
                                                      animal_ID == "Q29" |
                                                      animal_ID == "Q36" ) %>% 
  ggplot( aes(animal_ID, as.double(value)))+ 
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 4",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))
pulse_started_values_fence4_problem_cows

ggsave(path= graph_path, filename = "pulse_started_values_fence4_problem_cows.png", device = "png", 
       width = 21, height = 15, units = "cm")

----------------------------------------------------------------------------------------------------------
Fence5_cue_data_pulse_fill_start  <- filter(Fence5_cue_data_pulse_fill_start , animal_ID != "NA")
unique(Fence5_cue_data_pulse_fill_start$animal_ID)


pulse_started_values_fence5 <- ggplot( Fence5_cue_data_pulse_fill_start, aes(animal_ID, as.double(value)))+
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 5",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))

pulse_started_values_fence5_problem_cows <-  filter(Fence5_cue_data_pulse_fill_start, animal_ID == "Q26" |
                                                      animal_ID == "Q29" |
                                                      animal_ID == "Q36" ) %>% 
  ggplot( aes(animal_ID, as.double(value)))+ 
  geom_point()+
  facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence 5",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45, size = 6))
pulse_started_values_fence5_problem_cows

getwd()

ggsave(path= graph_path, filename = "pulse_started_values_fence5_problem_cows.png", device = "png", 
       width = 21, height = 15, units = "cm")
--------------------------------------------------------------------------------------------------------------
pulse_started_values_fence1
pulse_started_values_fence2
pulse_started_values_fence3
pulse_started_values_fence4
pulse_started_values_fence5

graph_path <- file.path("W:", "VF", "Eden_Valley", "temp_graphs")
pulse_started_values_fence5
ggsave(path= graph_path, filename = "pulse_started_values_fence5.png", device = "png", 
       width = 21, height = 15, units = "cm")


### just look at day 36
str(Fence5_cue_data_pulse_fill_start)
Fence5_cue_data_pulse_fill_start_day36_animal36  <- filter(Fence5_cue_data_pulse_fill_start , animal_ID != "NA" &
                                                    day_since_start == 36 &
                                                    animal_ID == "Q36")
unique(Fence5_cue_data_pulse_fill_start_day36_animal36$animal_ID)
unique(Fence5_cue_data_pulse_fill_start_day36_animal36$day_since_start)
unique(Fence5_cue_data_pulse_fill_start_day36_animal36$collar_ID)
unique(Fence5_cue_data_pulse_fill_start_day36_animal36$date)

check <- filter(Fence5_cue_data_pulse_fill_start_day36_animal36, collar_ID == "ad2643")
unique(check$collar_ID)
check2 <- filter(Fence5_cue_data_pulse_fill_start_day36_animal36, collar_ID == "ad2640")



Fence5_cue_data_pulse_fill_start_day36  <- filter(Fence5_cue_data_pulse_fill_start , animal_ID != "NA" &
                                                             day_since_start == 36 )

str(Fence5_cue_data_pulse_fill_start_day36)

ggplot( Fence5_cue_data_pulse_fill_start_day36, aes(animal_ID, as.double(value)))+
  geom_point()+
  #facet_wrap(.~ day_since_start)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size =.5)+
  labs(title="Pulse started - fence x",
       x ="animal ID", 
       y = "value of 'pulse result'")+
  theme(axis.text.x = element_text(angle = 45))
