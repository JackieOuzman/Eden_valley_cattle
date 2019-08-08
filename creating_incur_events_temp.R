##################################################################################################################################



## The aim here is create a clm and then maybe a subset of data for an incursion event
# this is when the animal has moved into the exclusion zone.

Check_df_VF_week1_2_3_InclusionBord_c_animalID 
# this is all animal ID data for first 3 weeks, 
#clipped to paddock boundar and then distance values of -500 to 500 retained.
summary(Check_df_VF_week1_2_3_InclusionBord_c_animalID)
head(Check_df_VF_week1_2_3_InclusionBord_c_animalID)
#create a subset of data to play with - just one animal
animal_IDQ46 <- Check_df_VF_week1_2_3_InclusionBord_c_animalID %>% 
  filter(animal_ID == "Q46")
head(animal_IDQ46)

####################################################################################################################################
################                          TRy something a bit different                 ############################################
####################################################################################################################################
#I want to create a clm called start this will have value in the row > 0 and value in the previous row <0
#I want to create a clm called end this will have value in the row > 0 and value in the previous row >0
#then I will do occurance search on start and end 


# this first bit looks for values indicating cattle have moved in to exclusion zone
# then checks if the value before was negative was the animal in the grazing zone
animal_IDQ46_3 <- mutate(animal_IDQ46,
                         start_end = case_when(
                           value >=0  & lag(value <=0) ~ "start", 
                           value <=0  & lag(value >=0) ~ "end"),
                        start_end_no_fill = case_when(
                          value >=0  & lag(value <=0) ~ "start", 
                          value <=0  & lag(value >=0) ~ "end"))
table(animal_IDQ46_3$start_end) 
table(animal_IDQ46_3$start_end_no_fill)

#subset data to work out fill function all good
temp <- animal_IDQ46_3[1905:3000,]

               
temp1 <- fill(temp, start_end, .direction = "down")
head(temp1)

table(temp1$start_end_no_fill)
table(temp1$start_end)


#need to code my na as other I need as na for the fill function above
temp1$start_end <- replace_na(temp1$start_end, "temp")

temp1 <- mutate(temp1,
                 event = case_when(
                   start_end == "start" ~ "exclusion_zone",
                   start_end == "temp" ~ "grazing_zone",
                   start_end == "end" ~ "grazing_zone"
                   
                 ))
table(temp1$event)

#################################################################################################################################
##############    Task for this work.....
##############     assign a value for each exlusion zone event
##############    work out how to do this for larger data set and filter for animal ID
#############      summary stats on the events


##########################################################################################################################
################              Create a new clm caled event number             ###################################
##########################################################################################################################
#this makes a new df with new clm called Index that index the start and end values using no fill clm
str(temp1)


temp1 <-  group_by(temp1, start_end_no_fill) %>% 
           mutate(Index=1:n())  
           

temp1 <- mutate(temp1,
                 index_start = case_when(
                   start_end_no_fill == "start" ~ as.character(Index)))
temp1 <- data.frame(ungroup(temp1) )
temp1 <- fill(temp1, index_start, .direction = "down")


temp1 <- mutate(temp1,
                         event_number = case_when(
                           start_end == "temp"  ~ "-999", 
                           start_end ==  "end"  ~ "-999",
                           start_end ==  "start"  ~ index_start))
#### remove the working out clms
temp1 <- select(temp1, -index_start, -Index, -start_end_no_fill, -start_end)

temp1$event_number <- na_if(temp1$event_number, "-999")


##############################################################################################################################################
#####################         Happy with this ! #############################################################################################
#############################################################################################################################################
######    summary stats ###################################################################################################################


#what is the max distance in an event

#this should be ok to have per animal??
dist_event <- group_by(temp1,event_number) %>% 
    summarise(max_dist = max(value), 
              mean_dis = mean(value))
print(dist_event)

#what is the start and end time for the event

time_event <- group_by(temp1,event_number) %>% 
  summarise(max_time = max(as_datetime(time, tz="GMT")), 
            min_time = min(as_datetime(time, tz="GMT")),
            time_in_exlusion_zone = max_time - min_time)

print(time_event)
