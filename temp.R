#messing baout
#read in data as csv
head(VF_week1,3)
#remove the na
VF_week1 <- VF_week1 %>% filter(!is.na(lat) | !is.na(lon))
#do the projection 
mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

####################  convert lat and longs to x and Y     ##########################################
coordinates(VF_week1) <- ~ lon + lat
proj4string(VF_week1) <- wgs84CRS   # assume input lat and longs are WGS84
#make new object_1
VF_week1_1 <- spTransform(VF_week1, mapCRS)
#make new df_1
head(VF_week1_1,3)
VF_week1_2 = as.data.frame(VF_week1_1) #this has the new coordinates projected !YES!!
#make new df with point x and point y
VF_week1_2 <- mutate(VF_week1_2,POINT_X = lon,  POINT_Y = lat )
head(VF_week1_2,3)


#### Not sure if this function is the one i want??
VF_week1_2_spatial <- st_as_sf(VF_week1_2, coords = c("POINT_X", "POINT_Y"), crs = mapCRS)
head(VF_week1_2_spatial, 3)

## clip function
clip1 <- st_intersection(VF_week1_2_spatial, eden_valley)
head(clip1,3)
clip1 = as.data.frame(clip1) 
#make new df with point x and point y
clip1 <- mutate(clip1,POINT_X = lon,  POINT_Y = lat )
#########################################################################################################

#Create a new column with the animal_number


head(df_VF_week1_2_3_InclusionBord_c,3)



table(df_VF_week1_2_3_InclusionBord_c$collar_ID)
min(as_datetime(df_VF_week1_2_3_InclusionBord_c$time, tz="GMT")) #should be 12:30
max(as_datetime(df_VF_week1_2_3_InclusionBord_c$time, tz="GMT")) 


test <- filter(df_VF_week1_2_3_InclusionBord_c,collar_ID == "ac220" ) 
min(as_datetime(test$time, tz="GMT")) 
max(as_datetime(test$time, tz="GMT"))

df_VF_week1_2_3_InclusionBord_c_test <- mutate(df_VF_week1_2_3_InclusionBord_c,
                                             animal_ID = case_when(
                                               collar_ID == "ac138" ~ "Q46",
                                               collar_ID == "ac187" ~ "Q36",
                                               collar_ID == "ac204"  &
                                                 between(time, as_datetime('2019-05-20 12:30:00', tz="GMT"),
                                                         as_datetime('2019-05-30 13:26:00', tz="GMT")) ~ "Q108",
                                               #collar_ID == "xxxx"  &
                                               #  between(time, as_datetime('2019-xx-xx xx:xx:00', tz="GMT"),
                                               #          as_datetime('2019-xx-xx xx:xx:00', tz="GMT")) ~ "Q108",
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







ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = fence2, color = "grey") +
  geom_point(data = Inclusion_20_05_2019, aes(POINT_X, POINT_Y,colour = collar), inherit.aes = FALSE, alpha = 0.01) +
  facet_wrap(.~collar)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  labs(title= "Fence 1 and 2 with data from 2019-05-20")
       
ggsave(filename = "x_yInclusion_Dist20_05.png", device = "png" ,dpi=600)





head(Inclusion_20_05_2019 ,3)





p4a <- ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = fence2, color = "grey") +
  geom_point(data = Inclusion_20_05_2019, aes(POINT_X, POINT_Y,colour = collar), inherit.aes = FALSE) +
  #facet_wrap(.~collar)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


p4a  

p4b <- p4a +
  labs( title =   'Date:  {format(as_datetime(frame_time, "%b %e"), tz="GMT")}',
        #subtitle = 'Hour: {format(as_datetime(frame_time, "%H"), tz="GMT")}',
        caption = "Frame {frame} of {nframes} ({progress * 100}%)") +
  transition_time(time) +
  shadow_wake(0.3)

animation_20th <- animate(p4b, duration = 30) 
animation_20th

anim_save(animation = animation_20th , filename = "animation_20th.gif")



##### DAy 2

p4a <- ggplot() +
  geom_sf(data = eden_valley, color = "black", fill = NA) +
  geom_sf(data = fence1, color = "grey") +
  geom_sf(data = fence2, color = "grey") +
  geom_point(data = Inclusion_21_05_2019, aes(POINT_X, POINT_Y,colour = collar), inherit.aes = FALSE) +
  #facet_wrap(.~collar)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


p4a  

p4b <- p4a +
  labs( title =   'Date:  {format(as_datetime(frame_time, "%b %e"), tz="GMT")}',
        #subtitle = 'Hour: {format(as_datetime(frame_time, "%H"), tz="GMT")}',
        caption = "Frame {frame} of {nframes} ({progress * 100}%)") +
  transition_time(time) +
  shadow_wake(0.3)

animation_21th <- animate(p4b, duration = 60) 
animation_21th

anim_save(animation = animation_21th , filename = "animation_21th.gif")





##################################################################################################################################

Check_df_VF_week1_2_3_InclusionBord_c_animalID
summary(Check_df_VF_week1_2_3_InclusionBord_c_animalID)

Inclusion_week1_almost_positive <- Inclusion_week1 %>% 
  filter(value >= -5 )
####################################################################################################################################
################                          TRy something a bit different                 ############################################
####################################################################################################################################
#I want to create a clm called start this will have value in the row > 0 and value in the previous row <0
#I want to create a clm called end this will have value in the row > 0 and value in the previous row >0
#then I will do occurance search on start and end 

Inclusion_week1_almost_positive_test1 <- mutate(Inclusion_week1_almost_positive,
                                        start = ifelse(value >0,"start", "" ))
                                        #start = lead(ifelse(value >0,"start", "" )))
summary(Inclusion_week1_almost_positive_test1$start) #37788




Inclusion_week1_almost_positive_test4 <- mutate(Inclusion_week1_almost_positive,
       start = case_when(
         value >=0  & lead(value <0) ~ "start",
         TRUE ~ "NA"))
                           
                                                
summary(Inclusion_week1_almost_positive_test4$start) #37788 its the same is it working - think so just shifted everything by one.
##################################################################################################################################




Inclusion_week1_positive1 <- group_by(Inclusion_week1_positive,
                                      day, animal_ID) %>%   
  summarise(count = n())

head(Inclusion_week1_positive1)
#change the day to factor
Inclusion_week1_positive1$day_factor <- as.factor(Inclusion_week1_positive1$day)






head(dat_vlines)

ggplot(Inclusion_week1_positive1, aes(x = day_factor, y = count))+
  geom_boxplot(alpha = 0.2)+
  geom_point()+
  theme_bw()+
  #facet_wrap(.~animal_ID)+
  geom_vline(xintercept = c(1,3),color = "blue", size=0.5)+ #this is the order which it appears on the axis can add 1.3 values too!
  theme(axis.text.x=element_text(angle=90,hjust=1))+
        #,legend.position = "none")+
  labs(title= "Week1",
       x= "Day",
       y = "Counts of logged activity inside exclusion zone")





##################################################################################################################################
#########################       create incursion events ##########################################################################

head(Inclusion_week1_positive)

Inclusion_week1_positive <- Inclusion_week1 %>% 
  filter(value >=0 )

summary(Inclusion_week1_positive)




cattle_incursion_events <- mutate(Inclusion_week1_positive,
                                  event_1 = case_when(
                                    between(value, 0, 0.5) ~ 1,
                                    TRUE ~ 2))



        
head(cattle_incursion_events)  
  
library(lubridate)
summary(cattle_incursion_events)  
 

cattle_incursion_events_20_Q46 <- filter(cattle_incursion_events, date == "2019-05-20" & animal_ID == "Q46")  
summary(cattle_incursion_events_20_Q46) 
ggplot(cattle_incursion_events_20_Q46, aes(time, value))+
  geom_point()
  
ggplot(cattle_incursion_events_20_Q46, aes(time, event_1))+
  geom_point()
  
###looks like 4 events occured on this day for this animal




##########################################################################################################################
################               This is the counting of occurances example              ###################################
##########################################################################################################################
new_test <- cattle_incursion_events_20_Q46 %>% 
  group_by(event_1) %>% 
  mutate(Index=1:n())
print(new_test)


#####################################################################################################################################
##### match function in action
