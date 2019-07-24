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







head(df_VF_week1_2_3_InclusionBord_c_test,3)
table(df_VF_week1_2_3_InclusionBord_c_test$animal_ID)


mutate(VF_week1_2_3_InclusionBord,
       chuck = case_when(
         collar_ID == "ad2042" & 
           value > 3000  ~ 1,
         TRUE ~ chuck))

VF_week1_2_3_InclusionBord_clip_20 <- filter(VF_week1_2_3_InclusionBord_clip, 
                                                   between(time, as_datetime('2019-05-20 12:00:00', tz="GMT"),
                                                           as_datetime('2019-05-20 23:55:00', tz="GMT"))) 