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



