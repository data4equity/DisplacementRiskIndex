library(pacman)
p_load(sf, tidyverse)

source("scripts/02_GEOS.R")

Parks_0711<-st_read("data_raw/Parks/Atlanta_BeltLine_Trails_Parks_07-11.shp")
Parks_0711 <- Parks_0711 %>% mutate(park_cat = 
                                      case_when(
                                        Year_ %in% c(2008, 2010) ~ "Completed"))


# ggplot()+
#   geom_sf(data=bg_limits, fill=NA)+
#   geom_sf(data=Parks_0711 %>% filter(!is.na(Year_)), aes(color = park_cat), fill=NA)+theme_minimal()


Parks_1219<-st_read("data_raw/Parks/Atlanta_BeltLine_Trails_Parks_12-19.shp")

Parks_1219 <- Parks_1219 %>% mutate(park_cat = 
                                      case_when(
                                        Year_ == 2012 | Year == 2013 | ABI_NAME == "Allene Urban Farm
" ~ "Completed",
                                    Year_ %in% c(2017, 2019) ~ "Slated (2017-2019)"))

# ggplot()+
#   geom_sf(data=bg_limits, fill=NA)+
#   geom_sf(data=Parks_1219 %>% filter(!is.na(park_cat)), aes(color = park_cat), fill=NA)+
#   theme_minimal()





# ABI<-st_read("data_raw/Parks/ABI_Greenspace_Distribute.shp")
# 
# Access<-st_read("data_raw/Parks/Access_Points.shp")
# ggplot()+geom_sf(data = bg_limits, fill = NA)+geom_sf(data=Access)+theme_minimal()
# 
# #Corridor<-st_read("data_raw/Parks/Atlanta_BeltLine_Corridor.shp")
# #ggplot()+geom_sf(data = bg_limits, fill = NA)+geom_sf(data=Corridor)+theme_minimal()
# 
# Parks<-st_read("data_raw/Parks/Atlanta_BeltLine_Parks.shp")
# Parks<-Parks %>% st_transform(st_crs(bg_limits))
# ggplot()+geom_sf(data=Parks)+theme_minimal()
# 
# Trails<-st_read("data_raw/Parks/Atlanta_BeltLine_Trails.shp")
# Trails<-Trails %>% st_transform(st_crs(bg_limits))
# ggplot()+geom_sf(data = bg_limits, fill = NA)+geom_sf(data=Trails, colour = "red")
# 
# BPA_Sub<-st_read("data_raw/Parks/BPA_Subareas.shp")
# 
# # Create a flag for tracts with BeltLine Greenspace Amenities
# 
# greenspace<-bg_limits
# parks_clip<-st_intersection(bg_limits, Parks)
# #ggplot()+geom_sf(data=parks_clip)
# trails_clip<-st_intersection(bg_limits, Trails)
# #ggplot()+geom_sf(data=trails_clip)
# access_clip<-st_intersection(bg_limits, Access)
# #ggplot()+geom_sf(data=access_clip)
# 
# parks_test<- parks_clip %>% st_set_geometry(NULL) %>% select(GEOID) %>% distinct()
# trails_test<-trails_clip %>% st_set_geometry(NULL) %>% select(GEOID) %>% distinct() 
# access_test<-access_clip %>% st_set_geometry(NULL) %>% select(GEOID) %>% distinct()
# 
# greenspace<- bind_rows(parks_test, trails_test, access_test) %>% distinct()
# greenspace<-greenspace %>% mutate(greenspace = 1)
# greenspace<-left_join(bg_limits, greenspace, by="GEOID")
# 
# ggplot()+
#   geom_sf(data=bg_limits %>% st_union(), fill = NA)+
#   geom_sf(data=BPA_Sub %>% st_union(), fill=NA, color = "blue", size=.75)+
#   geom_sf(data=ABI, aes(fill = Proj_Type))+
#   theme_minimal()
# 
# ggplot()+
#   geom_sf(data=bg_limits, fill = NA)+
#   geom_sf(data=greenspace %>% filter(greenspace == 1), fill = "gray60", show.legend = FALSE)+
#   geom_sf(data=BPA_Sub %>% st_union(), fill=NA, color = "blue", size=1.25)+
#   geom_sf(data=Trails, colour = "red", size = 1.25)+
#   theme_minimal()
# 
# rm(Access, access_clip, access_test, bg_limits, BPA_Sub, Corridor, Parks, parks_clip, parks_test, test, Trails, trails_clip, trails_test)
