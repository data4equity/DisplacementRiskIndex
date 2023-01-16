library(sf)
library(ggplot2)
library(tidyverse)
library(readxl)

#limits<-st_read("data_raw/Geography/Atlanta_City_Limits.shp")
#ggplot()+geom_sf(data=limits)

bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp")
#ggplot()+geom_sf(data=bg_limits)+geom_sf(data=limits, color="blue", fill=NA)
bg_limits<-bg_limits %>% select(GEOID)

bg_belt<-read_xlsx("Older/Census Data/2012-2016/Transform_Z-score_Below_Poverty_12-16.xlsx") %>% 
  select(GEOID = FIPS_BLKGRP_GEOID) %>% mutate(GEOID = as.character(GEOID), belt_flag = "Beltline")

bg_limits <-left_join(bg_limits, bg_belt, by="GEOID") %>% mutate(belt_flag = case_when(belt_flag == "Beltline" ~ "Beltline",
                                                                                       is.na(belt_flag) ~ "Not Beltline"))
rm(bg_belt)
