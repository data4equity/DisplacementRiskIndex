library(sf)
library(ggplot2)
library(tidyverse)
library(readxl)

#limits<-st_read("data_raw/Geography/Atlanta_City_Limits.shp")
#ggplot()+geom_sf(data=limits)

bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp")
#ggplot()+geom_sf(data=bg_limits)+geom_sf(data=limits, color="blue", fill=NA)
bg_limits<-bg_limits %>% select(GEOID)

# Flag derived per manuscript method: block-group centroid within the 10 BeltLine subareas.
# The legacy Excel list is retained only as a cross-check.
subareas <- st_read("data_raw/Beltline/subareas_shapefiles/beltline_subareas.shp", quiet = TRUE)
bg_limits_projected <- st_transform(bg_limits, 26916)
subareas_projected <- st_make_valid(st_transform(subareas, 26916))
centroids <- st_centroid(st_geometry(bg_limits_projected))
bg_limits$belt_flag <- ifelse(lengths(st_within(centroids, st_union(subareas_projected))) > 0,
                              "Beltline", "Not Beltline")

legacy_belt <- readxl::read_excel("Older/Census Data/2012-2016/Transform_Z-score_Below_Poverty_12-16.xlsx")[[1]]
legacy_belt <- sub("^15000US", "", as.character(legacy_belt))
message("Spatial and legacy BeltLine flags disagree for ",
        sum((bg_limits$belt_flag == "Beltline") != (bg_limits$GEOID %in% legacy_belt)),
        " block groups.")
