library(sf)
library(tidyverse)

create_school<-function(name, ds){
  schools<-st_read(paste0("data_raw/School Poverty/School_Poverty_", ds, ".shp")) %>% filter(Latitude != 0)
  st_crs(schools)<-st_crs(bg_limits)
  schools<-st_filter(schools, bg_limits)
  
  test<-st_nearest_feature(bg_cent, schools) %>% as.data.frame()
  names(test)[1]<-"index"
  test$GEOID<-bg_cent$GEOID
  test$Eligible_F<-schools$Eligible_F[test$index]
  test$Eligible_R<-schools$Eligible_R[test$index]
  test$Total_Stud<-schools$Total_Stud[test$index]
  test<-test %>% mutate(Eligible_FR = (Eligible_F+Eligible_R)/Total_Stud, Eligible_F = Eligible_F/Total_Stud, Eligible_R = Eligible_R/Total_Stud)
  test<-test %>% select(GEOID, Eligible_F, Eligible_R, Eligible_FR)
  
  test2<-left_join(bg_limits, test, by="GEOID")
  test2<-test2 %>% st_set_geometry(NULL)
  assign(name, test2, envir = .GlobalEnv)
  rm(schools, test, test2) 
}

bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)
bg_cent<-st_centroid(bg_limits)

# Construct Schools Data

create_school(name = "schools_00", ds = "00-01")

create_school(name = "schools_08", ds = "07-08")
create_school(name = "schools_09", ds = "08-09")
create_school(name = "schools_10", ds = "09-10")
create_school(name = "schools_11", ds = "10-11")

create_school(name = "schools_13", ds = "12-13")
create_school(name = "schools_14", ds = "13-14")
create_school(name = "schools_15", ds = "14-15")
create_school(name = "schools_16", ds = "15-16")

#Create Averaged Values

GEOID <- bg_limits %>% st_set_geometry(NULL)

# Construct Averaged Data Values (2007-2011)
Eligible_F<-left_join(GEOID, schools_08 %>% select(GEOID, Eligible_F08 = Eligible_F))
Eligible_F<-left_join(Eligible_F, schools_09 %>% select(GEOID, Eligible_F09 = Eligible_F))
Eligible_F<-left_join(Eligible_F, schools_10 %>% select(GEOID, Eligible_F10 = Eligible_F))
Eligible_F<-left_join(Eligible_F, schools_11 %>% select(GEOID, Eligible_F11 = Eligible_F))
Eligible_F<-Eligible_F %>% rowwise() %>% mutate(Eligible_F = mean(c(Eligible_F08,Eligible_F09,Eligible_F10,Eligible_F11), na.rm=TRUE)) %>% select(GEOID, Eligible_F)

Eligible_R<-left_join(GEOID, schools_08 %>% select(GEOID, Eligible_R08 = Eligible_R))
Eligible_R<-left_join(Eligible_R, schools_09 %>% select(GEOID, Eligible_R09 = Eligible_R))
Eligible_R<-left_join(Eligible_R, schools_10 %>% select(GEOID, Eligible_R10 = Eligible_R))
Eligible_R<-left_join(Eligible_R, schools_11 %>% select(GEOID, Eligible_R11 = Eligible_R))
Eligible_R<-Eligible_R %>% rowwise() %>% mutate(Eligible_R = mean(c(Eligible_R08,Eligible_R09,Eligible_R10,Eligible_R11), na.rm=TRUE)) %>% select(GEOID, Eligible_R)

Eligible_FR<-left_join(GEOID, schools_08 %>% select(GEOID, Eligible_FR08 = Eligible_FR))
Eligible_FR<-left_join(Eligible_FR, schools_09 %>% select(GEOID, Eligible_FR09 = Eligible_FR))
Eligible_FR<-left_join(Eligible_FR, schools_10 %>% select(GEOID, Eligible_FR10 = Eligible_FR))
Eligible_FR<-left_join(Eligible_FR, schools_11 %>% select(GEOID, Eligible_FR11 = Eligible_FR))
Eligible_FR<-Eligible_FR %>% rowwise() %>% mutate(Eligible_FR = mean(c(Eligible_FR08,Eligible_FR09,Eligible_FR10,Eligible_FR11), na.rm=TRUE)) %>% select(GEOID, Eligible_FR)

schools_11<-left_join(Eligible_F, Eligible_R)
schools_11<-left_join(schools_11, Eligible_FR)
rm(schools_08, schools_09, schools_10, Eligible_F, Eligible_R, Eligible_FR)

# Construct Averaged Data Values (2012-2016)
Eligible_F<-left_join(GEOID, schools_13 %>% select(GEOID, Eligible_F13 = Eligible_F))
Eligible_F<-left_join(Eligible_F, schools_14 %>% select(GEOID, Eligible_F14 = Eligible_F))
Eligible_F<-left_join(Eligible_F, schools_15 %>% select(GEOID, Eligible_F15 = Eligible_F))
Eligible_F<-left_join(Eligible_F, schools_16 %>% select(GEOID, Eligible_F16 = Eligible_F))
Eligible_F<-Eligible_F %>% rowwise() %>% mutate(Eligible_F = mean(c(Eligible_F13,Eligible_F14,Eligible_F15, Eligible_F16), na.rm=TRUE)) %>% select(GEOID, Eligible_F)

Eligible_R<-left_join(GEOID, schools_13 %>% select(GEOID, Eligible_R13 = Eligible_R))
Eligible_R<-left_join(Eligible_R, schools_14 %>% select(GEOID, Eligible_R14 = Eligible_R))
Eligible_R<-left_join(Eligible_R, schools_15 %>% select(GEOID, Eligible_R15 = Eligible_R))
Eligible_R<-left_join(Eligible_R, schools_16 %>% select(GEOID, Eligible_R16 = Eligible_R))
Eligible_R<-Eligible_R %>% rowwise() %>% mutate(Eligible_R = mean(c(Eligible_R13,Eligible_R14,Eligible_R15, Eligible_R16), na.rm=TRUE)) %>% select(GEOID, Eligible_R)

Eligible_FR<-left_join(GEOID, schools_13 %>% select(GEOID, Eligible_FR13 = Eligible_FR))
Eligible_FR<-left_join(Eligible_FR, schools_14 %>% select(GEOID, Eligible_FR14 = Eligible_FR))
Eligible_FR<-left_join(Eligible_FR, schools_15 %>% select(GEOID, Eligible_FR15 = Eligible_FR))
Eligible_FR<-left_join(Eligible_FR, schools_16 %>% select(GEOID, Eligible_FR16 = Eligible_FR))
Eligible_FR<-Eligible_FR %>% rowwise() %>% mutate(Eligible_FR = mean(c(Eligible_FR13,Eligible_FR14,Eligible_FR15, Eligible_FR16), na.rm=TRUE)) %>% select(GEOID, Eligible_FR)

schools_16<-left_join(Eligible_F, Eligible_R)
schools_16<-left_join(schools_16, Eligible_FR)
rm(schools_13, schools_14, schools_15, Eligible_F, Eligible_R, Eligible_FR)
rm(bg_cent, bg_limits, GEOID)
rm(create_school)
