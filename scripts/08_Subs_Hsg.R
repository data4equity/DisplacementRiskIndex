library(tidyverse)
library(sf)
library(ggplot2)
library(tigris)
library(ipumsr)
library(readxl)

NHPD <- read_xlsx("data_raw/NHPD/All Properties.xlsx") %>% 
  filter(CountyCode %in% c("13089", "13121"))
NHPD<-NHPD %>% filter(LatestEndDate > "2000-01-01")

Beltline <- read_xlsx(("data_raw/Beltline/Affordable Housing/Affordable Housing ABL.xlsx")) %>% 
  filter(Year %in% c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")) %>% 
  mutate(Latitude = as.numeric(as.character(Latitude)),
         Longitude = as.numeric(as.character(ifelse(ID == "7",-84.403387, Longitude))))

test<-st_as_sf(NHPD, coords = c("Longitude", "Latitude"))
test<-st_set_crs(test, 4269)

test_belt <- st_as_sf(Beltline, coords = c("Longitude", "Latitude"))
test_belt<-st_set_crs(test_belt, 4269)


bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)

AH <- st_join(bg_limits, test_belt) %>% 
  filter(Year %in% c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"))
test<-st_join(test, bg_limits)
test<-test %>% filter(!is.na(GEOID)) %>% select(GEOID, TotalUnits, EarliestStartDate, EarliestEndDate, LatestEndDate)
#ggplot()+geom_sf(data=bg_limits)+geom_sf(data=test, size = .5)

#Units in service in each of these years
subsid96<-test %>% filter(EarliestStartDate <= "1996-01-01", EarliestEndDate >= "1996-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, AHU_96 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_96 = sum(AHU_96, na.rm=TRUE))
subsid00<-test %>% filter(EarliestStartDate <= "2000-01-01", EarliestEndDate >= "2000-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_00 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_00 = sum(AHU_00, na.rm=TRUE))
subsid07<-test %>% filter(EarliestStartDate <= "2007-01-01", EarliestEndDate >= "2007-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_07 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_07 = sum(AHU_07, na.rm=TRUE))
subsid11<-test %>% filter(EarliestStartDate <= "2011-01-01", EarliestEndDate >= "2011-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_11 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_11 = sum(AHU_11, na.rm=TRUE))
subsid12<-test %>% filter(EarliestStartDate <= "2012-01-01", EarliestEndDate >= "2012-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_12 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_12 = sum(AHU_12, na.rm=TRUE))
subsid16<-test %>% filter(EarliestStartDate <= "2016-01-01", EarliestEndDate >= "2016-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_16 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_16 = sum(AHU_16, na.rm=TRUE))
subsid07_belt <- AH %>% filter(Year <= 2007) %>% st_set_geometry(NULL) %>% select(GEOID, AHU_07 = Total_Affordable_Units) %>% group_by(GEOID) %>% summarise(AHU_07 = sum(AHU_07, na.rm = TRUE))
subsid11_belt <- AH %>% filter(Year <= 2011) %>% st_set_geometry(NULL) %>% select(GEOID, AHU_11 = Total_Affordable_Units) %>%  group_by(GEOID) %>% summarise(AHU_11 = sum(AHU_11, na.rm = TRUE))
subsid12_belt <- AH %>% filter(Year <= 2012) %>% st_set_geometry(NULL) %>% select(GEOID, AHU_12 = Total_Affordable_Units) %>% group_by(GEOID) %>% summarise(AHU_12 = sum(AHU_12, na.rm = TRUE))
subsid16_belt <- AH %>% filter(Year <= 2016) %>% st_set_geometry(NULL) %>% select(GEOID, AHU_16 = Total_Affordable_Units) %>% group_by(GEOID) %>% summarise(AHU_16 = sum(AHU_16, na.rm = TRUE))

#Units set to expire (updated to constrain expiration to next five years)

expire00<-test %>% filter(EarliestStartDate <= "2000-01-01", LatestEndDate <= "2005-01-01" & LatestEndDate >="2000-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, Expire_00 = TotalUnits) %>% group_by(GEOID) %>% summarise(Expire_00 = sum(Expire_00, na.rm=TRUE))
expire11<-test %>% filter(EarliestStartDate <= "2011-01-01", LatestEndDate <= "2016-01-01"& LatestEndDate >="2011-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, Expire_11 = TotalUnits) %>% group_by(GEOID) %>% summarise(Expire_11 = sum(Expire_11, na.rm=TRUE))
expire16<-test %>% filter(EarliestStartDate <= "2016-01-01", LatestEndDate <= "2021-01-01"& LatestEndDate >="2016-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, Expire_16 = TotalUnits) %>% group_by(GEOID) %>% summarise(Expire_16 = sum(Expire_16, na.rm=TRUE))

var_list <- list(subsid96, subsid00, subsid07, subsid07_belt, subsid11, subsid11_belt, subsid12, subsid12_belt, subsid16, subsid16_belt, expire00, expire11, expire16)

AH_All<-var_list %>%  reduce(full_join, by="GEOID")

AH_All$AHU_07 <- rowSums(AH_All[, c("AHU_07.x", "AHU_07.y")], na.rm = TRUE)
AH_All$AHU_11 <- rowSums(AH_All[, c("AHU_11.x", "AHU_11.y")], na.rm = TRUE)
AH_All$AHU_12 <- rowSums(AH_All[, c("AHU_12.x", "AHU_12.y")], na.rm = TRUE)
AH_All$AHU_16 <- rowSums(AH_All[, c("AHU_16.x", "AHU_16.y")], na.rm = TRUE)

AH_All <- AH_All %>% 
  select(!c(AHU_07.x,AHU_07.y,AHU_11.x,AHU_11.y,AHU_12.x,AHU_12.y,AHU_16.x,AHU_16.y))

AH_All<-left_join(bg_limits, AH_All, by="GEOID")

AH_All<-AH_All %>% replace(is.na(.), 0)

HU_00 <- read_csv("data_raw/Geolytics/BG_Pct_Renter_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(HUs = H007001) %>% 
  select(AREAKEY, HU_00 = HUs)

HU_07_11 <-read_nhgis("data_raw/NHGIS/nhgis0016_ds184_20115_2011_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(HU_07_11 = MS2E001) %>% 
  select(AREAKEY, HU_07_11)

HU_12_16 <-read_nhgis("data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(HU_12_16 = AF7NE001) %>% 
  select(AREAKEY, HU_12_16)

HUs<-left_join(HU_00, HU_07_11)
HUs<-left_join(HUs, HU_12_16)

AH_All<-left_join(AH_All, HUs, by=c("GEOID" = "AREAKEY"))
AH_All<-AH_All %>% mutate(
  PSU_00 = AHU_00 / HU_00,
  PSU_11 = AHU_11 / HU_07_11,
  PSU_16 = AHU_16 / HU_12_16,
  CHU_00 = (AHU_00-AHU_96)/HU_00,
  CHU_11 = (AHU_11-AHU_07)/HU_07_11,
  CHU_16 = (AHU_16-AHU_12)/HU_12_16,
  P_Expire_00 = Expire_00/AHU_00,
  P_Expire_11 = Expire_11/AHU_11,
  P_Expire_16 = Expire_16/AHU_16)

AH_All<-AH_All %>% select(GEOID, PSU_00, PSU_11, PSU_16, CHU_00, CHU_11, CHU_16, P_Expire_00, P_Expire_11, P_Expire_16)

AH_00<-AH_All %>% select(GEOID, PSU = PSU_00, CHU = CHU_00, P_Expire = P_Expire_00)%>% mutate_at(vars(PSU, CHU, P_Expire), ~replace(., is.nan(.), 0))
AH_11<-AH_All %>% select(GEOID, PSU = PSU_11, CHU = CHU_11, P_Expire = P_Expire_11)%>% mutate_at(vars(PSU, CHU, P_Expire), ~replace(., is.nan(.), 0))
AH_16<-AH_All %>% select(GEOID, PSU = PSU_16, CHU = CHU_16, P_Expire = P_Expire_16)%>% mutate_at(vars(PSU, CHU, P_Expire), ~replace(., is.nan(.), 0))

AH_00<-AH_00 %>% mutate_at(vars(P_Expire), ~replace(., !is.finite(.), NA))
AH_11<-AH_11 %>% mutate(P_Expire)%>% mutate_at(vars(P_Expire), ~replace(., !is.finite(.), NA))
AH_16<-AH_16 %>% mutate(P_Expire)%>% mutate_at(vars(P_Expire), ~replace(., !is.finite(.), NA))

rm(AH_All, HU_00, HU_07_11, HU_12_16, HUs, NHPD, expire00, expire11, expire16, subsid96, subsid00, subsid07, subsid11, subsid12, subsid16, test, var_list)


  
get_crime<-function(year, ds){
  crime<-read_csv(paste0("data_raw/Crime/Crime Data_",ds,".csv"))
  crime<-crime %>% filter(!is.na(Longitude), !is.na(Latitude))
  crime<-st_as_sf(crime, coords=c("Longitude","Latitude"))
  crime<-st_set_crs(crime, 4269)
  crime<-st_join(crime, bg_limits)
  crime<-crime %>% st_set_geometry(NULL)
  crime<-crime %>% group_by(GEOID) %>% summarise(crimes = n())
  crime<-crime %>% filter(!is.na(GEOID)) %>% rename(!!sym(paste0("crime", year)) := crimes)
  assign(paste0("crime", year), crime, envir = .GlobalEnv)
  rm(crime)
}

get_crime("00", "2000_Merged")

get_crime("07", "2007_Merged")
get_crime("08", "2008_Merged")
get_crime("09", "2009_021221")
get_crime("10", "2010_21821_Lat_Long")
get_crime("11", "2011_21821_Lat_Long")

get_crime("12", "2012_21321")
get_crime("13", "2013_021321")
get_crime("14", "2014_021321")
get_crime("15", "2015_21321")
get_crime("16", "2016")




var_list <- list(crime00, crime07, crime08, crime09, crime10, crime11, crime12, crime13, crime14, crime15, crime16)

crime_all<-var_list %>%  reduce(full_join, by="GEOID")

crime_all<-left_join(bg_limits, crime_all)
crime_all<-crime_all %>% replace(is.na(.), 0)

pop_00<-read_csv("data_raw/Geolytics/BG_Pct_Black_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(pop_00 = P007001) %>% 
  select(AREAKEY, pop_00) 

pop_11<-read_nhgis("data_raw/NHGIS/nhgis0016_ds184_20115_2011_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(pop_11 = MN2E001) %>% 
  select(AREAKEY, pop_11) 

pop_16<-read_nhgis("data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(pop_16 = AF2UE001) %>% 
  select(AREAKEY, pop_16)

crime_all<-left_join(crime_all, pop_00, by=c("GEOID" = "AREAKEY"))
crime_all<-left_join(crime_all, pop_11, by=c("GEOID" = "AREAKEY"))
crime_all<-left_join(crime_all, pop_16, by=c("GEOID" = "AREAKEY"))
crime_all<-crime_all %>% st_set_geometry(NULL)
crime_all<-crime_all %>% mutate(R_Crime_00 = crime00/pop_00,
                                R_Crime_07 = crime07/pop_11,
                                R_Crime_08 = crime08/pop_11,
                                R_Crime_09 = crime09/pop_11,
                                R_Crime_10 = crime10/pop_11,
                                R_Crime_11 = crime11/pop_11,
                                R_Crime_12 = crime12/pop_16,
                                R_Crime_13 = crime13/pop_16,
                                R_Crime_14 = crime14/pop_16,
                                R_Crime_15 = crime15/pop_16,
                                R_Crime_16 = crime16/pop_16)

crime_all<-crime_all %>% rowwise() %>% summarise(GEOID = GEOID, R_Crime_00 = R_Crime_00, R_Crime_11 = mean(c(R_Crime_07, R_Crime_08, R_Crime_09, R_Crime_10, R_Crime_11), na.rm=TRUE), R_Crime_16 = mean(c(R_Crime_12, R_Crime_13, R_Crime_14, R_Crime_15, R_Crime_16), na.rm=TRUE))



crime_all<-crime_all %>%
  mutate_at(vars(R_Crime_00, R_Crime_11, R_Crime_16), ~replace(., is.nan(.), 0)) %>%  
  mutate(
    R_Crime_00 = ifelse(!is.finite(R_Crime_00), NA, R_Crime_00),
    R_Crime_11 = ifelse(!is.finite(R_Crime_11), NA, R_Crime_11),
    R_Crime_16 = ifelse(!is.finite(R_Crime_16), NA, R_Crime_16))

crime_00<-crime_all %>% select(GEOID, R_Crime = R_Crime_00)
crime_11<-crime_all %>% select(GEOID, R_Crime = R_Crime_11)
crime_16<-crime_all %>% select(GEOID, R_Crime = R_Crime_16)
rm(pop_00, pop_11, pop_16, var_list, crime_all, crime00, crime07, crime08, crime09, crime10, crime11, crime12, crime13, crime14, crime15, crime16)


evictions<-read_csv("data_raw/Evictions/Georgia Block Group Eviction Rate and Eviction Filing Rates.csv", col_types = cols(GEOID = col_character()))
evict_00<-evictions %>% filter(year == 2000) %>% select(GEOID, R_Evict00 = `eviction-rate`, R_File00 = `eviction-filing-rate`)
evict_07<-evictions %>% filter(year == 2007) %>% select(GEOID, R_Evict07 = `eviction-rate`, R_File07 = `eviction-filing-rate`)
evict_08<-evictions %>% filter(year == 2008) %>% select(GEOID, R_Evict08 = `eviction-rate`, R_File08 = `eviction-filing-rate`)
evict_09<-evictions %>% filter(year == 2009) %>% select(GEOID, R_Evict09 = `eviction-rate`, R_File09 = `eviction-filing-rate`)
evict_10<-evictions %>% filter(year == 2010) %>% select(GEOID, R_Evict10 = `eviction-rate`, R_File10 = `eviction-filing-rate`)
evict_11<-evictions %>% filter(year == 2011) %>% select(GEOID, R_Evict11 = `eviction-rate`, R_File11 = `eviction-filing-rate`)
evict_12<-evictions %>% filter(year == 2012) %>% select(GEOID, R_Evict12 = `eviction-rate`, R_File12 = `eviction-filing-rate`)
evict_13<-evictions %>% filter(year == 2013) %>% select(GEOID, R_Evict13 = `eviction-rate`, R_File13 = `eviction-filing-rate`)
evict_14<-evictions %>% filter(year == 2014) %>% select(GEOID, R_Evict14 = `eviction-rate`, R_File14 = `eviction-filing-rate`)
evict_15<-evictions %>% filter(year == 2015) %>% select(GEOID, R_Evict15 = `eviction-rate`, R_File15 = `eviction-filing-rate`)
evict_16<-evictions %>% filter(year == 2016) %>% select(GEOID, R_Evict16 = `eviction-rate`, R_File16 = `eviction-filing-rate`)


var_list <- list(evict_00, evict_07, evict_08, evict_09, evict_10, evict_11, evict_12, evict_13, evict_14, evict_15, evict_16)

evict_all<-var_list %>%  reduce(full_join, by="GEOID")
evict_all<-evict_all %>% rowwise() %>% summarise(
  GEOID = GEOID, 
  R_Evict00 = R_Evict00, 
  R_Evict11 = mean(c(R_Evict07, R_Evict08, R_Evict09, R_Evict10, R_Evict11), na.rm=TRUE), 
  R_Evict16 = mean(c(R_Evict12, R_Evict13, R_Evict14, R_Evict15, R_Evict16), na.rm=TRUE),
  R_File00 = R_File00, 
  R_File11 = mean(c(R_File07, R_File08, R_File09, R_File10, R_File11), na.rm=TRUE), 
  R_File16 = mean(c(R_File12, R_File13, R_File14, R_File15, R_File16), na.rm=TRUE))

evict_all<-evict_all %>%
  mutate_at(vars(R_Evict00, R_Evict11, R_Evict16, R_File00, R_File11, R_File16), ~replace(., is.nan(.), 0)) %>%  
  mutate(
    R_Evict00 = ifelse(!is.finite(R_Evict00), NA, R_Evict00),
    R_Evict11 = ifelse(!is.finite(R_Evict11), NA, R_Evict11),
    R_Evict16 = ifelse(!is.finite(R_Evict16), NA, R_Evict16),
    R_File00 = ifelse(!is.finite(R_File00), NA, R_File00),
    R_File11 = ifelse(!is.finite(R_File11), NA, R_File11),
    R_File16 = ifelse(!is.finite(R_File16), NA, R_File16))

evict_00 <- evict_all %>% select(GEOID, R_Evict = R_Evict00, R_File = R_File00)
evict_11 <- evict_all %>% select(GEOID, R_Evict = R_Evict11, R_File = R_File11)
evict_16 <- evict_all %>% select(GEOID, R_Evict = R_Evict16, R_File = R_File16)

rm(var_list, evictions, evict_07, evict_08, evict_09, evict_10, evict_12, evict_13, evict_14, evict_15, evict_all)

# Low Income Area Next to High-Income Area
bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)

IAF <- 1.397390273 #2000 CPI to 2016
MHHI_00 <- read_csv("data_raw/Geolytics/BG_Med_HI_2000.csv",col_types = cols(AREAKEY = col_character())) %>%
  mutate(MHHI = P053001, MHHI_IA = MHHI*IAF) %>% 
  select(AREAKEY, MHHI = MHHI_IA)

IAF <- 1.069288956 #2011 CPI to 2016
vuln_0711<-read_nhgis("data_raw/NHGIS/nhgis0016_ds184_20115_2011_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))

MHHI_07_11 <- vuln_0711 %>% 
  mutate(MHHI = MP1E001, MHHI_IA = MHHI*IAF) %>% 
  select(AREAKEY, MHHI = MHHI_IA)

vuln_1216<-read_nhgis("data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))

MHHI_12_16 <- vuln_1216 %>% 
  mutate(MHHI = AF49E001) %>% 
  select(AREAKEY, MHHI)

rm(vuln_0711, vuln_1216, IAF)

MHHI_00<-left_join(bg_limits, MHHI_00, by=c("GEOID" = "AREAKEY"))
MHHI_07_11<-left_join(bg_limits, MHHI_07_11, by=c("GEOID" = "AREAKEY"))
MHHI_12_16<-left_join(bg_limits, MHHI_12_16, by=c("GEOID" = "AREAKEY"))

MHHI_00<-MHHI_00 %>% mutate(MHHI_Q = ntile(MHHI, 5))
MHHI_07_11<-MHHI_07_11 %>% mutate(MHHI_Q = ntile(MHHI, 5))
MHHI_12_16<-MHHI_12_16 %>% mutate(MHHI_Q = ntile(MHHI, 5))

test<-spdep::poly2nb(MHHI_00, row.names = "GEOID")
#summary.nb(test)

test<-spdep::nb2mat(test, zero.policy=TRUE, style = "B")
rownames(test)<-MHHI_00$GEOID
colnames(test)<-MHHI_00$GEOID

test<-reshape2::melt(test) %>% 
  filter(value == 1, Var1 != Var2) %>% 
  arrange(Var1, Var2) %>% 
  select(Var1, Var2) %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))

test<-left_join(test, MHHI_00 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var1" = "GEOID"))
test<-left_join(test, MHHI_00 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var2" = "GEOID"))

test<-test %>% 
  mutate(LIC = ifelse(MHHI_Q.x <= 2 & MHHI_Q.y >= 4, 1, 0))
test<-test %>% mutate(LIC = replace_na(LIC, 0))
test<-test %>% 
  group_by(Var1) %>% 
  summarise(LIC = sum(LIC)) %>% 
  mutate(LIC = ifelse(LIC >= 1, 1, 0))

MHHI_00<-left_join(MHHI_00, test, by=c("GEOID" = "Var1"))
rm(test)

test<-spdep::poly2nb(MHHI_07_11, row.names = "GEOID")
#summary.nb(test)

test<-spdep::nb2mat(test, zero.policy=TRUE, style = "B")
rownames(test)<-MHHI_07_11$GEOID
colnames(test)<-MHHI_07_11$GEOID

test<-reshape2::melt(test) %>% 
  filter(value == 1, Var1 != Var2) %>% 
  arrange(Var1, Var2) %>% 
  select(Var1, Var2) %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))

test<-left_join(test, MHHI_07_11 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var1" = "GEOID"))
test<-left_join(test, MHHI_07_11 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var2" = "GEOID"))

test<-test %>% 
  mutate(LIC = ifelse(MHHI_Q.x <= 2 & MHHI_Q.y >= 4, 1, 0))
test<-test %>% mutate(LIC = replace_na(LIC, 0))
test<-test %>% 
  group_by(Var1) %>% 
  summarise(LIC = sum(LIC)) %>% 
  mutate(LIC = ifelse(LIC >= 1, 1, 0))

MHHI_07_11<-left_join(MHHI_07_11, test, by=c("GEOID" = "Var1"))
rm(test)

test<-spdep::poly2nb(MHHI_12_16, row.names = "GEOID")
#summary.nb(test)

test<-spdep::nb2mat(test, zero.policy=TRUE, style = "B")
rownames(test)<-MHHI_12_16$GEOID
colnames(test)<-MHHI_12_16$GEOID

test<-reshape2::melt(test) %>% 
  filter(value == 1, Var1 != Var2) %>% 
  arrange(Var1, Var2) %>% 
  select(Var1, Var2) %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))

test<-left_join(test, MHHI_12_16 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var1" = "GEOID"))
test<-left_join(test, MHHI_12_16 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var2" = "GEOID"))

test<-test %>% 
  mutate(LIC = ifelse(MHHI_Q.x <= 2 & MHHI_Q.y >= 4, 1, 0))

test<-test %>% mutate(LIC = replace_na(LIC, 0))

test<-test %>% 
  group_by(Var1) %>% 
  summarise(LIC = sum(LIC)) %>% 
  mutate(LIC = ifelse(LIC >= 1, 1, 0))

MHHI_12_16<-left_join(MHHI_12_16, test, by=c("GEOID" = "Var1"))
rm(test)


MHHI_00<-MHHI_00 %>% select(GEOID, LIC) %>% st_set_geometry(NULL)
MHHI_07_11<-MHHI_07_11 %>% select(GEOID, LIC) %>% st_set_geometry(NULL)
MHHI_12_16<-MHHI_12_16 %>% select(GEOID, LIC) %>% st_set_geometry(NULL)

#ggplot()+geom_sf(data=MHHI_00, aes(fill = LIC))
#ggplot()+geom_sf(data=MHHI_07_11, aes(fill = LIC))
#ggplot()+geom_sf(data=MHHI_12_16, aes(fill = LIC))