library(tidyverse)
library(ipumsr)

tenure_1721<-read_nhgis("data_raw/NHGIS/nhgis0005_ds254_20215_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))

rent_white <- tenure_1721 %>% 
  mutate(P_rent_white = AOSXE003/AOSXE001) %>% 
  dplyr::select(AREAKEY, P_rent_white)

rent_black <- tenure_1721 %>% 
  mutate(P_rent_black = AOSRE003/AOSRE001) %>% 
  dplyr::select(AREAKEY, P_rent_black)

rent_aian <- tenure_1721 %>% 
  mutate(P_rent_aian = AOSSE003/AOSSE001) %>% 
  dplyr::select(AREAKEY, P_rent_aian)

rent_hispanic <- tenure_1721 %>% 
  mutate(P_rent_hispanic = AOSYE003/AOSYE001) %>% 
  dplyr::select(AREAKEY, P_rent_hispanic)

rent_asian <- tenure_1721 %>% 
  mutate(P_rent_asian = AOSTE003/AOSTE001) %>% 
  dplyr::select(AREAKEY, P_rent_asian)

rent_nhpi <- tenure_1721 %>% 
  mutate(P_rent_nhpi = AOSUE003/AOSUE001) %>% 
  dplyr::select(AREAKEY, P_rent_nhpi)

own_white <- tenure_1721 %>% 
  mutate(P_own_white = AOSXE002/AOSXE001) %>% 
  dplyr::select(AREAKEY, P_own_white)

own_black <- tenure_1721 %>% 
  mutate(P_own_black = AOSRE002/AOSRE001) %>% 
  dplyr::select(AREAKEY, P_own_black)

own_aian <- tenure_1721 %>% 
  mutate(P_own_aian = AOSSE002/AOSSE001) %>% 
  dplyr::select(AREAKEY, P_own_aian)

own_asian <- tenure_1721 %>% 
  mutate(P_own_asian = AOSTE002/AOSTE001) %>% 
  dplyr::select(AREAKEY, P_own_asian)

own_nhpi <- tenure_1721 %>% 
  mutate(P_own_nhpi = AOSUE002/AOSUE001) %>% 
  dplyr::select(AREAKEY, P_own_nhpi)

own_hispanic <- tenure_1721 %>%  
  mutate(P_own_hispanic = AOSYE002/AOSYE001) %>% 
  dplyr::select(AREAKEY, P_own_hispanic)

var_list <- list(rent_white, rent_black, rent_aian, rent_asian, rent_nhpi, rent_hispanic, own_white, own_black, own_aian, own_asian, own_nhpi, own_hispanic)
VI_2021<-var_list %>%  reduce(left_join, by="AREAKEY")
VI_2021<-VI_2021 %>% mutate(S_Year = "2021")

# Replace any missing (NA) values with the mean
dataset_21<- VI_2021 %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
