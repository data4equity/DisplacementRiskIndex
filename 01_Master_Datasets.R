# Load and Prepare Data ----

source("scripts/02_GEOS.R")
source("scripts/03_2000_data.R")
source("scripts/04_07_11_data.R")
source("scripts/05_12_16_data.R")
source("scripts/08_Subs_Hsg.R")
source("scripts/07_Schools.R")
source("scripts/09_Greenspace.R")
source("scripts/02_GEOS.R")

library(pacman)
p_load(tidyverse, tigris,sf, scales,ltm)
#census_api_key("6ed16ee8bb787c547084eaa835a2ccfba8abe586")
#sf::sf_use_s2(FALSE)

# Filter Down to Atlanta Block Groups
VI_2000<-left_join(bg_limits, VI_2000, by=c("GEOID" = "AREAKEY"))%>% st_set_geometry(NULL)
VI_2011<-left_join(bg_limits, VI_2011, by=c("GEOID"="AREAKEY"))%>% st_set_geometry(NULL)
VI_2016<-left_join(bg_limits, VI_2016, by=c("GEOID"="AREAKEY"))%>% st_set_geometry(NULL)

# Join the other datasets
dataset_00<-left_join(VI_2000, AH_00, by="GEOID")
dataset_00<-left_join(dataset_00, MHHI_00, by="GEOID")
dataset_00<-left_join(dataset_00, schools_00, by="GEOID")
dataset_00<-left_join(dataset_00, crime_00, by="GEOID")
dataset_00<-left_join(dataset_00, evict_00, by="GEOID")

dataset_11<-left_join(VI_2011, AH_11, by="GEOID")
dataset_11<-left_join(dataset_11, MHHI_07_11, by="GEOID")
dataset_11<-left_join(dataset_11, schools_11, by="GEOID")
dataset_11<-left_join(dataset_11, crime_11, by="GEOID")
dataset_11<-left_join(dataset_11, evict_11, by="GEOID")


dataset_16<-left_join(VI_2016, AH_16, by="GEOID")
dataset_16<-left_join(dataset_16, MHHI_12_16, by="GEOID")
dataset_16<-left_join(dataset_16, schools_16, by="GEOID")
dataset_16<-left_join(dataset_16, crime_16, by="GEOID")
dataset_16<-left_join(dataset_16, evict_16, by="GEOID")

# Replace any missing (NA) values with the mean
dataset_00<- dataset_00 %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
dataset_11<- dataset_11 %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
dataset_16<- dataset_16 %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))



## Data Descriptives ----
# Vulnerability Descriptives
dataset_00 %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSorLess, P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE)
dataset_11 %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSorLess, P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE)
dataset_16 %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSorLess, P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE)

# Vulnerability Descriptives (BeltLine)
dataset_00 %>% group_by(belt_flag)%>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSorLess,P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE)
dataset_11 %>% group_by(belt_flag) %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSorLess,P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE)
dataset_16 %>% group_by(belt_flag) %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSorLess,P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE)

# Housing Market Descriptive
dataset_00 %>% summarise_at(vars(PSU, CHU,P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)
dataset_11 %>% summarise_at(vars(PSU, CHU,P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)
dataset_16 %>% summarise_at(vars(PSU, CHU,P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)

# Housing Market Descriptives (BeltLine)
dataset_00 %>% group_by(belt_flag) %>% summarise_at(vars(PSU, CHU, P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)
dataset_11 %>% group_by(belt_flag)%>% summarise_at(vars(PSU, CHU, P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)
dataset_16 %>% group_by(belt_flag)%>% summarise_at(vars(PSU, CHU, P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)



## Construct new dataset that keeps the original information for Select Spatiotemporal Analyses
dataset_00g <- dataset_00

dataset_11g <- dataset_11 

dataset_16g <- dataset_16 

dataset_0016g<-bind_rows(dataset_00g, dataset_11g, dataset_16g)



## Variable Standardization and Scaling ----

# Z_Scores
dataset_00<-dataset_00 %>% mutate_at(vars(P_Black:R_File, -S_Year, -geometry), scale)
dataset_11<-dataset_11 %>% mutate_at(vars(P_Black:R_File, -S_Year, -geometry), scale)
dataset_16<-dataset_16 %>% mutate_at(vars(P_Black:R_File, -S_Year, -geometry), scale)

# Elderly +
# LEP +
# HSorLess +
# Renter +
# Poverty +
# MHV -
# MGR -
# P_Vacant +
# MHHI -

# Rescale 0-100

dataset_00<-dataset_00 %>% 
  mutate_at(vars(P_Black:P_Elderly, P_LEP, P_HSorLess, P_Renter, P_Poverty, P_Vacant, P_Single, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, LIC, Eligible_F, Eligible_R, R_Crime, R_Evict, R_File, P_Expire), rescale, to = c(0, 100)) %>% 
  mutate_at(vars(MHV, MGR, MHHI, CHU), rescale, to = c(100, 0)) %>%
  mutate(Index_Vuln = P_Black+ P_Hispanic+ P_AIAN+ P_ASIAN+ P_NHPI+ P_Elderly+ P_Single+ P_LEP+ P_Renter+ P_RentCostBurden+ P_Own_Cost_Burden+ P_Severe_RentCostBurden+ P_Severe_OwnCostBurden+ P_Poverty+ MHHI,
         Index_Housing = P_Expire+ CHU+ MHV+ MGR+ R_Crime+ R_Evict+ R_File+ Eligible_FR+ P_Vacant+ LIC+MHHI,
         Index_DR = Index_Vuln + Index_Housing) %>% 
  mutate(Index_Q_Vuln = ntile(Index_Vuln, 4) %>% as.character(),
         Index_Q_Housing = ntile(Index_Housing, 4) %>% as.character(),
         Index_Q_DR = ntile(Index_DR,4) %>% as.character()) %>% 
  mutate(Vuln_Cat = case_when(Index_Q_Vuln == "1" ~ "Low Vulnerability",
                              Index_Q_Vuln %in% c("2", "3") ~ "Moderate Vulnerability",
                              Index_Q_Vuln == "4" ~ "High Vulnerability"),
         House_Cat = case_when(Index_Q_Housing == "1" ~ "Low Vulnerability",
                               Index_Q_Housing %in% c("2", "3") ~ "Moderate Vulnerability",
                               Index_Q_Housing == "4" ~ "High Vulnerability"),
         DR_Cat = case_when(Index_Q_DR == "1" ~ "Low Displacement Risk",
                            Index_Q_DR %in% c("2", "3") ~ "Moderate Displacement Risk",
                            Index_Q_DR == "4" ~ "High Displacement Risk")) %>% 
  mutate(Vuln_Cat = factor(Vuln_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         House_Cat = factor(House_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         DR_Cat = factor(DR_Cat, levels = c("High Displacement Risk", "Moderate Displacement Risk","Low Displacement Risk")))

dataset_11<-dataset_11 %>% 
  mutate_at(vars(P_Black:P_Elderly, P_LEP, P_HSorLess, P_Renter, P_Poverty, P_Vacant, P_Single, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, LIC, Eligible_F, Eligible_R, R_Crime, R_Evict, R_File, P_Expire), rescale, to = c(0, 100)) %>% 
  mutate_at(vars(MHV, MGR, MHHI, CHU), rescale, to = c(100, 0)) %>%
  mutate(Index_Vuln = P_Black+ P_Hispanic+ P_AIAN+ P_ASIAN+ P_NHPI+ P_Elderly+ P_Single+ P_LEP+ P_Renter+ P_RentCostBurden+ P_Own_Cost_Burden+ P_Severe_RentCostBurden+ P_Severe_OwnCostBurden+ P_Poverty+ MHHI,
         Index_Housing = P_Expire+ CHU+ MHV+ MGR+ R_Crime+ R_Evict+ R_File+ Eligible_FR+ P_Vacant+ LIC+MHHI,
         Index_DR = Index_Vuln + Index_Housing) %>% 
  mutate(Index_Q_Vuln = ntile(Index_Vuln, 4) %>% as.character(),
         Index_Q_Housing = ntile(Index_Housing, 4) %>% as.character(),
         Index_Q_DR = ntile(Index_DR,4) %>% as.character()) %>% 
  mutate(Vuln_Cat = case_when(Index_Q_Vuln == "1" ~ "Low Vulnerability",
                              Index_Q_Vuln %in% c("2", "3") ~ "Moderate Vulnerability",
                              Index_Q_Vuln == "4" ~ "High Vulnerability"),
         House_Cat = case_when(Index_Q_Housing == "1" ~ "Low Vulnerability",
                               Index_Q_Housing %in% c("2", "3") ~ "Moderate Vulnerability",
                               Index_Q_Housing == "4" ~ "High Vulnerability"),
         DR_Cat = case_when(Index_Q_DR == "1" ~ "Low Displacement Risk",
                            Index_Q_DR %in% c("2", "3") ~ "Moderate Displacement Risk",
                            Index_Q_DR == "4" ~ "High Displacement Risk")) %>% 
  mutate(Vuln_Cat = factor(Vuln_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         House_Cat = factor(House_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         DR_Cat = factor(DR_Cat, levels = c("High Displacement Risk", "Moderate Displacement Risk","Low Displacement Risk")))


dataset_16<-dataset_16 %>% 
  mutate_at(vars(P_Black:P_Elderly, P_LEP, P_HSorLess, P_Renter, P_Poverty, P_Vacant, P_Single, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, LIC, Eligible_F, Eligible_R, R_Crime, R_Evict, R_File, P_Expire), rescale, to = c(0, 100)) %>% 
  mutate_at(vars(MHV, MGR, MHHI, CHU), rescale, to = c(100, 0)) %>%
  mutate(Index_Vuln = P_Black+ P_Hispanic+ P_AIAN+ P_ASIAN+ P_NHPI+ P_Elderly+ P_Single+ P_LEP+ P_Renter+ P_RentCostBurden+ P_Own_Cost_Burden+ P_Severe_RentCostBurden+ P_Severe_OwnCostBurden+ P_Poverty+ MHHI,
         Index_Housing = P_Expire+ CHU+ MHV+ MGR+ R_Crime+ R_Evict+ R_File+ Eligible_FR+ P_Vacant+ LIC+MHHI,
         Index_DR = Index_Vuln + Index_Housing) %>% 
  mutate(Index_Q_Vuln = ntile(Index_Vuln, 4) %>% as.character(),
         Index_Q_Housing = ntile(Index_Housing, 4) %>% as.character(),
         Index_Q_DR = ntile(Index_DR,4) %>% as.character()) %>% 
  mutate(Vuln_Cat = case_when(Index_Q_Vuln == "1" ~ "Low Vulnerability",
                              Index_Q_Vuln %in% c("2", "3") ~ "Moderate Vulnerability",
                              Index_Q_Vuln == "4" ~ "High Vulnerability"),
         House_Cat = case_when(Index_Q_Housing == "1" ~ "Low Vulnerability",
                               Index_Q_Housing %in% c("2", "3") ~ "Moderate Vulnerability",
                               Index_Q_Housing == "4" ~ "High Vulnerability"),
         DR_Cat = case_when(Index_Q_DR == "1" ~ "Low Displacement Risk",
                            Index_Q_DR %in% c("2", "3") ~ "Moderate Displacement Risk",
                            Index_Q_DR == "4" ~ "High Displacement Risk")) %>% 
  mutate(Vuln_Cat = factor(Vuln_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         House_Cat = factor(House_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         DR_Cat = factor(DR_Cat, levels = c("High Displacement Risk", "Moderate Displacement Risk","Low Displacement Risk")))


# Determine the cronbach alpha for the different indexes
alpha_vul_00 <- data.frame(dataset_00$P_Black, dataset_00$P_Hispanic, dataset_00$P_AIAN, dataset_00$P_ASIAN, dataset_00$P_NHPI, dataset_00$P_Elderly, dataset_00$P_Single, dataset_00$P_LEP, dataset_00$P_HSorLess, dataset_00$P_Renter, dataset_00$P_RentCostBurden, dataset_00$P_Own_Cost_Burden, dataset_00$P_Severe_RentCostBurden, dataset_00$P_Severe_OwnCostBurden, dataset_00$P_Poverty, dataset_00$MHHI)
cronbach.alpha(alpha_vul_00)

alpha_vul_11 <- data.frame(dataset_11$P_Black, dataset_11$P_Hispanic, dataset_11$P_AIAN, dataset_11$P_ASIAN, dataset_11$P_NHPI, dataset_11$P_Elderly, dataset_11$P_Single, dataset_11$P_LEP, dataset_11$P_HSorLess, dataset_11$P_Renter, dataset_11$P_RentCostBurden, dataset_11$P_Own_Cost_Burden, dataset_11$P_Severe_RentCostBurden, dataset_11$P_Severe_OwnCostBurden, dataset_11$P_Poverty, dataset_11$MHHI)
cronbach.alpha(alpha_vul_11)

alpha_vul_16 <- data.frame(dataset_16$P_Black, dataset_16$P_Hispanic, dataset_16$P_AIAN, dataset_16$P_ASIAN, dataset_16$P_NHPI, dataset_16$P_Elderly, dataset_16$P_Single, dataset_16$P_LEP, dataset_16$P_HSorLess, dataset_16$P_Renter, dataset_16$P_RentCostBurden, dataset_16$P_Own_Cost_Burden, dataset_16$P_Severe_RentCostBurden, dataset_16$P_Severe_OwnCostBurden, dataset_16$P_Poverty, dataset_16$MHHI)
cronbach.alpha(alpha_vul_16)

alpha_hm_00 <- data.frame(dataset_00$CHU,dataset_00$P_Expire, dataset_00$MHV, dataset_00$MGR, dataset_00$R_Crime, dataset_00$R_Evict, dataset_00$R_File, dataset_00$Eligible_FR, dataset_00$P_Vacant, dataset_00$LIC)
cronbach.alpha(alpha_hm_00)

alpha_hm_11 <- data.frame(dataset_11$CHU,dataset_11$P_Expire, dataset_11$MHV, dataset_11$MGR, dataset_11$R_Crime, dataset_11$R_Evict, dataset_11$R_File, dataset_11$Eligible_FR, dataset_11$P_Vacant, dataset_11$LIC)
cronbach.alpha(alpha_hm_11)

alpha_hm_16 <- data.frame(dataset_16$CHU,dataset_16$P_Expire, dataset_16$MHV, dataset_16$MGR, dataset_16$R_Crime, dataset_16$R_Evict, dataset_16$R_File, dataset_16$Eligible_FR, dataset_16$P_Vacant, dataset_16$LIC)
cronbach.alpha(alpha_hm_16)

alpha_dri_00 <- data.frame(dataset_00$P_Black, dataset_00$P_Hispanic, dataset_00$P_AIAN, dataset_00$P_ASIAN, dataset_00$P_NHPI, dataset_00$P_Elderly, dataset_00$P_Single, dataset_00$P_LEP, dataset_00$P_HSorLess, dataset_00$P_Renter, dataset_00$P_RentCostBurden, dataset_00$P_Own_Cost_Burden, dataset_00$P_Severe_RentCostBurden, dataset_00$P_Severe_OwnCostBurden, dataset_00$P_Poverty, dataset_00$MHHI,
                           dataset_00$CHU,dataset_00$P_Expire, dataset_00$MHV, dataset_00$MGR, dataset_00$R_Crime, dataset_00$R_Evict, dataset_00$R_File, dataset_00$Eligible_FR, dataset_00$P_Vacant, dataset_00$LIC)
cronbach.alpha(alpha_dri_00)

alpha_dri_11 <- data.frame(dataset_11$P_Black, dataset_11$P_Hispanic, dataset_11$P_AIAN, dataset_11$P_ASIAN, dataset_11$P_NHPI, dataset_11$P_Elderly, dataset_11$P_Single, dataset_11$P_LEP, dataset_11$P_HSorLess, dataset_11$P_Renter, dataset_11$P_RentCostBurden, dataset_11$P_Own_Cost_Burden, dataset_11$P_Severe_RentCostBurden, dataset_11$P_Severe_OwnCostBurden, dataset_11$P_Poverty, dataset_11$MHHI,
                           dataset_11$CHU,dataset_11$P_Expire, dataset_11$MHV, dataset_11$MGR, dataset_11$R_Crime, dataset_11$R_Evict, dataset_11$R_File, dataset_11$Eligible_FR, dataset_11$P_Vacant, dataset_11$LIC)
cronbach.alpha(alpha_dri_11)

alpha_dri_16 <- data.frame(dataset_16$P_Black, dataset_16$P_Hispanic, dataset_16$P_AIAN, dataset_16$P_ASIAN, dataset_16$P_NHPI, dataset_16$P_Elderly, dataset_16$P_Single, dataset_16$P_LEP, dataset_16$P_HSorLess, dataset_16$P_Renter, dataset_16$P_RentCostBurden, dataset_16$P_Own_Cost_Burden, dataset_16$P_Severe_RentCostBurden, dataset_16$P_Severe_OwnCostBurden, dataset_16$P_Poverty, dataset_16$MHHI,
                           dataset_16$CHU,dataset_16$P_Expire, dataset_16$MHV, dataset_16$MGR, dataset_16$R_Crime, dataset_16$R_Evict, dataset_16$R_File, dataset_16$Eligible_FR, dataset_16$P_Vacant, dataset_16$LIC)
cronbach.alpha(alpha_dri_16)

# Descriptives on DRI
summary(dataset_00$Index_DR)
summary(dataset_11$Index_DR)
summary(dataset_16$Index_DR)

# Save Out Datasets

# write_csv(dataset_00 %>% select(-geometry), "outputs/dataset_00.csv")
# write_csv(dataset_11%>% select(-geometry), "outputs/dataset_11.csv")
# write_csv(dataset_16%>% select(-geometry), "outputs/dataset_16.csv")
# write_csv(dataset_map_Housing, "outputs/vulnerability_index.csv")
