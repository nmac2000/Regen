############################
# Synthsizing Plot Cleanup #
############################

# Fire Severity
bin_dis_GIS1 <- bin_dis_GIS %>%
  distinct(SampleSite_ID, .keep_all = T) %>% 
  filter(!SampleSite_ID %in% not_measured$SampleSite_ID) %>% 
  filter(!SampleSite_ID %in% c(456, 496, 1108, 1422, 1514, 1643))

BARC <- read.csv("https://raw.githubusercontent.com/nmac2000/Regen/main/Data/BARC.csv")
BARC <- BARC %>% 
  select(Card, MEAN) %>% 
  mutate(BARC = round(MEAN)) %>% 
  rename(SampleSite_ID = "Card")
bin_dis_GIS <- left_join(bin_dis_GIS, BARC, by = "SampleSite_ID")

bin_dis_GIS1$BARC.x <- bin_dis_GIS1$BARC.y 

bin_dis_GIS1 <- within(bin_dis_GIS1, {   
  BARC.x <- NA # need to initialize variable
  BARC.x[MEAN < .105] <- "1"
  BARC.x[MEAN >= .105 & MEAN < 0.275] <- "2"
  BARC.x[MEAN >= 0.275 & MEAN < 0.66] <- "3"
  BARC.x[MEAN >= 0.66] <- "4"
} )



# NA value cleanup
bin_dis_GIS1 <- bin_dis_GIS %>%
  distinct(SampleSite_ID, .keep_all = T) %>% 
  filter(!SampleSite_ID %in% not_measured$SampleSite_ID) %>% 
  filter(!SampleSite_ID %in% c(456, 496, 1108, 1422, 1514, 1643))

# NA Distance

bin_dis_GIS1 <- bin_dis_GIS1 %>%
  mutate(Distance = ifelse(SampleSite_ID == 1511, 4, Distance)) %>%
  mutate(Distance = ifelse(SampleSite_ID == 3, 4, Distance))



# And now add values to missing dNBR #
bin_dis_GIS1 <- bin_dis_GIS1 %>% 
  mutate(MEAN = ifelse(SampleSite_ID == 1153, MEAN[which(SampleSite_ID == 2626)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1182, MEAN[which(SampleSite_ID == 2644)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1500, MEAN[which(SampleSite_ID == 1760)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1501, MEAN[which(SampleSite_ID == 1761)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1502, MEAN[which(SampleSite_ID == 1770)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1503, MEAN[which(SampleSite_ID == 1771)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1531, MEAN[which(SampleSite_ID == 1791)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1192, MEAN[which(SampleSite_ID == 2644)] , MEAN),
         MEAN = ifelse(SampleSite_ID == 1484, mean(c(0.485442, 0.462872, 0.575654, 0.470040)), MEAN),
         MEAN = ifelse(SampleSite_ID == 1504, mean(c(0.356386, 0.246404)), MEAN))




# remove VRI non-treed sites
bin_dis_GIS1 <- bin_dis_GIS1 %>% 
  filter(SPECIES_CD_1_1 != "")

# remove MS BEC zone (not enough observations)
bin_dis_GIS1 <- bin_dis_GIS1 %>% 
  filter(BEC_Zone != "MS")
# ensure y variable is a factor for dead versus alive
bin_dis_GIS1$PLI.f <- as.factor(bin_dis_GIS1$PLI_count_bin)
bin_dis_GIS1$FDI.f <- as.factor(bin_dis_GIS1$FDI_count_bin)
# y=1 for dead trees, y=0 for live trees
class(bin_dis_GIS1$PLI.f)
bin_dis_GIS1$Distance <- as.numeric(bin_dis_GIS1$Distance)
bin_dis_GIS1$CROWN_CLOSURE_1 <- as.numeric(bin_dis_GIS1$CROWN_CLOSURE_1)
bin_dis_GIS1$PROJ_AGE_1_1 <- as.numeric(bin_dis_GIS1$PROJ_AGE_1_1)
bin_dis_GIS1$PROJ_HEIGHT_1_1 <- as.numeric(bin_dis_GIS1$PROJ_HEIGHT_1_1)
bin_dis_GIS1$DISTANCE_TO_PERIMETER <- as.numeric(bin_dis_GIS1$DISTANCE_TO_PERIMETER)
bin_dis_GIS1$years_since <- as.numeric(2023-bin_dis_GIS1$FIRE_YEAR_1)
bin_dis_GIS1$DISTURBANCE_TYPE_1 <- as.factor(bin_dis_GIS1$DISTURBANCE_TYPE_1)
bin_dis_GIS1$SPECIES_CD_1_1 <- factor(bin_dis_GIS1$SPECIES_CD_1_1, levels = c("PLI", "FDI", "AT", "SX"))
bin_dis_GIS1$BARC.x <- as.factor(bin_dis_GIS1$BARC.x)

# combine parent material categories

table(bin_dis_GIS_test$PARENT_SOILS)
bin_dis_GIS_test <- bin_dis_GIS1 %>% 
  filter(!PARENT_SOILS %in% c("ORGANIC ", "ROCK ")) %>% 
  mutate()


#write.csv(bin_dis_GIS1, "C:/Users/nmac2000/Documents/regen project/Regen/Data/bin_dis.csv")
bin_dis_GIS1 <- read.csv("https://raw.githubusercontent.com/nmac2000/Regen/main/Data/bin_dis.csv", header = TRUE)
bin_dis_GIS1 <- bin_dis_GIS1 %>% 
  select(-X, -X.x)
  
  
  
