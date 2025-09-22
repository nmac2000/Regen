# Chapter 1 stats

STM %>% 
  filter(Dominant == "NR") %>% 
  pull(Dominant_pre) %>% 
  table()

STM %>% 
  filter(Dominant_pre != "Mix" & Dominant == "Mix") %>% 
  pull(Dominant_pre) %>% 
  table()

STM_PLI %>% 
  filter(PLI_pre != "NoPLI" & PLI_post == "NoPLI") %>% 
  select(BEC_Zone, BEC_Subzone) %>% 
  table()

regen_combo_STM %>% 
  filter(PLI_count > 0 | SX_count > 0 | FDI_count > 0) %>% 
  select( PLI_count, SX_count, FDI_count)

table(STM_PLI$PLI_pre)
table(STM_PLI$PLI_post)
table(STM_PLI$PLI_pre,STM_PLI$PLI_post)

table(bin_dis_GIS1$FIRE_NUMBER_1, bin_dis_GIS1$FIRE_YEAR_1)

table(STM$Dominant)

table(STM_AT$Dominant_pre, STM_AT$LeadingSpecies_pre)

STM_SX %>% 
  filter(SX_pre != "NoSX" & SX_post == "NoSX") %>% 
  pull(LeadingSpecies) %>% 
  table()

VRI_read_2023 <- read.csv("C:/Users/nmac2000/Documents/regen project/Regen/Dataframes/regen_binary.csv")
GIS_2023 <- VRI_read_2023 %>% 
  filter(SPECIES_CD_1_1 != "") %>% 
  distinct(SampleSite_ID, .keep_all = T) %>% 
  filter(!SampleSite_ID %in% not_measured$SampleSite_ID) %>% 
  filter(!SampleSite_ID %in% c(456, 496, 1108, 1422, 1514, 1643, 1511)) 
GIS_2023$SampleSite_ID <- as.character(GIS_2023$SampleSite_ID)

VRI_read_2024 <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/VRI.csv")

GIS_2024 <- VRI_read_2024 %>% 
  rename(SampleSite_ID = Plot_ID)%>% 
  filter(SPECIES_CD_1_1 != "") %>% 
  distinct(SampleSite_ID, .keep_all = T) %>% 
  filter(!SampleSite_ID %in% not_measured$SampleSite_ID) %>% 
  filter(!SampleSite_ID %in% c(456, 496, 1108, 1422, 1514, 1643, 1511)) 

GIS_2024$SampleSite_ID <- as.character(GIS_2024$SampleSite_ID)

names(GIS_2023)

GIS_2023 <- GIS_2023 %>% 
  select(names(GIS_2024))

GIS_all <- bind_rows(GIS_2023, GIS_2024) %>% 
  filter(SampleSite_ID %in% regen_percents$SampleSite_ID) 

table(GIS_all$FIRE_NUMBER_1, GIS_all$FIRE_YEAR_1)
