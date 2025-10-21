# Format VRI data

## 2023 data
VRI_read_2023 <- read.csv("C:/Users/nmac2000/Documents/regen project/Regen/Dataframes/regen_binary.csv")
VRI_2023 <- VRI_read_2023 %>% 
  select(SampleSite_ID, BEC_Zone, BEC_Subzone, BEC_Variant, CROWN_CLOSURE_1:SPECIES_PCT_4_1, FIRE_YEAR_1) %>% 
  filter(SPECIES_CD_1_1 != "") %>% 
  distinct(SampleSite_ID, .keep_all = T) %>% 
  filter(!SampleSite_ID %in% not_measured$SampleSite_ID) %>% 
  filter(!SampleSite_ID %in% c(456, 496, 1108, 1422, 1514, 1643, 1511)) 

VRI_2023$SampleSite_ID <- as.character(VRI_2023$SampleSite_ID)

VRI_2023$PLI_percent_pre <- 0  
VRI_2023$FDI_percent_pre <- 0  
VRI_2023$SX_percent_pre <- 0
VRI_2023$AT_percent_pre <- 0 

VRI_2023$PLI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_1_1 == "PLI", 
                               VRI_2023$SPECIES_PCT_1_1, VRI_2023$PLI_percent_pre)
VRI_2023$PLI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_2_1 == "PLI", 
                               VRI_2023$SPECIES_PCT_2_1, VRI_2023$PLI_percent_pre)
VRI_2023$PLI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_3_1 == "PLI", 
                               VRI_2023$SPECIES_PCT_3_1, VRI_2023$PLI_percent_pre)
VRI_2023$PLI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_4_1 == "PLI", 
                               VRI_2023$SPECIES_PCT_4_1, VRI_2023$PLI_percent_pre)



VRI_2023$FDI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_1_1 == "FDI",
                               VRI_2023$SPECIES_PCT_1_1, VRI_2023$FDI_percent_pre)
VRI_2023$FDI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_2_1 == "FDI", 
                               VRI_2023$SPECIES_PCT_2_1, VRI_2023$FDI_percent_pre)
VRI_2023$FDI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_3_1 == "FDI", 
                               VRI_2023$SPECIES_PCT_3_1, VRI_2023$FDI_percent_pre)
VRI_2023$FDI_percent_pre <- ifelse(VRI_2023$SPECIES_CD_4_1 == "FDI", 
                               VRI_2023$SPECIES_PCT_4_1, VRI_2023$FDI_percent_pre)


VRI_2023$SX_percent_pre <- ifelse(VRI_2023$SPECIES_CD_1_1 == "SX",
                              VRI_2023$SPECIES_PCT_1_1, VRI_2023$SX_percent_pre)
VRI_2023$SX_percent_pre <- ifelse(VRI_2023$SPECIES_CD_2_1 == "SX", 
                              VRI_2023$SPECIES_PCT_2_1, VRI_2023$SX_percent_pre)
VRI_2023$SX_percent_pre <- ifelse(VRI_2023$SPECIES_CD_3_1 == "SX", 
                              VRI_2023$SPECIES_PCT_3_1, VRI_2023$SX_percent_pre)
VRI_2023$SX_percent_pre <- ifelse(VRI_2023$SPECIES_CD_4_1 == "SX", 
                              VRI_2023$SPECIES_PCT_4_1, VRI_2023$SX_percent_pre)


VRI_2023$AT_percent_pre <- ifelse(VRI_2023$SPECIES_CD_1_1 == "AT",
                              VRI_2023$SPECIES_PCT_1_1, VRI_2023$AT_percent_pre)
VRI_2023$AT_percent_pre <- ifelse(VRI_2023$SPECIES_CD_2_1 == "AT", 
                              VRI_2023$SPECIES_PCT_2_1, VRI_2023$AT_percent_pre)
VRI_2023$AT_percent_pre <- ifelse(VRI_2023$SPECIES_CD_3_1 == "AT", 
                              VRI_2023$SPECIES_PCT_3_1, VRI_2023$AT_percent_pre)
VRI_2023$AT_percent_pre <- ifelse(VRI_2023$SPECIES_CD_4_1 == "AT", 
                              VRI_2023$SPECIES_PCT_4_1, VRI_2023$AT_percent_pre)

VRI_2023$Species_pre <- ""
VRI_2023$Species_pre <- ifelse(VRI_2023$PLI_percent_pre > 0, paste(VRI_2023$Species_pre, "PLI"),VRI_2023$Species_pre)
VRI_2023$Species_pre <- ifelse(VRI_2023$FDI_percent_pre > 0, paste(VRI_2023$Species_pre, "FDI"),VRI_2023$Species_pre)
VRI_2023$Species_pre <- ifelse(VRI_2023$SX_percent_pre > 0, paste(VRI_2023$Species_pre, "SX"),VRI_2023$Species_pre)
VRI_2023$Species_pre <- ifelse(VRI_2023$AT_percent_pre > 0, paste(VRI_2023$Species_pre, "AT"),VRI_2023$Species_pre)

VRI_2023$Year <- 2023

## 2024 data
VRI_read_2024 <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/VRI.csv")

VRI_2024 <- VRI_read_2024 %>% 
  select(Plot_ID, BEC_Zone, BEC_Subzone, BEC_Variant, CROWN_CLOSURE_1:SPECIES_PCT_4_1, FIRE_YEAR_1) %>% 
  rename(SampleSite_ID = Plot_ID)%>% 
  filter(SPECIES_CD_1_1 != "") %>% 
  distinct(SampleSite_ID, .keep_all = T) %>% 
  filter(!SampleSite_ID %in% not_measured$SampleSite_ID) %>% 
  filter(!SampleSite_ID %in% c(456, 496, 1108, 1422, 1514, 1643, 1511)) 

VRI_2024$SampleSite_ID <- as.character(VRI_2024$SampleSite_ID)

VRI_2024$SPECIES_CD_1_1 <- sub("\\bSX\\b", "SX", VRI_2024$SPECIES_CD_1_1)
VRI_2024$SPECIES_CD_1_1 <- sub("\\bS\\b", "SX", VRI_2024$SPECIES_CD_1_1)
VRI_2024$SPECIES_CD_1_1 <- sub("\\bSE\\b", "SX", VRI_2024$SPECIES_CD_1_1)
VRI_2024$SPECIES_CD_1_1 <- sub("\\bPL\\b", "PLI", VRI_2024$SPECIES_CD_1_1)
VRI_2024$SPECIES_CD_1_1 <- sub("\\bFD\\b", "FDI", VRI_2024$SPECIES_CD_1_1)
VRI_2024$SPECIES_CD_1_1 <- sub("\\bPA\\b", "PLI", VRI_2024$SPECIES_CD_1_1)

VRI_2024$SPECIES_CD_2_1 <- sub("\\bSX\\b", "SX", VRI_2024$SPECIES_CD_2_1)
VRI_2024$SPECIES_CD_2_1 <- sub("\\bS\\b", "SX", VRI_2024$SPECIES_CD_2_1)
VRI_2024$SPECIES_CD_2_1 <- sub("\\bSE\\b", "SX", VRI_2024$SPECIES_CD_2_1)
VRI_2024$SPECIES_CD_2_1 <- sub("\\bPL\\b", "PLI", VRI_2024$SPECIES_CD_2_1)
VRI_2024$SPECIES_CD_2_1 <- sub("\\bFD\\b", "FDI", VRI_2024$SPECIES_CD_2_1)
VRI_2024$SPECIES_CD_2_1 <- sub("\\bPA\\b", "PLI", VRI_2024$SPECIES_CD_2_1)

VRI_2024$SPECIES_CD_3_1 <- sub("\\bSX\\b", "SX", VRI_2024$SPECIES_CD_3_1)
VRI_2024$SPECIES_CD_3_1 <- sub("\\bS\\b", "SX", VRI_2024$SPECIES_CD_3_1)
VRI_2024$SPECIES_CD_3_1 <- sub("\\bSE\\b", "SX", VRI_2024$SPECIES_CD_3_1)
VRI_2024$SPECIES_CD_3_1 <- sub("\\bPL\\b", "PLI", VRI_2024$SPECIES_CD_3_1)
VRI_2024$SPECIES_CD_3_1 <- sub("\\bFD\\b", "FDI", VRI_2024$SPECIES_CD_3_1)
VRI_2024$SPECIES_CD_3_1 <- sub("\\bPA\\b", "PLI", VRI_2024$SPECIES_CD_3_1)

VRI_2024$SPECIES_CD_4_1 <- sub("\\bSX\\b", "SX", VRI_2024$SPECIES_CD_4_1)
VRI_2024$SPECIES_CD_4_1 <- sub("\\bS\\b", "SX", VRI_2024$SPECIES_CD_4_1)
VRI_2024$SPECIES_CD_4_1 <- sub("\\bSE\\b", "SX", VRI_2024$SPECIES_CD_4_1)
VRI_2024$SPECIES_CD_4_1 <- sub("\\bPL\\b", "PLI", VRI_2024$SPECIES_CD_4_1)
VRI_2024$SPECIES_CD_4_1 <- sub("\\bFD\\b", "FDI", VRI_2024$SPECIES_CD_4_1)
VRI_2024$SPECIES_CD_4_1 <- sub("\\bPA\\b", "PLI", VRI_2024$SPECIES_CD_4_1)



VRI_2024$PLI_percent_pre <- 0  
VRI_2024$FDI_percent_pre <- 0  
VRI_2024$SX_percent_pre <- 0
VRI_2024$AT_percent_pre <- 0 

VRI_2024$PLI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_1_1 == "PLI", 
                                   VRI_2024$SPECIES_PCT_1_1, VRI_2024$PLI_percent_pre)
VRI_2024$PLI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_2_1 == "PLI", 
                                   VRI_2024$SPECIES_PCT_2_1, VRI_2024$PLI_percent_pre)
VRI_2024$PLI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_3_1 == "PLI", 
                                   VRI_2024$SPECIES_PCT_3_1, VRI_2024$PLI_percent_pre)
VRI_2024$PLI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_4_1 == "PLI", 
                                   VRI_2024$SPECIES_PCT_4_1, VRI_2024$PLI_percent_pre)



VRI_2024$FDI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_1_1 == "FDI",
                                   VRI_2024$SPECIES_PCT_1_1, VRI_2024$FDI_percent_pre)
VRI_2024$FDI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_2_1 == "FDI", 
                                   VRI_2024$SPECIES_PCT_2_1, VRI_2024$FDI_percent_pre)
VRI_2024$FDI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_3_1 == "FDI", 
                                   VRI_2024$SPECIES_PCT_3_1, VRI_2024$FDI_percent_pre)
VRI_2024$FDI_percent_pre <- ifelse(VRI_2024$SPECIES_CD_4_1 == "FDI", 
                                   VRI_2024$SPECIES_PCT_4_1, VRI_2024$FDI_percent_pre)

  
VRI_2024$SX_percent_pre <- ifelse(VRI_2024$SPECIES_CD_1_1 == "SX",
                          VRI_2024$SPECIES_PCT_1_1, VRI_2024$SX_percent_pre)
VRI_2024$SX_percent_pre <- ifelse(VRI_2024$SPECIES_CD_2_1 == "SX", 
                          VRI_2024$SPECIES_PCT_2_1, VRI_2024$SX_percent_pre)
VRI_2024$SX_percent_pre <- ifelse(VRI_2024$SPECIES_CD_3_1 == "SX", 
                          VRI_2024$SPECIES_PCT_3_1, VRI_2024$SX_percent_pre)
VRI_2024$SX_percent_pre <- ifelse(VRI_2024$SPECIES_CD_4_1 == "SX", 
                          VRI_2024$SPECIES_PCT_4_1, VRI_2024$SX_percent_pre)

 
VRI_2024$AT_percent_pre <- ifelse(VRI_2024$SPECIES_CD_1_1 == "AT",
                          VRI_2024$SPECIES_PCT_1_1, VRI_2024$AT_percent_pre)
VRI_2024$AT_percent_pre <- ifelse(VRI_2024$SPECIES_CD_2_1 == "AT", 
                          VRI_2024$SPECIES_PCT_2_1, VRI_2024$AT_percent_pre)
VRI_2024$AT_percent_pre <- ifelse(VRI_2024$SPECIES_CD_3_1 == "AT", 
                          VRI_2024$SPECIES_PCT_3_1, VRI_2024$AT_percent_pre)
VRI_2024$AT_percent_pre <- ifelse(VRI_2024$SPECIES_CD_4_1 == "AT", 
                          VRI_2024$SPECIES_PCT_4_1, VRI_2024$AT_percent_pre)

VRI_2024$Species_pre <- ""
VRI_2024$Species_pre <- ifelse(VRI_2024$PLI_percent_pre > 0, paste(VRI_2024$Species_pre, "PLI"),VRI_2024$Species_pre)
VRI_2024$Species_pre <- ifelse(VRI_2024$FDI_percent_pre > 0, paste(VRI_2024$Species_pre, "FDI"),VRI_2024$Species_pre)
VRI_2024$Species_pre <- ifelse(VRI_2024$SX_percent_pre > 0, paste(VRI_2024$Species_pre, "SX"),VRI_2024$Species_pre)
VRI_2024$Species_pre <- ifelse(VRI_2024$AT_percent_pre > 0, paste(VRI_2024$Species_pre, "AT"),VRI_2024$Species_pre)

VRI_2024$Year <- 2024

# Combine years
VRI_all <- bind_rows(VRI_2023, VRI_2024)

VRI_all <- VRI_all %>% 
  select(-CROWN_CLOSURE_1, -PROJ_AGE_1_1, -PROJ_HEIGHT_1_1, -PROJ_AGE_2_1, -PROJ_HEIGHT_2_1)

VRI_all <- VRI_all %>% 
  mutate(Dominant_pre = case_when(
    PLI_percent_pre >= 80 ~ "PLI",
    FDI_percent_pre >= 80 ~ "FDI",
    SX_percent_pre >= 80 ~ "SX",
    AT_percent_pre >=80~ "AT",
    PLI_percent_pre < 80 & FDI_percent_pre < 80 & AT_percent_pre < 80 & SX_percent_pre < 80 ~ "Mix"
  ))

VRI_all <- VRI_all %>%
  mutate(LeadingSpecies_pre = case_when(
    PLI_percent_pre >= FDI_percent_pre & PLI_percent_pre >= AT_percent_pre & PLI_percent_pre >= SX_percent_pre ~ "PLI",
    FDI_percent_pre >= PLI_percent_pre & FDI_percent_pre >= AT_percent_pre & FDI_percent_pre >= SX_percent_pre ~ "FDI",
    AT_percent_pre >= PLI_percent_pre & AT_percent_pre >= FDI_percent_pre & AT_percent_pre >= SX_percent_pre ~ "AT",
    SX_percent_pre >= PLI_percent_pre & SX_percent_pre >= FDI_percent_pre & SX_percent_pre >= AT_percent_pre ~ "SX"
  ))

VRI_all <- VRI_all %>%
  mutate(LeadingPercent_pre = pmax(PLI_percent_pre, FDI_percent_pre, AT_percent_pre, SX_percent_pre, na.rm = TRUE))

VRI_all_unfiltered <- VRI_all %>%
  rowwise() %>%
  mutate(
    # Build named vector, remove NAs and 0s
    sp_percents = list(c(PLI = PLI_percent_pre, FDI = FDI_percent_pre, AT = AT_percent_pre, SX = SX_percent_pre)),
    sp_percents = list(sp_percents[!is.na(sp_percents) & sp_percents > 0]),
    
    # Sort descending
    sorted_species = list(names(sort(sp_percents, decreasing = TRUE))),
    sorted_values = list(sort(sp_percents, decreasing = TRUE)),
    
    # Safely assign top 4 or NA
    LeadingSpecies_pre  = ifelse(length(sorted_species) >= 1, sorted_species[1], NA_character_),
    LeadingPercent_pre  = ifelse(length(sorted_values) >= 1, sorted_values[1], NA_real_),
    SecondSpecies_pre   = ifelse(length(sorted_species) >= 2, sorted_species[2], NA_character_),
    SecondPercent_pre   = ifelse(length(sorted_values) >= 2, sorted_values[2], NA_real_),
    ThirdSpecies_pre    = ifelse(length(sorted_species) >= 3, sorted_species[3], NA_character_),
    ThirdPercent_pre    = ifelse(length(sorted_values) >= 3, sorted_values[3], NA_real_),
    FourthSpecies_pre   = ifelse(length(sorted_species) >= 4, sorted_species[4], NA_character_),
    FourthPercent_pre   = ifelse(length(sorted_values) >= 4, sorted_values[4], NA_real_)
  ) %>%
  ungroup() %>%
  select(-sp_percents, -sorted_species, -sorted_values)

VRI_all <- VRI_all_unfiltered %>% 
  filter(SampleSite_ID %in% regen_percents$SampleSite_ID) 

VRI2023_fires <- VRI_read_2023 %>% 
  select(Card.x, FIRE_NUMBER_1) %>% 
  mutate(SampleSite_ID = as.character(Card.x)) %>% 
  select(-Card.x) %>% 
  filter(SampleSite_ID %in% STM$SampleSite_ID) 

VRI2024_fires <- VRI_read_2024 %>% 
  select(Plot_ID, FIRE_NUMBER_1) %>% 
  mutate(SampleSite_ID = as.character(Plot_ID)) %>% 
  select(-Plot_ID) %>% 
  filter(SampleSite_ID %in% STM$SampleSite_ID) 

VRI_fires <- rbind(VRI2024_fires, VRI2023_fires) 

VRI2023_fires %>% 
  group_by(FIRE_NUMBER_1) %>% 
  summarize(count = n())

names(VRI_read_2023)
write.csv(STM, "C:/Users/nmac2000/Desktop/Chapter2/data/STM.csv")
VRI_all <- read.csv("C:/Users/nmac2000/Desktop/Chapter2/data/STM.csv")

