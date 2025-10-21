# Format regeneration data

library(tidyverse)
regen_STM_read <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/AllPlotsRegen.csv")



regen_STM <- regen_STM_read %>% 
  rename(SampleSite_ID = Plot.ID,
         x0 = X0.10cm.adj,
         x10 = X10.29cm.adj,
         x30 = X30.130cm.adj,
         x130 = X.1.3m..4cm.adj,
         total = total.seedlings,
         BEC_Zone = BEC_Zone_fromMeta,
         BEC_Subzone = BEC_Subzone_fromMeta,
         FIRE_YEAR_1 = FIRE_YEAR_1_fromMeta) %>% 
  select(SampleSite_ID, Species, x0:total, BEC_Zone:BEC_Subzone, FIRE_YEAR_1)
table(regen_STM$Species)



regen_STM$Species <- sub("^$", "NR", regen_STM$Species)
regen_STM$Species <- sub("JR", "NR", regen_STM$Species)
regen_STM$Species <- sub("no regen", "NR", regen_STM$Species)
regen_STM$Species <- sub("ACT", "AT", regen_STM$Species)

table(regen_STM$Species)

regen_STM <- regen_STM %>%
  mutate(
    x0 = ifelse(is.na(x0), 0, x0),
    x10 = ifelse(is.na(x10), 0, x10),
    x30 = ifelse(is.na(x30), 0, x30),
    x130 = ifelse(is.na(x130), 0, x130)
  )

regen_STM <- regen_STM %>%
  mutate(Species = ifelse(
    (x0 > 0 | x10 > 0 | x30 > 0 | x130 > 0) & Species == "NR",
    "PLI",
    Species
  ))

regen_STM$Plot_Sp <- paste(regen_STM$SampleSite_ID, regen_STM$Species, sep = "")

create_species_columns <- function(data, species_list, columns) {
  for (species in species_list) {
    for (col in columns) {
      new_col_name <- paste0(species, "_", col)
      data[[new_col_name]] <- ifelse(data$Species == species, data[[col]], 0)
    }
  }
  return(data)
}
species_list <- c("FDI", "PLI", "AT", "SX")
columns <- c("total", "x0", "x10", "x30", "x130")

regen_STM_sp <- create_species_columns(regen_STM, species_list, columns)

regen_combo_STM <- regen_STM_sp %>%
  group_by(SampleSite_ID) %>%
  summarize(SpeciesAll = paste(Species, collapse = ", "),
            total_count = sum(total),
            total_tph = total_count*200,
            total_x0 = sum(x0),
            total_x10 = sum(x10),
            total_x30 = sum(x30),
            total_x130 = sum(x130),
            FDI_count = sum(FDI_total),
            FDI_tph = FDI_count*200,
            FDI_x0 = sum(FDI_x0),
            FDI_x10 = sum(FDI_x10),
            FDI_x30 = sum(FDI_x30),
            FDI_x130 = sum(FDI_x130),
            PLI_count = sum(PLI_total),
            PLI_tph = PLI_count*200,
            PLI_x0 = sum(PLI_x0),
            PLI_x10 = sum(PLI_x10),
            PLI_x30 = sum(PLI_x30),
            PLI_x130 = sum(PLI_x130),
            AT_count = sum(AT_total),
            AT_x0 = sum(AT_x0),
            AT_x10 = sum(AT_x10),
            AT_x30 = sum(AT_x30),
            AT_x130 = sum(AT_x130),
            SX_count = sum(SX_total),
            SX_x0 = sum(SX_x0),
            SX_x10 = sum(SX_x10),
            SX_x30 = sum(SX_x30),
            SX_x130 = sum(SX_x130)) %>% 
  ungroup()



## now add percentages and such
regen_percents <- regen_combo_STM %>% 
  mutate(PLI_percent = round(100*PLI_count/total_count)) %>% 
  mutate(FDI_percent = round(100*FDI_count/total_count)) %>% 
  mutate(SX_percent = round(100*SX_count/total_count)) %>% 
  mutate(AT_percent = round(100*AT_count/total_count)) %>% 
  select(SampleSite_ID, SpeciesAll, PLI_percent:AT_percent)

regen_percents$Species <- ""
regen_percents$Species <- ifelse(regen_percents$PLI_percent > 0, paste(regen_percents$Species, "PLI"),regen_percents$Species)
regen_percents$Species <- ifelse(regen_percents$FDI_percent > 0, paste(regen_percents$Species, "FDI"),regen_percents$Species)
regen_percents$Species <- ifelse(regen_percents$SX_percent > 0, paste(regen_percents$Species, "SX"),regen_percents$Species)
regen_percents$Species <- ifelse(regen_percents$AT_percent > 0, paste(regen_percents$Species, "AT"),regen_percents$Species)
regen_percents$Species <- ifelse(regen_percents$SpeciesAll == "NR", "NR",regen_percents$Species)

regen_percents <- regen_percents %>% 
  mutate(Dominant = case_when(
    PLI_percent >= 80 ~ "PLI",
    FDI_percent >= 80 ~ "FDI",
    SX_percent >= 80 ~ "SX",
    AT_percent >=80~ "AT",
    PLI_percent < 80 & FDI_percent < 80 & AT_percent < 80 & SX_percent < 80 ~ "Mix"
  ))

regen_percents <- regen_percents %>%
  mutate(LeadingSpecies = case_when(
    PLI_percent >= FDI_percent & PLI_percent >= AT_percent & PLI_percent >= SX_percent ~ "PLI",
    FDI_percent >= PLI_percent & FDI_percent >= AT_percent & FDI_percent >= SX_percent ~ "FDI",
    AT_percent >= PLI_percent & AT_percent >= FDI_percent & AT_percent >= SX_percent ~ "AT",
    SX_percent >= PLI_percent & SX_percent >= FDI_percent & SX_percent >= AT_percent ~ "SX"
  ))

regen_percents$LeadingSpecies <- ifelse(regen_percents$SpeciesAll == "NR", "NR",regen_percents$LeadingSpecies)
regen_percents$Dominant <- ifelse(regen_percents$SpeciesAll == "NR", "NR",regen_percents$Dominant)

regen_percents <- regen_percents %>%
  mutate(LeadingPercent = pmax(PLI_percent, FDI_percent, AT_percent, SX_percent, na.rm = TRUE))

regen_percents_unfiltered <- regen_percents %>%
  rowwise() %>%
  mutate(
    # Build named vector, remove NAs and 0s
    sp_percents = list(c(PLI = PLI_percent, FDI = FDI_percent, AT = AT_percent, SX = SX_percent)),
    sp_percents = list(sp_percents[!is.na(sp_percents) & sp_percents > 0]),
    
    # Sort descending
    sorted_species = list(names(sort(sp_percents, decreasing = TRUE))),
    sorted_values = list(sort(sp_percents, decreasing = TRUE)),
    
    # Safely assign top 4 or NA
    LeadingSpecies  = ifelse(length(sorted_species) >= 1, sorted_species[1], NA_character_),
    LeadingPercent  = ifelse(length(sorted_values) >= 1, sorted_values[1], NA_real_),
    SecondSpecies   = ifelse(length(sorted_species) >= 2, sorted_species[2], NA_character_),
    SecondPercent   = ifelse(length(sorted_values) >= 2, sorted_values[2], NA_real_),
    ThirdSpecies    = ifelse(length(sorted_species) >= 3, sorted_species[3], NA_character_),
    ThirdPercent    = ifelse(length(sorted_values) >= 3, sorted_values[3], NA_real_),
    FourthSpecies   = ifelse(length(sorted_species) >= 4, sorted_species[4], NA_character_),
    FourthPercent   = ifelse(length(sorted_values) >= 4, sorted_values[4], NA_real_)
  ) %>%
  ungroup() %>%
  select(-sp_percents, -sorted_species, -sorted_values)

regen_percents$SampleSite_ID <- as.character(regen_percents$SampleSite_ID)

regen_percents <- regen_percents_unfiltered %>% 
  filter(SampleSite_ID %in% VRI_all$SampleSite_ID) 

regen_percents$PLI_percent <- ifelse(is.na(regen_percents$PLI_percent), 0, regen_percents$PLI_percent)
regen_percents$FDI_percent <- ifelse(is.na(regen_percents$FDI_percent), 0, regen_percents$FDI_percent)
regen_percents$SX_percent <- ifelse(is.na(regen_percents$SX_percent), 0, regen_percents$SX_percent)
regen_percents$AT_percent <- ifelse(is.na(regen_percents$AT_percent), 0, regen_percents$AT_percent)
