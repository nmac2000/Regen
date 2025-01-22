###########################
# Early Chapter 1 Attempts#
###########################

table(bin_dis_GIS1$SPECIES_CD_1_1, bin_dis_GIS1$SPECIES_CD_2_1)



VRI <- bin_dis_GIS1 %>% 
  select(SampleSite_ID, SPECIES_CD_1_1:SPECIES_PCT_4_1, SpeciesAll:SX_x130, total_count_bin:SX_x130_bin ) %>% 
  select(-PROJ_HEIGHT_1_1,-PROJ_AGE_1_1,-PROJ_HEIGHT_2_1,-PROJ_AGE_2_1)
