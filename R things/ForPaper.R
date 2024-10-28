### Looking at data for writeup ###
# October 18
# Nat MacMillan

bin_dis_GIS1 %>% 
  filter(BARC.x == 4) %>%
  filter(PLI_percent > 0) %>% 
  select(SpeciesAll, total_count, PLI_count, PARENT_SOILS, DRAIN_1, BARC.x, TWI)


bin_dis_GIS1 %>% 
  group_by(BEC_Zone, PLI_count_bin) %>% 
  filter(PLI_percent > 0) %>% 
  summarize(n = n())
