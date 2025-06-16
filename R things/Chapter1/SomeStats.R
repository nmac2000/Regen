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



table(STM_PLI$PLI_pre)
table(STM_PLI$PLI_post)
table(STM_PLI$PLI_pre,STM_PLI$PLI_post)

table(bin_dis_GIS1$FIRE_NUMBER_1, bin_dis_GIS1$FIRE_YEAR_1)
