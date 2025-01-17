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

bin_dis_GIS1 %>% 
  filter(PLI_count >0) %>% 
  group_by() %>% 
  summarize(mean = median(PLI_tph))

library(ggeffects)





<<<<<<< HEAD

#structure: 
#  species % (FDI and PLI)

ggpredict(structure.PLI.5a, terms = c("PLI_percent", "BARC.x")) %>%
  plot()

ggpredict(structure.FDI.3j, terms = c("FDI_percent", "BARC.x")) %>% 
  plot()



#distance (FDI)
ggpredict(structure.FDI.3j, "Distance") %>% 
  plot()
  
#site:
#  BEC subzone (FDI and PLI)
predict_response(site.PLI.2i, "BEC_Subzone") %>% 
  plot()
predict_response(site.FDI.3k, "BEC_Subzone") %>% 
  plot()
=======
#structure: 
#  species % (FDI and PLI)
predict_response(structure.PLI.5a, "PLI_percent")
predict_response(structure.FDI.3j, c("FDI_percent"))
#  distance (FDI)
predict_response(structure.FDI.3j, c("Distance"))
  
#site:
#  BEC subzone (FDI and PLI)
predict_response(site.PLI.2j, "BEC_Subzone")
predict_response(site.FDI.3k, "BEC_Subzone")  
>>>>>>> e720a7c5c7b1b7cfdeb53d1512c475d990d2d9a8

#FDI
ggpredict(FDI.7, c("MCMT")) %>% 
  plot()

ggpredict(FDI.7, c("years_since")) %>% 
  plot()

ggpredict(FDI.7, c("FDI_percent")) %>% 
  plot()

ggpredict(FDI.7, c("Distance")) %>% 
  plot()

ggpredict(FDI.7, c("PARENT_SOILS")) %>% 
  plot()

#PLI
ggpredict(PLI.6, "years_since") %>% 
  plot()

ggpredict(PLI.6, "BARC.x") %>% 
  plot()

ggpredict(PLI.6, "BEC_Subzone") %>% 
  plot()

ggpredict(PLI.6, "PLI_percent") %>% 
  plot()
