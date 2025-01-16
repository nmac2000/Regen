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

ggeffects::ggpredict(structure.FDI.3j, terms=c("FDI_percent")) %>%  
  ggplot(mapping = aes (x=x, y=predicted, color= group)) +
  geom_smooth() +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  labs(title = "Predicted Probability of FDI Occurrence",
       x = "", y = "Predicted Probability") +
  theme_classic()

ggeffects::ggpredict(structure.PLI.5a, terms=c("BARC.x")) %>%  
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth() +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  labs(title = "Predicted Probability of PLI Occurrence",
       x = "", y = "Predicted Probability") +
  theme_classic()


predict_response(FDI.7, terms=c("Distance", "BARC.x")) %>% 
     mutate(group = ordered(as_factor(group))) %>% 
     ggplot(mapping = aes (x=x, y=predicted, color= group)) +
     geom_smooth() +
    labs(title = "Predicted Probability of FDI Occurrence",
                   x = "Distance to Live Tree", y = "Predicted Probability") +
     theme_classic

predict_response(structure.FDI.3j, c("Distance")) %>% 
  ggplot(mapping = aes (x=x, y=predicted, color= group, fill = group)) +
  geom_smooth(se = F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  labs(title = "Predicted Probability of FDI Occurrence",
       x = "Distance to Live Tree", y = "Predicted Probability") +
  theme_classic()


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


