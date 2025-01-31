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
library(tidyverse)
library(gridExtra)

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
predict_response(site.PLI.2i, "BEC_Subzone")  
  plot()
predict_response(site.FDI.3k, "BEC_Subzone")  
  plot()

#structure: 
#  species % (FDI and PLI)
ggpredict(structure.PLI.5a, "PLI_percent")
predict_response(structure.FDI.3j, c("FDI_percent"))
#  distance (FDI)
predict_response(structure.FDI.3j, c("Distance"))
  
#site:
#  BEC subzone (FDI and PLI)
predict_response(site.PLI.2j, "BEC_Subzone")
predict_response(site.FDI.3k, "BEC_Subzone")  
predict_response(site.FDI.3k, "PARENT_SOILS") 
predict_response(site.PLI.2j, "Slope") 
  plot()

#FDI
FDI_MCMT <- ggpredict(FDI.7, c("MCMT")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  scale_x_continuous(labels = ~ paste0(.x, "°"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Predicted Probability of FDI Occurrence",
       x = "MCMT", y = "Predicted Probability") +
  theme_classic()


FDI_percent <- ggpredict(FDI.7, c("FDI_percent")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  scale_x_continuous(labels = ~ paste0(.x, "%"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "FDI",x = "Pre-fire Douglas-fir Basal Area Composition", y = "Predicted Probability of FDI Occurrence") +
  theme_classic()

FDI_distance <- ggpredict(FDI.7, c("Distance")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "FDI",x = "Distance to Live Tree (m)", y = "Predicted Probability of FDI Occurrence") +
  theme_classic()

FDI_soil <- ggpredict(FDI.7, c("PARENT_SOILS")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high), color="grey", size=1) +
  geom_point(color="blue") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Parent Soil", y = "Predicted Probability of FDI Occurrence", title = "FDI") +
  theme_classic()

FDI_BEC <- ggpredict(site.FDI.3k, c("BEC_Subzone")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high), color="grey", size=1) +
  geom_point(color="blue") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "BEC Subzone", y = "Predicted Probability of FDI Occurrence", title = "FDI") +
  theme_classic()

grid.arrange(FDI_MCMT, FDI_percent, FDI_soil, FDI_distance)

#PLI
PLI_BARC <- ggpredict(PLI.6, "BARC.x") %>% 
  ggplot(mapping = aes(x=x, y=predicted)) +
#  geom_smooth(se=F) +
  geom_point() +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete(labels=c("Unburned", "Low", "Medium", "High")) +
  labs(x = "Fire Severity", y = "Predicted Probability") +
  theme_classic()

PLI_BEC <- ggpredict(PLI.6, "BEC_Subzone") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high), color="grey", size=1) +
  geom_point(colour="blue") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "PLI", x = "BEC Subzone", y="Predicted Probability of PLI Occurrence") +
  theme_classic()

PLI_percent <- ggpredict(PLI.6, "PLI_percent") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  scale_x_continuous(labels = ~ paste0(.x, "%"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "PLI",
       x = "Pre-fire Lodgepole Pine Basal Area Composition", y = "Predicted Probability of PLI Occurrence") +
  theme_classic()

PLI_slope <- ggpredict(site.PLI.2j, "Slope") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  scale_x_continuous(labels = ~ paste0(.x, "%"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "PLI",
       x = "Slope", y = "Predicted Probability of PLI Occurrence") +
  theme_classic()

grid.arrange(PLI_percent, PLI_BEC, PLI_BARC)


plot_grid(FDI_distance, FDI_percent, PLI_percent, labels=c("A", "B", "C"), ncol = 2, nrow = 2)
plot_grid(FDI_BEC, FDI_soil, PLI_BEC,PLI_slope, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)

#maybe more results?

ggpredict(structure.PLI.5a, "BARC.x") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
#  geom_point() +
#  geom_linerange(aes(ymin=conf.low, ymax = conf.high)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete(labels=c("Unburned", "Low", "Medium", "High")) +
  labs(x = "Fire Severity", y = "Predicted Probability") +
  theme_classic()

ggpredict(structure.FDI.3j, "BARC.x") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_point() +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete(labels=c("Unburned", "Low", "Medium", "High")) +
  labs(x = "Fire Severity", y = "Predicted Probability") +
  theme_classic()

ggplot(bin_dis_GIS1, aes(x=as.factor(FIRE_NUMBER_1),y=FDI_percent)) +
  geom_boxplot(outlier.color=NA)+
  geom_jitter(width =.25, aes( colour=as.factor(FDI_count_bin), shape = as.factor(BARC.x))) +
  scale_color_manual(values=c("#ED5151","#149ECE")) +
#  scale_shape_manual(values=c(21,24,22,4)) +
  scale_y_continuous(labels = ~ paste0(.x, "%"))+
  labs(x= "Fire Severity", y="FDI Percent", 
       colour = "FDI Occurrence") +
  theme_classic() 

