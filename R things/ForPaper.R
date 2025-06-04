### Looking at data for writeup ###
# October 18
# Nat MacMillan

bin_dis_GIS1 %>% 
  filter(BARC.x == 4) %>%
  filter(PLI_percent > 0) %>% 
  select(SpeciesAll, total_count, PLI_count, PARENT_SOILS, DRAIN_1, BARC.x, TWI)

bin_dis_GIS1 %>% 
  filter(PLI_tph < max(PLI_tph)) %>% 
  summarize(max = max(PLI_tph))

bin_dis_GCard.xbin_dis_GIS1 %>% 
  group_by(BEC_Subzone, PLI_count_bin) %>% 
#  filter(PLI_percent > 0) %>% 
  summarize(n = n())

bin_dis_GIS1 %>% 
  filter(PLI_percent == 0) %>% 
  filter(PLI_count > 0) %>% 
  summarize(count = n())

bin_dis_GIS1 %>% 
  reframe(r = quantile(PLI_percent))

bin_dis_GIS1 %>% 
  filter(FDI_count < 1) %>% 
  filter(PLI_count > 0) %>% 
#  group_by(SampleSite_ID) %>% 
  summarize(count = n())

ggplot(bin_dis_GIS1, aes(x=FDI_percent)) +
  facet_grid(vars(as.factor(FDI_count_bin))) +
  geom_histogram(bins = 5)

bin_dis_GIS1 %>% 
  group_by(PLI_count_bin) %>% 
  summarize(count = n ())

bin_dis_GIS1 %>% 
  filter(years_since > 9) %>% 
  mutate(x30 = ifelse(PLI_x30_bin==1 | PLI_x130_bin ==1,1,0)) %>% 
  mutate(x130 = ifelse(PLI_x130_bin==1,1,0)) %>% 
  group_by(x30) %>% 
  summarize(count = n())

overstory %>% 
  filter(Card %in% c(369,425,758,766,1109,1362,1479,1816,1835,2113,2158,2210)) %>%  
  print(n=25)

site %>% 
  filter(Card %in% c(369,425,758,766,1109,1362,1479,1816,1835,2113,2158,2210)) 

#2113, 1109, 1479, 1816, 2158, 2210, 758, 1362, 369, 425, 1835, 766


library(ggeffects)
library(tidyverse)
library(gridExtra)

#structure: 
#  species % (FDI and PLI)

ggpredict(structure.PLI.5a, terms = c( "BARC.x")) %>%
  plot(xlab= ) +
  labs(title = "Lodgepole pine",
      x = "Fire Severity",
      y = "Predicted Probability")

ggpredict(structure.FDI.3j, terms = c("BARC.x")) %>% 
  plot()

bin_dis_GIS1 %>% 
  filter(PLI_count_bin > 0) %>%
  summarize(
    rangeDistance = list(range(Distance)),
    rangeFDI = list(range(PLI_percent)),
  )


#distance (FDI)
ggpredict(structure.FDI.3j, c("Distance", "years_since")) %>% 
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

PLI_percent <- ggpredict(structure.PLI.5a, "PLI_percent") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  scale_x_continuous(labels = ~ paste0(.x, "%"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Lodgepole pine",
       x = "Pre-fire Lodgepole Pine Basal Area Composition", y ="Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

FDI_percent <- ggpredict(structure.FDI.3j, c("FDI_percent")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(aes(color = "Model Fit"), se = FALSE) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high, , fill = "95% CI"), alpha=.5) +
  scale_x_continuous(labels = ~ paste0(.x, "%"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Douglas-fir",x = "Pre-fire Douglas-fir Basal Area Composition", y = "Predicted Probability") +
  scale_color_manual(name = "Legend", values = "blue", labels = "Model Prediction") + # Line color
  scale_fill_manual(name = "Legend", values = "darkgrey", labels = "95% Confidence Interval") + # Ribbon color
  theme_classic() +
  theme(legend.position="none") +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

FDI_distance <- 
  ggpredict(structure.FDI.3j, "Distance") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(aes(color = "Model Fit"), se = FALSE) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high, , fill = "95% CI"), alpha=.5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Douglas-fir",x = "Distance to Live Tree (m)", y = "Predicted Probability") +
  scale_color_manual(name = "", values = "blue") + #, labels = "Model Prediction") + 
  scale_fill_manual(name = "", values = "darkgrey") + #, labels = "95% Confidence Interval") + 
  theme_classic()  +
  theme(axis.text = element_text(size=18),
                    axis.title = element_text(size=18),
                    title = element_text(size=18))#,
    #legend.text = element_text(size=12),
     #       legend.box.background = element_rect(),
      #      legend.box.margin = margin(.5,.5,.5,.5),
      #                                 legend.position="right")


bin_dis_GIS1 %>% 
  filter(is.na(BARC.x)) %>% 
  select(SampleSite_ID)

FDI_BARC <- ggpredict(structure.FDI.3j, c("BARC.x")) %>%
  mutate(x = factor(x, levels = c(1, 2, 3, 4),
                    labels = c("Unburned", "Low", "Medium", "High"))) %>%
  ggplot(mapping = aes(x = x, y = predicted)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "grey", size = 4) +
  geom_point(color = "blue", size=4) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Fire Severity", y = "Predicted Probability", title = "Douglas-fir") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))



PLI_BARC <- ggpredict(structure.PLI.5a, c("BARC.x"))  %>%
  mutate(x = factor(x, levels = c(1, 2, 3, 4),
                    labels = c("Unburned", "Low", "Medium", "High"))) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high), color="grey", size=4) +
  geom_point(color="blue", size = 4) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Fire Severity", y = "Predicted Probability", title = "Lodgepole Pine") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))


legend <- get_legend(FDI_distance + theme(legend.box.margin = margin(5, 5, 5, 5),
                                          legend.box.background = element_rect(color = "black", linewidth = 0.5)))

FDI_distance <- FDI_distance + theme(legend.position = "none")

tiff("C:/Users/nmac2000/OneDrive - UBC/Figures/structure.tif",width = 7.48, height = 5, units = "in", res = 2244)
plot_grid(
  PLI_percent, FDI_percent, 
  FDI_distance, legend,
  labels = c("A", "B", "C", ""),
  ncol = 2, nrow = 2,
  rel_widths = c(1, 1, 0.5) # Legend takes less space
)
dev.off()


  
#site:
###########
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
  labs(title = "Predicted Probability",
       x = "MCMT", y = "Predicted Probability") +
  theme_classic()


###########

FDI_soil <- ggpredict(site.FDI.3k, c("PARENT_SOILS")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high), color="grey", size=4) +
  geom_point(color="blue", size = 4) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Parent Soil", y = "Predicted Probability", title = "Douglas-fir") +
#  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

FDI_BEC <- ggpredict(site.FDI.3k, c("BEC_Subzone")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high), color="grey", size=4) +
  geom_point(color="blue", size=4) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "BEC Subzone", y = "Predicted Probability", title = "Douglas-fir") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

#grid.arrange(FDI_MCMT, FDI_percent, FDI_soil, FDI_distance)

#PLI
PLI_BARC <- ggpredict(structure.PLI.5a, "BARC.x") %>% 
  ggplot(mapping = aes(x=x, y=predicted)) +
#  geom_smooth(se=F) +
  geom_point() +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete(labels=c("Unburned", "Low", "Medium", "High")) +
  labs(x = "Fire Severity", y = "Predicted Probability") +
  theme_classic()

PLI_BEC <- ggpredict(site.PLI.2j, "BEC_Subzone") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high), color="grey", size=4) +
  geom_point(colour="blue", size = 4) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Lodgepole pine", x = "BEC Subzone", y="Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")



PLI_slope <- ggpredict(site.PLI.2j, "Slope") %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  scale_x_continuous(labels = ~ paste0(.x, "%"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Lodgepole pine",
       x = "Slope", y = "Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

#grid.arrange(PLI_percent, PLI_BEC, PLI_BARC)

tiff("C:/Users/nmac2000/OneDrive - UBC/Figures/site1.tif",width = 7.48, height = 5, units = "in", res = 2244)

plot_grid(PLI_BEC,PLI_slope, FDI_BEC,  labels = c("A", "B", "C"),
          ncol = 3, nrow = 2,         # Three columns, two rows
          rel_widths = c(1, 1, 1),  # Make the legend smaller in width
          rel_heights = c(1),      # Equal heights for rows
          align = "hv",               # Align horizontally and vertically
          axis = "tblr" )
dev.off()

tiff("C:/Users/nmac2000/OneDrive - UBC/Figures/site2.tif",width = 7.48, height = 2.5, units = "in", res = 2244)

plot_grid( FDI_soil, legend,  labels = c( "D", ""),
          ncol = 2, nrow = 1,
          rel_widths = c(.7,1),# Equal heights for rows
          align = "hv",               # Align horizontally and vertically
          axis = "tblr" )
dev.off()


# Climate
# FDI
predict_response(climate.FDI.2f, "MCMT")
predict_response(climate.FDI.2f, "PAS")  
predict_response(climate.FDI.2f, "CMI_sm") 

str(bin_dis_GIS1$PAS)

bin_dis_GIS1 %>% 
#  filter(FDI_count_bin > 0) %>%
  summarize(
    rangePAS = list(range(PAS)),
    rangeMCMT = list(range(MCMT)),
    rangeCMI = list(range(CMI_sm))
  )


FDI_MCMT <- ggpredict(climate.FDI.2f, c("MCMT")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  geom_vline(xintercept = c(-6, -8.6), color = "red", linetype = "dashed") +
  scale_x_continuous(labels = ~ paste0(.x, "°"))+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Douglas-fir",x = "Mean Coldest Month Temp", y = "Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

FDI_PAS <- ggpredict(climate.FDI.2f, c("PAS")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  geom_vline(xintercept = c(75, 169), color = "red", linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Douglas-fir",x = "Precipitation as Snow", y = "Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

FDI_CMI <- ggpredict(climate.FDI.2f, c("CMI_sm")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F, method="loess") +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  geom_vline(xintercept = c(-32.18, -17.72), color = "red", linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Douglas-fir",x = "Summer Climate Moisture Index",y = "Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

# PLI
predict_response(climate.PLI.2d, "NFFD_sp")
predict_response(climate.PLI.2d, "cool.wet.anomalies.MCMT")  

bin_dis_GIS1 %>% 
  filter(PLI_count_bin > 0) %>%
  summarize(
    rangeNFFD = list(range(NFFD_sp)),
    rangeMCMT = list(range(cool.wet.anomalies.MCMT))
  )
 

PLI_MCMT <- ggpredict(climate.PLI.2d, c("cool.wet.anomalies.MCMT [all]")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  geom_vline(xintercept = c(0, 2), color = "red", linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Lodgepole pine", subtitle = "Mean Coldest Month Temperature",
       x = "Number of Anomalous Years",y = "Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

PLI_NFFD <- ggpredict(climate.PLI.2d, c("NFFD_sp [all]")) %>% 
  ggplot(mapping = aes (x=x, y=predicted)) +
  geom_smooth(se=F) +
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high), alpha=.2) +
  geom_vline(xintercept = c(11, 30), color = "red", linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Lodgepole pine",x = "# of Frost Free Days (March-May)", y = "Predicted Probability") +
  theme_classic() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        title = element_text(size=18))#,
#legend.text = element_text(size=12),
#       legend.box.background = element_rect(),
#      legend.box.margin = margin(.5,.5,.5,.5),
#                                 legend.position="right")

tiff("C:/Users/nmac2000/OneDrive - UBC/Figures/climate.tif",
     width = 7.48, height = 5, units = "in", res = 2244)
plot_grid(PLI_NFFD, PLI_MCMT, FDI_MCMT, FDI_CMI, FDI_PAS, legend,
          labels=c("A", "B", "C", "D", "E", ""), ncol = 2, nrow = 3, 
          hjust = -2.5)
dev.off()



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



library(corrplot)

St <- bin_dis_GIS1 %>% 
  select(Distance, PATCH_SIZE_1,  DISTANCE_TO_PERIMETER, FDI_percent, PLI_percent)
Si <- bin_dis_GIS1 %>% 
  select(Aspect, Elevation, Slope, Solar_Radiation, TWI)
Cl <- bin_dis_GIS1 %>% 
  select(MWMT, MCMT, Tmax_sm, MAP, PAS, PPT_sm, NFFD_sp, AHM, SHM, CMI_sm)

M <- cor(St)
M <- cor(Si)
M <- cor(Cl)
corrplot(M, method = 'number')
