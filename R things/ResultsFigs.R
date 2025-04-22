
# For results
ggplot(bin_dis_GIS_bec, aes(x=Longitude, y=Latitude)) +
  geom_point(aes(shape = as.factor(outlier), colour = BEC_Subzone), size = 3) +
  theme_classic()

table(bin_dis_GIS1$BEC_Subzone, bin_dis_GIS1$BEC_Zone)

tiff("C:/Users/nmac2000/OneDrive - UBC/Figures/BECandMCMT.tif",width = 3.543, height = 2.1, units = "in", res = 300)
ggplot(bin_dis_GIS1,aes(x=BEC_Subzone, y=MCMT)) +
  geom_boxplot(outlier.color=NA, linewidth = .25)+
  geom_point(aes(colour = as.factor(FDI_count_bin)), 
             position = position_jitter(width = .35), alpha = .5, 
             size = 1.15, shape = 19)  +
  scale_color_manual(values=c("#BD9457","#41A83E")) +
  scale_y_continuous(labels = ~ paste0(.x, "°"))+
  labs(x= "BEC Subzone", y="MCMT", 
       colour = "FDI Occurrence", title = "BEC Subzone vs MCMT") +
  theme_classic() +
  theme(axis.text = element_text(size = 7), 
    axis.title = element_text(size = 7),
    title = element_text(size=7),
    legend.text = element_text(size=7),
    legend.box.background = element_rect(),
    legend.box.margin = margin(.5,.5,.5,.5)
)
dev.off()

bin_dis_GIS1 %>% 
  mutate(FDI = ifelse(FDI_count>6,1,0)) %>% 
ggplot(aes(x=yhat.FDI, y=FDI_count)) +
  geom_point(aes(color=FDI))+
#  ylim(0,1500) +
#  xlim(.5,1) +
  theme_classic() 

bin_dis_GIS1 %>% 
  group_by(PARENT_SOILS) %>% 
  summarize(count = n())

bin_dis_GIS1 %>% 
  mutate(FDI = ifelse(FDI_count>6,1,0)) %>%
  filter(yhat.FDI>.5) %>% 
  group_by(FDI) %>% 
  summarize(count=n())
  

ggplot(bin_dis_GIS1, aes(x=yhat.PLI, y=yhat.FDI)) +
  geom_point(aes(color=as.factor(FDI.f))) +
  theme_minimal()

ggplot(bin_dis_GIS1, aes(x=yhat.PLI, y=yhat.FDI)) +
  geom_point(aes(color=as.factor(PLI.f))) +
  theme_minimal()


