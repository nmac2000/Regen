
# For results
ggplot(bin_dis_GIS_bec, aes(x=Longitude, y=Latitude)) +
  geom_point(aes(shape = as.factor(outlier), colour = BEC_Subzone), size = 3) +
  theme_classic()

table(bin_dis_GIS1$BEC_Subzone, bin_dis_GIS1$PLI.f)


ggplot(bin_dis_GIS1,aes(x=BEC_Subzone, y=MCMT)) +
  geom_boxplot(outlier.color=NA)+
  geom_jitter(width =.25, aes( colour=as.factor(FDI_count_bin)),shape=19) +
  scale_color_manual(values=c("#ED5151","#149ECE")) +
  scale_y_continuous(labels = ~ paste0(.x, "°"))+
  labs(x= "BEC Subzone", y="MCMT", 
       colour = "FDI Occurrence", title = "BEC Subzone vs MCMT") +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
    axis.title = element_text(size = 16),
    title = element_text(size=16),
    legend.text = element_text(size=16))

bin_dis_GIS1 %>% 
  mutate(FDI = ifelse(FDI_count>6,1,0)) %>% 
ggplot(aes(x=yhat.FDI, y=FDI_count)) +
  geom_point(aes(color=FDI))+
#  ylim(0,1500) +
#  xlim(.5,1) +
  theme_classic() 



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


