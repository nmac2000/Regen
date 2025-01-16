
# For results
ggplot(bin_dis_GIS_bec, aes(x=Longitude, y=Latitude)) +
  geom_point(aes(shape = as.factor(outlier), colour = BEC_Subzone), size = 3) +
  theme_classic()

table(bin_dis_GIS1$BEC_Subzone, bin_dis_GIS1$PLI.f)

filter(bin_dis_GIS1, years_since >= 10) %>% 
  filter(FDI_count_bin > 0) %>% 
  group_by(SampleSite_ID) %>% 
  mutate(PLI_tall = PLI_x30_bin + PLI_x130_bin) %>% 
  mutate(FDI_tall = FDI_x30_bin + FDI_x130_bin) %>%
  ungroup() %>% 
  group_by(FDI_tall) %>% 
  summarize(n = n())


ggplot(bin_dis_GIS_bec,aes(x=BEC_Subzone, y=MCMT)) +
  geom_boxplot(outlier.color=NA)+
  geom_jitter(width =.25, aes(shape=as.factor(FDI_count_bin), colour=as.factor(FDI_count_bin)), size=2.2) +
  scale_color_manual(values=c("#999999", "#E69F00")) +
  scale_y_continuous(labels = ~ paste0(.x, "°"))+
  labs(x= "BEC Subzone", y="MCMT", shape = "FDI Occurrence", 
       colour = "FDI Occurrence", title = "BEC Subzone vs MCMT") +
  theme_classic()

ggplot(bin_dis_GIS1, aes(x=as.factor(PLI_count_bin), y=PLI_percent)) +
         geom_boxplot(outlier.color=NA)+
         geom_jitter(width =.25) +
         theme_classic() 

bin_dis_GIS_bec <- bin_dis_GIS1 %>% 
  mutate(outlier = ifelse(BEC_Zone == "IDF" & MCMT <= -7, 1, 0)) %>% 
  mutate(outlier = ifelse(BEC_Zone == "SBPS" & MCMT >= -7, 2, outlier))

ggpredict(FDI.8, c("MCMT")) %>% 
  plot()

ggpredict(FDI.8, c( "BEC_Subzone")) %>% 
  plot()
