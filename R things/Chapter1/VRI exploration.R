###########################
# Early Chapter 1 Attempts#
###########################

table(bin_dis_GIS1$SPECIES_CD_1_1, bin_dis_GIS1$SPECIES_CD_2_1)

bin_GIS %>% 
  filter(Card == 1942) %>% 
  select(FIRE_YEAR_1)

VRI <- bin_dis_GIS1 %>% 
  select(SampleSite_ID, SPECIES_CD_1_1:SPECIES_PCT_4_1, SpeciesAll:SX_x130, total_count_bin:SX_x130_bin ) %>% 
  select(-PROJ_HEIGHT_1_1,-PROJ_AGE_1_1,-PROJ_HEIGHT_2_1,-PROJ_AGE_2_1)


ggplot(bin_dis_GIS1, aes(x=as.factor(years_since), y=PLI_x130)) +
  geom_boxplot(outlier.color=NA)+
  geom_jitter(width =.25)  +
  theme_classic()

ggplot(bin_dis_GIS1, aes(x=as.factor(years_since), y=PLI_x30)) +
  geom_boxplot(outlier.color=NA)+
  geom_jitter(width =.25)  +
  theme_classic()

ggplot(bin_dis_GIS1, aes(x=as.factor(years_since), y=PLI_x0)) +
  geom_boxplot(outlier.color=NA)+
  geom_jitter(width =.25)  +
  theme_classic()

ggplot(bin_dis_GIS1, aes(x=as.factor(years_since), y=PLI_x10)) +
  geom_boxplot(outlier.color=NA)+
  geom_jitter(width =.25)  +
  theme_classic()


bin_dis_GIS1 %>% 
  filter(SampleSite_ID == 812) %>% 
  select(total_count:SampleSite_ID, SPECIES_CD_1_1:SPECIES_PCT_4_1)
