regen_combo_STM  # all regen data

GIS_2023 <- GIS_i %>%    #2023 GIS data
  select(-Card, -UNIQUE_ID, -Beers_aspect, -Azimuth)

GIS_2024 <- VRI_read_2024 %>%  #2024 GIS data
  select(-UNIQUE_ID) %>% 
  rename(SampleSite_ID = Plot_ID)

GIS_all <- rbind(GIS_2023, GIS_2024) #all GIS data

clim.all # 2023 climate data
write.csv(clim.all, "C:\\Users\\nmac2000\\Documents\\regen project\\For Anya\\Climate.csv")


distance$SampleSite_ID <- as.character(distance$SampleSite_ID) # 2023 distance
fire_severity_all$SampleSite_ID <- as.character(fire_severity_all$SampleSite_ID) # all fire severity data

almostfulldata <- left_join(regen_combo_STM, GIS_all, by="SampleSite_ID")

almostdistance <- left_join(almostfulldata, distance, by = "SampleSite_ID")

fulldata <- left_join(almostdistance, fire_severity_all, by = "SampleSite_ID")

fulldata$UsedForCh3 <- ifelse(fulldata$SampleSite_ID %in% 
                                as.character(bin_dis_GIS1$SampleSite_ID), "yes", "no")
fulldata$UsedForCh2 <- ifelse(fulldata$SampleSite_ID %in% 
                                as.character(VRI_all$SampleSite_ID), "yes", "no")

write.csv(fulldata, "C:\\Users\\nmac2000\\Documents\\regen project\\For Anya\\Data.csv")

notused <- fulldata %>% 
  filter(UsedForCh2 == "no") %>% 
  filter(UsedForCh3 == "no")

write.csv(notused, "C:\\Users\\nmac2000\\Documents\\regen project\\For Anya\\NotUsed.csv")

not_used_list <- as.list(notused$SampleSite_ID)

not_measured_list
