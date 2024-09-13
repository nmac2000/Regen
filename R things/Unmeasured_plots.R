#########################################
# Need to filter out unsampled plots    #
#########################################

# Will be filtering this dataframe first
bin_dis_GIS

# This is the list based off of the site notes
not_measured_list <-c(2042, 1840, 1701, 2208, 1609, 2646, 1598, 1942, 2043, 1623, 2269, 2115, 1482, 2213, 2334,
                               1936, 1724, 193, 318, 1209, 1267, 499, 1836, 361, 545, 907,
                               1467, 1297, 1423, 1831, 585, 1796, 2271, 1807, 865, 315)



not_measured <- regen_GIS %>% 
  filter(SampleSite_ID %in% not_measured_list) %>% 
  select(SampleSite_ID)
print(not_measured) 

######################################
# And now add values to missing dNBR #
######################################


bin_dis_GIS1 <- bin_dis_GIS1 %>% 
  mutate(MEAN = ifelse(SampleSite_ID == 1153, MEAN[SampleSite_ID == 2626] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1182, MEAN[SampleSite_ID == 2644] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1500, MEAN[SampleSite_ID == 1760] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1501, MEAN[SampleSite_ID == 1761] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1502, MEAN[SampleSite_ID == 1770] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1503, MEAN[SampleSite_ID == 1771] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1531, MEAN[SampleSite_ID == 1791] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1192, MEAN[SampleSite_ID == 2644] ,MEAN),
         MEAN = ifelse(SampleSite_ID == 1484, mean(c(0.485442, 0.462872, 0.575654, 0.470040)),MEAN),
         MEAN = ifelse(SampleSite_ID == 1504, mean(c(0.356386, 0.246404)),MEAN))


bin_dis_GIS1 %>% 
  filter(SampleSite_ID %in% c(1153, 2626, 1182, 2644, 1500, 1760, 1501, 1761)) %>% 
  select(SampleSite_ID, MEAN)















