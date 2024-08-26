#########################################
# Need to filter out unsampled plots    #
#########################################

# Will be filtering this dataframe first
bin_dis_GIS

# This is the list based off of the site notes
not_measured_list <-c(2042, 1840, 1701, 2208, 1609, 2646, 1598, 1942, 2043, 1623, 2269, 2115, 1482, 2213, 2334,
                               1936, 1724, 193, 318, 1209, 847, 1267, 499, 1836, 361, 1080, 545, 2253, 162, 161, 907,
                               1467, 1297, 1423, 1831, 1413, 585, 1796, 1319, 2271, 1807, 865, 315)

not_measured <- regen_GIS %>% 
  filter(SampleSite_ID %in% not_measured_list,
         total_count == 0) %>% 
  select(SampleSite_ID) 
not_measured <- as.list(not_measured)

