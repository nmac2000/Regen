### Looking at data for writeup ###
# October 18
# Nat MacMillan

bin_dis_GIS1 %>% 
  group_by(FIRE_NUMBER_1, total_count_bin) %>% 
  summarise(total = n())

