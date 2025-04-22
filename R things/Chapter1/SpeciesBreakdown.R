# Looking at percentages and what not

PLI_VRI <- VRI_all %>% 
  filter(LeadingSpecies == "PLI")

FDI_VRI <- VRI_all %>% 
  filter(LeadingSpecies == "FDI")

SX_VRI <- VRI_all %>% 
  filter(LeadingSpecies == "SX")

AT_VRI <- VRI_all %>% 
  filter(LeadingSpecies == "AT")

MIX_VRI <- VRI_all %>% 
  filter(Dominant == "Mix")

ggplot(MIX_VRI, aes(x=LeadingSpecies,SecondSpecies)) +
  geom_count()


table(regen_percents$Species)


MIX_VRI %>% 
  filter(LeadingSpecies == "AT") %>% 
  select(LeadingSpecies:FourthPercent)
