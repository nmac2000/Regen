#####################################
# Fire severity x species dominance #
#####################################
library(CNPS)
library(tidyverse)

STM <- read.csv("C:/Users/nmac2000/Desktop/Chapter2/data/STM.csv")
fulldata <- read.csv("C:/Users/nmac2000/Documents/regen project/For Anya/Data.csv")
fire_severity_all <- read.csv("C:/Users/nmac2000/Documents/regen project/Regen/Data/fire_severity_all.csv")


# Fire severity and BEC subzones for each plot, and filter for just 2017 fires
fire_severity_BEC <- fulldata %>% 
  filter(FIRE_YEAR_1 == 2017) %>% 
  select(SampleSite_ID, BEC_Zone, BEC_Subzone, BARC)

# Fire severities by BEC zone and subzone
table(fire_severity_BEC$BEC_Zone, fire_severity_BEC$BARC)
table(fire_severity_BEC$BEC_Subzone, fire_severity_BEC$BARC)

# Now we add plot dominance shifts
## this is where I am distinguishing between a plot having no change (dominant for that species pre and post, or not dominant for
# that species pre and post) and making a difference between if it was that species of interest or not
dominance <- STM %>% 
  filter(SampleSite_ID %in% fire_severity_BEC$SampleSite_ID) %>% 
  select(SampleSite_ID, Dominant_pre, Dominant, PLI_percent_pre:AT_percent_pre, PLI_percent:AT_percent)

dominance_shifts <- dominance %>%
  mutate(FDI_shifts = case_when(
          Dominant_pre == "FDI" & Dominant != "FDI" ~ "lost",
          Dominant_pre != "FDI" & Dominant == "FDI" ~ "gain",
          Dominant_pre == "FDI" & Dominant == "FDI" ~ "no_change_FDI",
          Dominant_pre != "FDI" & Dominant != "FDI" ~ "no_change_other"
    )) %>% 
  mutate(PLI_shifts = case_when(
        Dominant_pre == "PLI" & Dominant != "PLI" ~ "lost",
        Dominant_pre != "PLI" & Dominant == "PLI" ~ "gain",
        Dominant_pre == "PLI" & Dominant == "PLI" ~ "no_change_PLI",
        Dominant_pre != "PLI" & Dominant != "PLI" ~ "no_change_other"
      )) %>% 
  mutate(SX_shifts = case_when(
        Dominant_pre == "SX" & Dominant != "SX" ~ "lost",
        Dominant_pre != "SX" & Dominant == "SX" ~ "gain",
        Dominant_pre == "SX" & Dominant == "SX" ~ "no_change_SX",
        Dominant_pre != "SX" & Dominant != "SX" ~ "no_change_other"
      )) %>% 
  mutate(AT_shifts = case_when(
        Dominant_pre == "AT" & Dominant != "AT" ~ "lost",
        Dominant_pre != "AT" & Dominant == "AT" ~ "gain",
        Dominant_pre == "AT" & Dominant == "AT" ~ "no_change_AT",
        Dominant_pre != "AT" & Dominant != "AT" ~ "no_change_other"
      ))          


# dominance shifts across fire severities
dominance_fire_BEC <- left_join(dominance_shifts, fire_severity_BEC, by = "SampleSite_ID")

dominance_fire_BEC <- distinct(dominance_fire_BEC, SampleSite_ID, .keep_all = TRUE)

FDI_contingency <- as.data.frame.matrix(table(dominance_fire_BEC$FDI_shifts, dominance_fire_BEC$BARC))
PLI_contingency <- as.data.frame.matrix(table(dominance_fire_BEC$PLI_shifts, dominance_fire_BEC$BARC))
SX_contingency <- as.data.frame.matrix(table(dominance_fire_BEC$SX_shifts, dominance_fire_BEC$BARC))
AT_contingency <- as.data.frame.matrix(table(dominance_fire_BEC$AT_shifts, dominance_fire_BEC$BARC))

# Chi-squared tests, but assumptions are violated for all but PLI
# use chisq.test()$expected and chisq.test()$observed to check assumptions 
chisq.test(FDI_contingency)
chisq.test(PLI_contingency)
chisq.test(SX_contingency)
chisq.test(AT_contingency)

# Permutation tests
permu_table(FDI_contingency) 
permu_table(PLI_contingency)
permu_table(SX_contingency)
permu_table(AT_contingency)


