
fire_severity2024 <- read.csv("C:/Users/nmac2000/Documents/regen project/Regen/Data/fire_severity_2024plots.csv")

regen_combo$SampleSite_ID <- as.integer(regen_combo$Card, na.rm=TRUE)
regen_GIS <- left_join(regen_combo,GIS_i, by = "SampleSite_ID") %>% 
  ungroup()


fire_severity2024 <- fire_severity2024 %>% 
  select(Plot_ID, MEAN) %>% 
  rename(SampleSite_ID = Plot_ID)

# Fix some fire severity values that were close to other plots
new_rows <- data.frame(
  SampleSite_ID = c(1153, 1182, 1500, 1501, 1502, 1503, 1531, 1192, 1484, 1504),
  MEAN = c(
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 2626)],
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 2644)],
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 1760)],
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 1761)],
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 1770)],
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 1771)],
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 1791)],
    fire_severity$MEAN[which(fire_severity$SampleSite_ID == 2644)],
    mean(c(0.485442, 0.462872, 0.575654, 0.470040)),
    mean(c(0.356386, 0.246404))
  )
)

# Combine the new rows with the existing dataframe
fire_severity <- bind_rows(fire_severity, new_rows)


fire_severity2024 <- within(fire_severity2024, {   
  BARC.x <- NA # need to initialize variable
  BARC.x[MEAN < .105] <- 1
  BARC.x[MEAN >= .105 & MEAN < 0.275] <- 2
  BARC.x[MEAN >= 0.275 & MEAN < 0.66] <- 3
  BARC.x[MEAN >= 0.66] <- 4
} )

fire_severity$SampleSite_ID <- as.character(fire_severity$SampleSite_ID)
fire_severity_all <- bind_rows(fire_severity, fire_severity2024) %>% 
  rename(dNBR = MEAN) %>% 
  rename(BARC = BARC.x)
write.csv(fire_severity_all, "C:/Users/nmac2000/Documents/regen project/Regen/Data/fire_severity_all.csv")

table(fire_severity_all$BARC)

regen_GIS <- full_join(regen_GIS, fire_severity, by = "SampleSite_ID") 


regen_GIS %>% filter(is.na(MEAN))