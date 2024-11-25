##############################
# Graphs for climate variables
# Nat
# October 23, 2024
#############################

# clim.all <- read.csv(on github)
library(tidyverse)
library(gridExtra)

#clim.all <- rename(clim.all, SampleSite_ID = ID1)
messy <- left_join(zscores, bin_dis_GIS, by = "SampleSite_ID")
messy$PLI_count_bin <- as.factor(messy$PLI_count_bin)
messy$FDI_count_bin <- as.factor(messy$FDI_count_bin)
messy$ID2 <- as.factor(messy$ID2)
messy$Year <- as.factor(messy$ID2)
messy <- messy %>% 
  filter(Year != 0)

colors <- c("0" = "black", "1" = "red", "2"= "blue", "3" = "green", "4" = "orange", "5" = "purple")


messy9 <- messy %>% 
  filter(ID2 == 2009)
messy15 <- messy %>% 
  filter(ID2 == 2015)
messy10 <- messy %>% 
  filter(ID2 == 2010)
messy16 <- messy %>% 
  filter(ID2 == 2016)
messy17 <- messy %>% 
  filter(ID2 == 2017)


plot1a <- ggplot(messy, aes(x=PLI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="PLI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm All - PLI", subtitle = "Cool and Wet Anomalies")

plot2a <- ggplot(messy, aes(x=FDI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="FDI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm All - FDI", subtitle = "Cool and Wet Anomalies")


plot1b <- ggplot(messy9, aes(x=PLI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="PLI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2009 - PLI", subtitle = "Cool and Wet Anomalies")

plot2b <- ggplot(messy10, aes(x=PLI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="PLI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2010 - PLI", subtitle = "Cool and Wet Anomalies")

plot3b <- ggplot(messy15, aes(x=PLI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="PLI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2015 - PLI", subtitle = "Cool and Wet Anomalies")

plot4b <- ggplot(messy17, aes(x=PLI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="PLI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2017 - PLI", subtitle = "Cool and Wet Anomalies")




plot1c <- ggplot(messy9, aes(x=FDI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="FDI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2009 - FDI", subtitle = "Cool and Wet Anomalies")

plot2c <- ggplot(messy10, aes(x=FDI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="FDI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2010 - FDI", subtitle = "Cool and Wet Anomalies")

plot3c <- ggplot(messy15, aes(x=FDI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="FDI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2015 - FDI", subtitle = "Cool and Wet Anomalies")

plot4c <- ggplot(messy17, aes(x=FDI_count_bin, y=cool.wet.anomalies.CMI_sm)) +
  geom_boxplot() +
  geom_jitter(width =.25, height=.15) +
  coord_cartesian(ylim = c(0,5)) +
  theme_classic() +
  labs(x="FDI Occurrence", y = "Number of Anomalous Years",
       title = "CMI_sm 2017 - FDI", subtitle = "Cool and Wet Anomalies")

grid.arrange(plot1c, plot2c, plot3c, plot4c, ncol = 2)
grid.arrange(plot1b, plot2b, plot3b, plot4b, ncol = 2)
grid.arrange(plot1a,plot2a, ncol=2)













# 
# 
# boxplot(cool.wet.anomalies.CMI_sm ~ interaction(PLI_count_bin), data=messy17,
#         col=colors, 
#         xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2009 - PLI")
# 
# points(jitter(as.numeric(interaction(messy9$Year, messy9$PLI_count_bin))), messy9$MAT, 
#        col=rgb(0, 0, 0, 0.7), 
#        pch=16, 
#        cex=.8)
# 
# boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy10,
#         col=colors, 
#         xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2010 - PLI")
# 
# points(jitter(as.numeric(interaction(messy10$Year, messy10$PLI_count_bin))), messy10$MAT, 
#        col=rgb(0, 0, 0, 0.7), 
#        pch=16, 
#        cex=.8)
# 
# boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy15,
#         col=colors, 
#         xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2015 - PLI")
# 
# points(jitter(as.numeric(interaction(messy15$Year, messy15$PLI_count_bin))), messy15$MAT, 
#        col=rgb(0, 0, 0, 0.7), 
#        pch=16, 
#        cex=.8)
# 
# 
# boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy17,
#         col=colors, 
#         xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2017 - PLI")
# 
# points(jitter(as.numeric(interaction(messy17$Year, messy17$PLI_count_bin))), messy17$MAT, 
#        col=rgb(0, 0, 0, 0.5), 
#        pch=16, 
#        cex=.8)
# 
# boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy,
#         col=colors, 
#         xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT All - PLI")
# 
# boxplot(MAT ~ interaction(Year, FDI_count_bin), data=messy,
#         col=colors, 
#         xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT All - FDI")
# 
# 
# bin_dis_GIS1 %>% 
#   group_by(FIRE_NUMBER_1 ,FDI_count_bin) %>% 
#   summarize(n = n())
# 
# table(bin_dis_GIS1$FIRE_NUMBER_1)
# 
# ggplot(filter(bin_dis_GIS1, FDI_count <= 1000), aes(y=FDI_percent, x=FDI_count) )+
#  #geom_boxplot() +
#   geom_point( position = "jitter")







  