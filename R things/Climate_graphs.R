##############################
# Graphs for climate variables
# Nat
# October 23, 2024
#############################

# clim.all <- read.csv(on github)
library(tidyverse)

clim.all <- rename(clim.all, SampleSite_ID = ID1)
messy <- left_join(clim.all, bin_dis_GIS1, by = "SampleSite_ID")
messy$PLI_count_bin <- as.factor(messy$PLI_count_bin)
messy$FDI_count_bin <- as.factor(messy$FDI_count_bin)
messy$ID2 <- as.factor(messy$ID2)
messy$Year <- as.factor(messy$Year)
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

boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy9,
        col=colors, 
        xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2009 - PLI")

points(jitter(as.numeric(interaction(messy9$Year, messy9$PLI_count_bin))), messy9$MAT, 
       col=rgb(0, 0, 0, 0.7), 
       pch=16, 
       cex=.8)

boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy10,
        col=colors, 
        xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2010 - PLI")

points(jitter(as.numeric(interaction(messy10$Year, messy10$PLI_count_bin))), messy10$MAT, 
       col=rgb(0, 0, 0, 0.7), 
       pch=16, 
       cex=.8)

boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy15,
        col=colors, 
        xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2015 - PLI")

points(jitter(as.numeric(interaction(messy15$Year, messy15$PLI_count_bin))), messy15$MAT, 
       col=rgb(0, 0, 0, 0.7), 
       pch=16, 
       cex=.8)


boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy17,
        col=colors, 
        xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT 2017 - PLI")

points(jitter(as.numeric(interaction(messy17$Year, messy17$PLI_count_bin))), messy17$MAT, 
       col=rgb(0, 0, 0, 0.5), 
       pch=16, 
       cex=.8)

boxplot(MAT ~ interaction(Year, PLI_count_bin), data=messy,
        col=colors, 
        xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT All - PLI")

boxplot(MAT ~ interaction(Year, FDI_count_bin), data=messy,
        col=colors, 
        xlab="PLI Presence and Year after Fire", ylab="MAT", main="MAT All - FDI")



  