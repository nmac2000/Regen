library(Gmisc)
library(tidyverse)

fire_severity_all <- read.csv("C:\\Users\\nmac2000\\Documents\\regen project\\Data\\fire_severity_all.csv")

ggplot(bin_dis_GIS1, aes(x=Slope)) +
  geom_histogram()

bin_dis_GIS1 %>% 
  filter(BARC.x != 4) %>% 
  filter(FDI_count_bin == 0) %>% 
  select(FDI_count_bin, Distance) %>% 
  plot()


# Looking at percentages and what not
regen_percents

PLI_VRI <- VRI_all %>% 
  filter(LeadingSpecies_pre == "PLI")

FDI_VRI <- VRI_all %>% 
  filter(LeadingSpecies_pre == "FDI")

SX_VRI <- VRI_all %>% 
  filter(LeadingSpecies_pre == "SX")

AT_VRI <- VRI_all %>% 
  filter(LeadingSpecies_pre == "AT")

MIX_VRI <- VRI_all %>% 
  filter(Dominant_pre == "Mix")

MIX_regen <- regen_percents %>% 
  filter(Dominant == "Mix")

ggplot(MIX_VRI, aes(x=LeadingSpecies_pre,y = LeadingPercent_pre)) +
  geom_count()





MIX_VRI %>% 
  filter(LeadingSpecies == "AT") %>% 
  select(LeadingSpecies:FourthPercent)

# Pre and Post

STM <- full_join(VRI_all, regen_percents)
STM$Dominant[is.na(STM$Dominant)] <- "NR"
STM$LeadingSpecies[is.na(STM$LeadingSpecies)] <- "NR"
STM$LeadingPercent[is.na(STM$LeadingSpecies)] <- 0
STM <- STM %>%
  group_by(SampleSite_ID) %>% 
  mutate(Trans = ifelse(Dominant_pre == Dominant, "static", "shift"))

write.csv(STM, "C:/Users/nmac2000/Desktop/Chapter2/data/STM.csv")
STM <- read.csv("https://raw.githubusercontent.com/nmac2000/Regen/refs/heads/main/Data/Chapter1/STM.csv")
STM <- STM %>% 
  select(-X.1, -X)
ggplot(STM, aes(x=FDI_percent_pre, y=FDI_percent)) +
  geom_point()

ggplot(STM, aes(x=PLI_percent_pre, y=PLI_percent)) +
  geom_point()

ggplot(STM, aes(x=SX_percent_pre, y=SX_percent)) +
  geom_point()

ggplot(STM, aes(x=AT_percent_pre, y=AT_percent)) +
  geom_point()

# FDI transitions

STM_FDI <- STM %>% 
  mutate(FDI_pre = case_when(
    Dominant_pre == "FDI" ~ "Dominant",
    FDI_percent_pre < 80 & FDI_percent_pre >= 30 ~ "MixHighFDI",
    FDI_percent_pre < 30 & FDI_percent_pre > 0 ~ "MixLowFDI",
    FDI_percent_pre == 0 ~ "NoFDI")) %>% 
  mutate(FDI_post = case_when(
    Dominant == "FDI" ~ "Dominant",
    FDI_percent < 80 & FDI_percent >= 30 ~ "MixHighFDI",
    FDI_percent < 30 & FDI_percent > 0 ~ "MixLowFDI",
    FDI_percent == 0 ~ "NoFDI")) %>% 
  mutate(trans_FDI = case_when(
    FDI_pre != FDI_post ~ "shift", 
    FDI_pre == FDI_post ~ "static"
  ))

STM_FDI %>% 
#  filter(trans_FDI == "shift") %>% 
  filter(!(FDI_pre == "NoFDI" & FDI_post == "NoFDI")) %>% 
  group_by(trans_FDI, FDI_pre) %>% 
  summarize(count = n())

STM_FDI %>% 
  filter(FDI_pre == "NoFDI") %>% 
#  ggplot(aes(x=FDI_pre, y=FDI_post)) +
#  geom_count()
  select(FDI_pre, FDI_post) %>% 
  table()

test <- left_join(STM_SX, fire_severity_all, by = "SampleSite_ID")

table(test$trans_SX, test$BARC)

FDI_successTransFDI_succession <- table(STM_FDI$FDI_pre, STM_FDI$FDI_post)
transitionPlot(FDI_succession,type_of_arrow = "simple",
               fill_start_box =c("#373d20","#717744","#3e5c76",
                                 "#a3b9e0"),
               arrow_clr =c("#373d20","#717744","#3e5c76",
                            "#a3b9e0"),
               box_label = c("Pre-fire","Post-fire"),
               box_txt = c("Dominant", "Moderate", "Sparse", 
                           "Absent"),
               #main = "Aspen", 
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               new_page = T ,
               tot_spacing = .05,
               overlap_add_width = 1)

ggplot(STM_FDI, aes(x=Dominant, y=FDI_percent_pre)) +
  geom_boxplot(aes(color=Trans))

ggplot(STM_FDI, aes(x=trans_FDI)) +
  geom_bar(aes(color=FDI_pre, fill = FDI_pre))

# PLI transitions

STM_PLI <- STM %>% 
  mutate(PLI_pre = case_when(
    Dominant_pre == "PLI" ~ "Dominant",
    PLI_percent_pre < 80 & PLI_percent_pre >= 30 ~ "MixHighPLI",
    PLI_percent_pre < 30 & PLI_percent_pre > 0 ~ "MixLowPLI",
    PLI_percent_pre == 0 ~ "NoPLI")) %>% 
  mutate(PLI_post = case_when(
    Dominant == "PLI" ~ "Dominant",
    PLI_percent < 80 & PLI_percent >= 30 ~ "MixHighPLI",
    PLI_percent < 30 & PLI_percent > 0 ~ "MixLowPLI",
    PLI_percent == 0 ~ "NoPLI")) %>% 
  mutate(trans_PLI = case_when(
    PLI_pre != PLI_post ~ "shift", 
    PLI_pre == PLI_post ~ "static"
  ))


ggplot(STM_PLI, aes(x=PLI_pre, y=PLI_post)) +
  geom_count()

ggplot(STM_PLI, aes(x=trans_PLI)) +
  geom_bar(aes(color=PLI_pre, fill = PLI_pre))

STM_PLI %>% 
  filter(trans_PLI == "shift") %>% 
  select(PLI_pre, PLI_post) %>% 
  table()

test <- left_join(STM_PLI, fire_severity_all, by = "SampleSite_ID")

table(test$PLI_post, test$BARC)

PLI_succession <- table(STM_PLI$PLI_pre, STM_PLI$PLI_post)
transitionPlot(PLI_succession,type_of_arrow = "simple",
               fill_start_box =c("#373d20","#717744","#3e5c76",
                                 "#a3b9e0"),
               arrow_clr =c("#373d20","#717744","#3e5c76",
                            "#a3b9e0"),
               box_label = c("Pre-fire","Post-fire"),
               box_txt = c("Dominant", "Moderate", "Sparse", 
                            "Absent"),
               #main = "Aspen", 
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               new_page = T ,
               tot_spacing = .05,
               overlap_add_width = 1)
# SX transitions

STM_SX <- STM %>% 
  mutate(SX_pre = case_when(
    Dominant_pre == "SX" ~ "Dominant",
    SX_percent_pre < 80 & SX_percent_pre >= 30 ~ "MixHighSX",
    SX_percent_pre < 30 & SX_percent_pre > 0 ~ "MixLowSX",
    SX_percent_pre == 0 ~ "NoSX")) %>% 
  mutate(SX_post = case_when(
    Dominant == "SX" ~ "Dominant",
    SX_percent < 80 & SX_percent >= 30 ~ "MixHighSX",
    SX_percent < 30 & SX_percent > 0 ~ "MixLowSX",
    SX_percent == 0 ~ "NoSX")) %>% 
  mutate(trans_SX = case_when(
      SX_pre != SX_post ~ "shift", 
      SX_pre == SX_post ~ "static"
    ))

ggplot(STM_SX, aes(x=trans_SX)) +
  geom_bar(aes(color=SX_pre, fill = SX_pre))

STM_SX %>% 
  filter(trans_SX == "shift") %>% 
  select(SX_pre, SX_post) %>% 
  table()

ggplot(STM_SX, aes(x=SX_pre, y=SX_post)) +
  geom_count()

STM_SX %>% 
  filter(trans_SX == "shift") %>% 
  filter(SX_post == "NoSX") %>% 
  select(Dominant, SX_pre) %>% 
  table()



SX_succession <- table(STM_SX$SX_pre, STM_SX$SX_post)
transitionPlot(SX_succession,type_of_arrow = "simple",
               fill_start_box =c("#373d20","#717744","#3e5c76",
                                 "#a3b9e0"),
               arrow_clr =c("#373d20","#717744","#3e5c76",
                            "#a3b9e0"),
               box_label = c("Pre-fire","Post-fire"),
               box_txt = c("Dominant", "Moderate", "Sparse", 
                           "Absent"),
               #main = "Aspen", 
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               new_page = T ,
               tot_spacing = .05,
               overlap_add_width = 1)

# AT transitions

STM_AT <- STM %>% 
  mutate(AT_pre = case_when(
    Dominant_pre == "AT" ~ "Dominant",
    AT_percent_pre < 80 & AT_percent_pre >= 30 ~ "MixHighAT",
    AT_percent_pre < 30 & AT_percent_pre > 0 ~ "MixLowAT",
    AT_percent_pre == 0 ~ "NoAT")) %>% 
  mutate(AT_post = case_when(
    Dominant == "AT" ~ "Dominant",
    AT_percent < 80 & AT_percent >= 30 ~ "MixHighAT",
    AT_percent < 30 & AT_percent > 0 ~ "MixLowAT",
    AT_percent == 0 ~ "NoAT")) %>% 
  mutate(trans_AT = case_when(
    AT_pre != AT_post ~ "shift", 
    AT_pre == AT_post ~ "static"
  ))

ggplot(STM_AT, aes(x=AT_pre, y=AT_post, fill = AT_percent)) +
  geom_tile()

ggplot(STM_AT, aes(x=trans_AT)) +
  geom_bar(aes(color=AT_pre, fill = AT_pre))

STM_AT %>% 
  filter(AT_pre == "NoAT") %>% 
  select(AT_pre,AT_post) %>% 
  table()

STM_AT %>% 
  group_by(trans_AT, AT_pre) %>% 
  summarize(count = n())

AT_succession <- table(STM_AT$AT_pre, STM_AT$AT_post)
transitionPlot(AT_succession,type_of_arrow = "simple",
               fill_start_box =c("#373d20","#717744","#3e5c76",
                                 "#a3b9e0"),
               arrow_clr =c("#373d20","#717744","#3e5c76",
                            "#a3b9e0"),
               box_label = c("Pre-fire","Post-fire"),
               box_txt = c("Dominant", "Moderate", "Sparse", 
                           "Absent"),
               #main = "Aspen", 
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               new_page = T ,
               tot_spacing = .05,
               overlap_add_width = 1)

# Overall Transitions

STM <- STM %>% 
  mutate(State_pre = case_when(
    Dominant_pre == "AT" ~ "AT Dominant",
    Dominant_pre == "FDI" ~ "FDI Dominant",
    Dominant_pre == "PLI" ~ "PLI Dominant",
    Dominant_pre == "SX" ~ "SX Dominant",
    Dominant_pre == "Mix" ~ "Mix",
  ))



library(miscTools)


succession <- table(STM$Dominant_pre, STM$Dominant)
mat <- matrix(succession, nrow = nrow(succession), ncol = ncol(succession),
              dimnames = dimnames(succession))

succession <- insertRow(m=mat, r=4, v=0, rName="NR")

transitionPlot(succession,type_of_arrow = "simple",
               fill_start_box =c("#84a98c","#7c6a0a","cyan4","lightblue",
                                 "darkolivegreen", "#2f3e46"),
               arrow_clr =c("#84a98c","#7c6a0a","cyan4","lightblue",
                            "darkolivegreen", "#2f3e46"),
               box_label = c("Pre-fire","Post-fire"),
               box_txt = c("Aspen", "Douglas-fir", "Mixed Stands", 
                           "", "Lodgepole Pine", "Spruce"),
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
              new_page = T ,
              tot_spacing = .05,
              overlap_add_width = 1)

# mixed stand transitions

STM_MIX <- STM %>% 
  filter(Dominant_pre == "Mix" | Dominant == "Mix")
table(STM_MIX$LeadingSpecies_pre)
table(STM_MIX$LeadingSpecies)

mix <- table(STM_MIX$LeadingSpecies_pre, STM_MIX$LeadingSpecies)
#mat <- matrix(mix, nrow = nrow(mix), ncol = ncol(mix),
#              dimnames = dimnames(mix))
#
#mix <- insertRow(m=mat, r=3, v=0, rName="NR")

transitionPlot(mix,type_of_arrow = "simple",
               fill_start_box =c("olivedrab3","springgreen4","cyan4","lightblue",
                                 "darkolivegreen", "green4"),
               arrow_clr =c("olivedrab3","springgreen4","cyan4","lightblue",
                            "darkolivegreen", "green4"),
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               new_page = T ,
               tot_spacing = .05,
               overlap_add_width = 1)


table(bin_dis_GIS1$FIRE_NUMBER_1, bin_dis_GIS1$FIRE_YEAR_1)


## do plots stay the same or transition?

STM_trans <- STM %>%
  group_by(SampleSite_ID) %>% 
  mutate(Trans = ifelse(Dominant_pre == Dominant, "static", "shift")) %>% 
  mutate(LeadShift = ifelse(LeadingSpecies_pre == LeadingSpecies, "static", "shift"))

ggplot(STM_trans, aes(x=Trans)) +
  geom_bar(aes(color=Dominant_pre, fill = Dominant_pre))


ggplot(STM_trans, aes(x = Dominant_pre, fill = Dominant)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Trans) +
  labs(x = "Dominant_pre", y = "Count", fill = "Dominant") #+
#  scale_y_continuous(labels = scales::percent)

ggplot(STM_trans, aes(x = Dominant_pre, fill = Dominant)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Trans) +
  labs(
    x = "Pre-fire Dominant Species",
    y = "Proportion of Post-fire Species",
    fill = "Post-fire Dominant",
    title = "Post-fire Dominance by Pre-fire Species and Transition Type"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

ggplot(STM_trans, aes(x = Dominant, fill = Dominant_pre)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Trans) +
  labs(
    x = "Post-fire Dominant",
    y = "Count",
    fill = "Pre-fire Dominant",
    title = "Pre- to Post-fire Dominance Transitions by Plot Type"
  ) +
  theme_minimal()

ggplot(STM_trans, aes(x=LeadingSpecies)) +
  geom_bar(aes(color = LeadShift, fill = LeadShift), position = "dodge") 
ggplot(STM_trans, aes(x=Dominant_pre)) +
  geom_bar(aes(color = Trans, fill = Trans), position = "dodge")
  
table(STM_trans$Trans,STM_trans$Dominant_pre)
table(STM_trans$LeadShift)

table(STM$Dominant_pre, STM$Dominant)

STM_trans %>% 
  filter(Trans == "shift") %>% 
  ungroup() %>% 
  select(Dominant_pre, Dominant) %>% 
  table()

library(cowplot)
#tiff("C:/Users/nmac2000/OneDrive - UBC/Figures/climate.tif",
#     width = 90, height =500, units = "mm", res = 2244)
#plot_grid(PLI_NFFD, PLI_MCMT, FDI_MCMT, FDI_CMI, FDI_PAS,
#          labels=c("A", "B", "C", "D", "E"), ncol = 1, nrow = 5)
#dev.off()


library(grid)
par_org <- par(ask = TRUE)
grid.newpage()
transitionPlot(PLI_succession,type_of_arrow = "simple",
               fill_start_box =c("darkgoldenrod1","yellowgreen","lightblue",
                                 "darkgreen"),
               arrow_clr = c("darkgoldenrod1","yellowgreen","lightblue",
                             "darkgreen"),
               box_label = c("Pre-fire","Post-fire"),
               box_txt = c("PLI dominant", "Mixed: High PLI", "Mixed: Low PLI", 
                           "No PLI"),
               #main = "Aspen", 
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               new_page = T ,
               tot_spacing = .05,
               overlap_add_width = 1)


transitionPlot(FDI_succession,type_of_arrow = "simple",
               fill_start_box =c("darkgoldenrod1","yellowgreen","lightblue",
                                 "darkgreen"),
               arrow_clr = c("darkgoldenrod1","yellowgreen","lightblue",
                             "darkgreen"),
               box_label = c("Pre-fire","Post-fire"),
               box_txt = c("FDI dominant", "Mixed: High FDI", "Mixed: Low FDI", 
                           "No FDI"),
               #main = "Aspen", 
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               new_page = T ,
               tot_spacing = .05,
               overlap_add_width = 1)


par(par_org)


trans <- STM
trans$AT <- STM_AT$trans_AT
trans$FDI <- STM_FDI$trans_FDI
trans$PLI <- STM_PLI$trans_PLI
trans$SX <- STM_SX$trans_SX
trans <- left_join(trans, fire_severity_all, by="SampleSite_ID")



trans %>% 
#  filter(Trans == "static") %>% 
  filter(AT == "static") %>% 
#  filter(FDI == "static") %>% 
#  filter(PLI == "static") %>% 
#  filter(SX == "static") %>% 
  summarize(count = n())

STM %>% 
  filter(str_detect(Species_pre, "AT")) %>% 
  filter(!str_detect(Species, "AT")) %>% 
  select(Dominant) %>% 
  table()

STM %>% 
  filter(str_detect(Species_pre, "PLI")) %>% 
  filter(!str_detect(Species, "PLI")) %>% 
  select(Species) %>% 
  table()
