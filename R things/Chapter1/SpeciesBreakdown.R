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

MIX_regen <- regen_percents %>% 
  filter(Dominant == "Mix")

ggplot(MIX_VRI, aes(x=LeadingSpecies,y = LeadingPercent)) +
  geom_count()


table(MIX_regen$LeadingSpecies, MIX_regen$Leading)


MIX_VRI %>% 
  filter(LeadingSpecies == "AT") %>% 
  select(LeadingSpecies:FourthPercent)

# Pre and Post

STM <- full_join(VRI_all, regen_percents)

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
    FDI_percent == 0 ~ "NoFDI"))

ggplot(STM_FDI, aes(x=FDI_pre, y=FDI_post)) +
  geom_count()

FDI_succession <- table(STM_FDI$FDI_pre, STM_FDI$FDI_post)
transitionPlot(FDI_succession,type_of_arrow = "gradient",overlap_add_width = 1.3,
               fill_start_box =c("darkgoldenrod1","yellowgreen","lightblue","darkgreen"),
               min_lwd =unit(0, "mm"),
               max_lwd =unit(10, "mm"),
               main = "FDI",
               new_page = T)

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
    PLI_percent == 0 ~ "NoPLI"))

ggplot(STM_PLI, aes(x=PLI_pre, y=PLI_post)) +
  geom_count()

PLI_succession <- table(STM_PLI$PLI_pre, STM_PLI$PLI_post)
transitionPlot(PLI_succession,type_of_arrow = "gradient",overlap_add_width = 1.3,
               fill_start_box =c("darkgoldenrod1","yellowgreen","lightblue","darkgreen"),
               min_lwd =unit(0, "mm"),
               max_lwd =unit(10, "mm"),
               main = "PLI",
               new_page = T)

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
    SX_percent == 0 ~ "NoSX"))

ggplot(STM_SX, aes(x=SX_pre, y=SX_post)) +
  geom_count()

SX_succession <- table(STM_SX$SX_pre, STM_SX$SX_post)
transitionPlot(SX_succession,type_of_arrow = "gradient",overlap_add_width = 1.3,
               fill_start_box =c("darkgoldenrod1","yellowgreen","lightblue","darkgreen"),
               min_lwd =unit(0, "mm"),
               max_lwd =unit(10, "mm"),
               main = "SX",
               new_page = T)

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
    AT_percent == 0 ~ "NoAT"))

ggplot(STM_AT, aes(x=AT_pre, y=AT_post)) +
  geom_count()

AT_succession <- table(STM_AT$AT_pre, STM_AT$AT_post)
transitionPlot(AT_succession,type_of_arrow = "gradient",overlap_add_width = unit(1.3, "mm"),
               fill_start_box =c("darkgoldenrod1","yellowgreen","lightblue","darkgreen"),
               min_lwd =unit(0, "mm"),
               max_lwd =unit(6, "mm"),
               main = "AT",
               new_page = TRUE)

# Overall Transitions

STM <- STM %>% 
  mutate(State_pre = case_when(
    Dominant_pre == "AT" ~ "AT Dominant",
    Dominant_pre == "FDI" ~ "FDI Dominant",
    Dominant_pre == "PLI" ~ "PLI Dominant",
    Dominant_pre == "SX" ~ "SX Dominant",
    Dominant_pre == "Mix" ~ "Mix",
  ))

succession <- table(STM$Dominant_pre, STM$Dominant)
new_row <- matrix(0, nrow = 1, ncol = ncol(succession))
colnames(new_row) <- colnames(succession)
rownames(new_row) <- "NR"
insert_position <- 4
succession <- rbind(
  succession[1:(insert_position - 1), , drop = FALSE],
  new_row,
  succession[insert_position:nrow(succession), , drop = FALSE]
)


transitionPlot(succession,type_of_arrow = "gradient",
               fill_start_box =c("darkgoldenrod1","yellowgreen","lightblue",
                                 "darkgreen", "darkslategray4", "lightgoldenrod"),
              new_page = T )


