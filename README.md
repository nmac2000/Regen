# Regen

## Welcome to the project. Load these packages and these dataframes:

# Packages
library(tidyverse)
library(lmtest)
library(stats)

# Dataframe
bin_dis_GIS1 <- read.csv("https://raw.githubusercontent.com/nmac2000/Regen/refs/heads/main/Dataframes/bin_dis_GIS1.csv")
regen_GIS <- read.csv("https://raw.githubusercontent.com/nmac2000/Regen/refs/heads/main/Dataframes/regen_GIS.csv")
regen_binary <- read.csv("https://raw.githubusercontent.com/nmac2000/Regen/refs/heads/main/Dataframes/regen_binary.csv")

bin_dis_GIS1 <- bin_dis_GIS1 %>% 
  select(-X.3, -X.2, -X.1, -X)

