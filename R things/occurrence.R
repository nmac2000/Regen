library(tidyverse)


# plots with Card#B removed, ask Anya what they are
regen <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/Regen_2023_ALL.csv")
site <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/site.csv")
GIS <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/from_GIS.csv")
GIS1 <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/from_GIS.csv")
clipGis <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/PossiblePlot.csv")
internal <- read.csv("C:/Users/nmac2000/Documents/regen project/Data/Internal.csv")
#formatting data###############################################################################################
# Fill in blank species, NR = no regen
regen$Species <- sub("^$", "NR", regen$Species)
regen$Species <- sub("ACT", "NonCon", regen$Species)
regen$Species <- sub("AT", "NonCon", regen$Species)
regen$Species <- sub("SW", "Spruce", regen$Species)
regen$Species <- sub("SE", "Spruce", regen$Species)
regen$Species <- sub("SX", "Spruce", regen$Species)

regen$Card <- as.integer(regen$Card)

# get rid of unnecessary columns
regen<- regen %>% 
  select(Card,Quandrant,Species,Notes,x0,x10,x30,x130) 

# new column for species by plot
regen$Plot_Sp <- paste(regen$Card, regen$Species, sep = "")

#new column for total of seedlings per wedge by species
regen<- regen%>% 
  mutate(total = rowSums(cbind(regen$x0,regen$x10,regen$x30,regen$x130),na.rm=TRUE))

#combine different wedges by height class

regen_only_total <- regen %>% 
  group_by(Card, Species, Plot_Sp) %>% 
  summarise(across(total, sum))

regen_only_x0 <- regen %>% 
  group_by(Plot_Sp) %>% 
  summarise(x0 = sum(x0, na.rm = TRUE))

regen_only_x10 <- regen %>% 
  group_by(Plot_Sp) %>% 
  summarise(x10 = sum(x10, na.rm = TRUE))

regen_only_x30 <- regen %>% 
  group_by(Plot_Sp) %>% 
  summarise(x30 = sum(x30, na.rm = TRUE))

regen_only_x130 <- regen %>% 
  group_by(Plot_Sp) %>% 
  summarise(x130 = sum(x130, na.rm = TRUE))

#combine into 1 data frame

regen_all <- full_join(regen_only_total, regen_only_x0, by = "Plot_Sp") %>% 
  full_join(regen_only_x10, by = "Plot_Sp") %>% 
  full_join(regen_only_x30, by = "Plot_Sp") %>%
  full_join(regen_only_x130, by = "Plot_Sp")

# now i want to have columns for each species
regen_all$FDI_total <- ifelse(regen_all$Species == "FDI", regen_all$total, 0)
regen_all$FDI_x0 <- ifelse(regen_all$Species == "FDI", regen_all$x0, 0)
regen_all$FDI_x10 <- ifelse(regen_all$Species == "FDI", regen_all$x10, 0)
regen_all$FDI_x30 <- ifelse(regen_all$Species == "FDI", regen_all$x30, 0)
regen_all$FDI_x130 <- ifelse(regen_all$Species == "FDI", regen_all$x130, 0)
regen_all$PLI_total <- ifelse(regen_all$Species == "PLI", regen_all$total, 0)
regen_all$PLI_x0 <- ifelse(regen_all$Species == "PLI", regen_all$x0, 0)
regen_all$PLI_x10 <- ifelse(regen_all$Species == "PLI", regen_all$x10, 0)
regen_all$PLI_x30 <- ifelse(regen_all$Species == "PLI", regen_all$x30, 0)
regen_all$PLI_x130 <- ifelse(regen_all$Species == "PLI", regen_all$x130, 0)
regen_all$NonCon_total <- ifelse(regen_all$Species == "NonCon", regen_all$total, 0)
regen_all$NonCon_x0 <- ifelse(regen_all$Species == "NonCon", regen_all$x0, 0)
regen_all$NonCon_x10 <- ifelse(regen_all$Species == "NonCon", regen_all$x10, 0)
regen_all$NonCon_x30 <- ifelse(regen_all$Species == "NonCon", regen_all$x30, 0)
regen_all$NonCon_x130 <- ifelse(regen_all$Species == "NonCon", regen_all$x130, 0)
regen_all$Spruce_total <- ifelse(regen_all$Species == "Spruce", regen_all$total, 0)
regen_all$Spruce_x0 <- ifelse(regen_all$Species == "Spruce", regen_all$x0, 0)
regen_all$Spruce_x10 <- ifelse(regen_all$Species == "Spruce", regen_all$x10, 0)
regen_all$Spruce_x30 <- ifelse(regen_all$Species == "Spruce", regen_all$x30, 0)
regen_all$Spruce_x130 <- ifelse(regen_all$Species == "Spruce", regen_all$x130, 0)


# remove NR rows for plots that have other species present
regen_all <- regen_all %>% 
  group_by(Card) %>% 
  mutate(has_nr = any(Species == "NR"), num_rows = n()) %>% 
  filter(num_rows == 1 | !(has_nr & Species == "NR")) %>% 
  select(-has_nr, -num_rows)


## Check if we removed the correct values
# look into plots that have counts larger than zero. Seems like there should have been a species recorded
#(plots 1505, 1529, 1567, 1647, 1739, 2318)
regen_retained <- regen_all %>% 
  group_by(Card) %>% 
  mutate(has_nr = any(Species == "NR"), num_rows = n()) %>% 
  filter(num_rows == 1 | !(has_nr & Species == "NR")) %>% 
  select(-has_nr, -num_rows)

## Create dataframe with removed rows
regen_removed <- regen_all %>%
  anti_join(regen_retained, by = c("Card", "Species"))

# combine rows for same plot
regen_combo <- regen_all %>%
  group_by(Card) %>%
  summarize(SpeciesAll = paste(Species, collapse = ", "),
            total_count = sum(total),
            total_tph = total_count*200,
            total_x0 = sum(x0),
            total_x10 = sum(x10),
            total_x30 = sum(x30),
            total_x130 = sum(x130),
            FDI_count = sum(FDI_total),
            FDI_tph = FDI_count*200,
            FDI_x0 = sum(FDI_x0),
            FDI_x10 = sum(FDI_x10),
            FDI_x30 = sum(FDI_x30),
            FDI_x130 = sum(FDI_x130),
            PLI_count = sum(PLI_total),
            PLI_tph = PLI_count*200,
            PLI_x0 = sum(PLI_x0),
            PLI_x10 = sum(PLI_x10),
            PLI_x30 = sum(PLI_x30),
            PLI_x130 = sum(PLI_x130),
            NonCon_count = sum(NonCon_total),
            NonCon_x0 = sum(NonCon_x0),
            NonCon_x10 = sum(NonCon_x10),
            NonCon_x30 = sum(NonCon_x30),
            NonCon_x130 = sum(NonCon_x130),
            Spruce_count = sum(Spruce_total),
            Spruce_x0 = sum(Spruce_x0),
            Spruce_x10 = sum(Spruce_x10),
            Spruce_x30 = sum(Spruce_x30),
            Spruce_x130 = sum(Spruce_x130)) %>% 
  ungroup()



# I could add TPH, but I don't think I will. It's just multiplying everything by 200
##adding it now for total counts







# export dataframe
#write.csv(regen_combo, "C:/Users/nmac2000/Documents/regen project/Data/cleaned_counts_021224.csv")










# add GIS data
regen_copy <- regen_combo #don't
regen_copy <- regen_copy %>% 
  rename(SampleSite_ID = Card)
clipped_plots <- semi_join(clipGis, regen_copy, "SampleSite_ID")
forgotten_plots <- anti_join(regen_copy, clipped_plots, "SampleSite_ID")

clipGIS1 <- clipGis %>% 
  select(SampleSite_ID)
forgotten_plots <- forgotten_plots %>% 
  select(SampleSite_ID)
clipGis <- full_join(clipGIS1,forgotten_plots, "SampleSite_ID")

GIS <- semi_join(GIS, clipGis, "SampleSite_ID")
GIS$Card <- as.integer(GIS$SampleSite_ID)
regen_combo$Card <- as.integer(regen_combo$Card, na.rm=TRUE)

GIS$Species_1 <- sub("\\bSX\\b", "Spruce", GIS$Species_1)
GIS$Species_1 <- sub("\\bS\\b", "Spruce", GIS$Species_1)
GIS$Species_1 <- sub("\\bSE\\b", "Spruce", GIS$Species_1)
GIS$Species_1 <- sub("\\bPL\\b", "PLI", GIS$Species_1)
GIS$Species_1 <- sub("\\bFD\\b", "FDI", GIS$Species_1)
GIS$Species_1 <- sub("\\bPA\\b", "PLI", GIS$Species_1)

#write.csv(GIS, "C:/Users/nmac2000/Documents/regen project/Data/possibleplots_GIS.csv")

regen_GIS <- left_join(regen_combo,GIS, by = "Card") %>% 
  ungroup()

#write.csv(regen_GIS, "C:/Users/nmac2000/Documents/regen project/Data/regen_GIS.csv")

#trying to get some stuff to work on arc
#GIS_filter <- regen_GIS %>% 
 # filter(!is.na(Latitude)) 
#sf_data <- st_as_sf(GIS_filter, coords = c("Longitude", "Latitude"), crs = 3005)
#st_write(sf_data, "C:/Users/nmac2000/Documents/regen project/Data/cleaned_counts_021224.shp", driver = "ESRI Shapefile")
#plots###############################################################################################
head(regen_GIS)
names(regen_GIS)


## Making plots. Need to make them look better


ggplot(regen_GIS, aes(x=PARENT_SOILS,y=total_count)) +
  geom_boxplot()

ggplot(regen_GIS, aes(x=Fire_Severity_1,y=total_count)) +
  geom_boxplot()

ggplot(regen_GIS, aes(x=Zone,y=total_count)) +
  geom_boxplot()

#will maybe have to combine some of these (what is PL)
ggplot(regen_GIS, aes(x=Species_1,y=total_count)) +
  geom_boxplot()

ggplot(regen_GIS, aes(x=DRAIN_1,y=total_count)) +
  geom_boxplot()

ggplot(regen_GIS, aes(x=DRAIN_1)) +
  geom_boxplot(aes(y=total_count), color="blue",position = "dodge") +
  geom_boxplot(aes(y=PLI_count), color = "red", position = "dodge")

ggplot(regen_GIS, aes(x=Solar_Radiation)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

ggplot(regen_GIS, aes(x=TWI)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5) 

ggplot(regen_GIS, aes(x=COFRAG_1)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

#do as boxplot
ggplot(regen_GIS, aes(x=Aspect)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

ggplot(regen_GIS, aes(x=Severity_Patch_Size_1)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

ggplot(regen_GIS, aes(x=Age_1)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)  

ggplot(regen_GIS, aes(x=Height_1)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

ggplot(regen_GIS, aes(x=Crown_Closure_1)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

ggplot(regen_GIS, aes(x=Disturbance_Date_1)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

ggplot(regen_GIS, aes(x=Distance_to_Perimeter)) +
  geom_point(aes(y=PLI_count), color="red", alpha= .5) +
  geom_point(aes(y=FDI_count), color="green", alpha=.5)

#deliverables###############################################################################################


table(GIS$SOILNAME_1)
table(regen_GIS$SOILNAME_1)


compare_tables <- function(table1, table2, driver_column) {
  # Convert factor columns to characters
  table1[[driver_column]] <- as.character(table1[[driver_column]])
  table2[[driver_column]] <- as.character(table2[[driver_column]])
  
  # Use count function to get counts for each table
  count_table1 <- count(table1, !!as.name(driver_column))
  count_table2 <- count(table2, !!as.name(driver_column))
  
  # Combine counts using full join
  comparison_df <- full_join(count_table1, count_table2, by = driver_column)
  
  # Replace NAs with 0 in the count columns
  comparison_df[is.na(comparison_df)] <- 0
  
  # Return the comparison table
  return(comparison_df)
}

# usage:
driver_column_name <- c("Fire_Severity_1")  
result <- compare_tables(GIS, regen_GIS, driver_column_name)
print(result)


BEC_comparison <- compare_tables(GIS, regen_GIS, "Zone")%>%
  rename(total_plots = n.x, measured_plots = n.y) %>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)
ParentSoil_comparison <- compare_tables(GIS, regen_GIS, "PARENT_SOILS")%>%
  rename(total_plots = n.x, measured_plots = n.y) %>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)
SoilName_comparison  <- compare_tables(GIS, regen_GIS, "SOILNAME_1")%>%
  rename(total_plots = n.x, measured_plots = n.y) %>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)
Texture_comparison <- compare_tables(GIS, regen_GIS, "TEXTURE_1")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)
Drainage_comparison  <- compare_tables(GIS, regen_GIS, "DRAIN_1")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)
#two different rows with 0s
Cofrag_comparison  <- compare_tables(GIS, regen_GIS, "COFRAG_1")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100)
FireYear_comparison  <- compare_tables(GIS, regen_GIS, "Fire_Year_1")%>%
  rename(total_plots = n.x, measured_plots = n.y) %>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%  
  filter(row_number() <= n()-1)
FireNum_comparison  <- compare_tables(GIS, regen_GIS, "Fire_Number_1")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/total_plots*100) %>% 
  filter(row_number() <= n()-1)
FireSeverity_comparison  <- compare_tables(GIS, regen_GIS, "Fire_Severity_1")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)




Overstory_comparison  <- compare_tables(GIS, regen_GIS, "Species_1")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)
# Combine some of the Species for overstory
# PLI <- PL
# FDI <- FD
# Spruce <- SE, SX
# ??? <- B, EP, S


#
CrownClosure_comparison  <- compare_tables(GIS, regen_GIS, "Crown_Closure_1")%>%
  rename(total_plots = n.x, measured_plots = n.y) %>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) 
Disturbance_comparison  <- compare_tables(GIS, regen_GIS, "Disturbance_Type_1")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)
Subzone_comparison  <- compare_tables(GIS, regen_GIS, "Subzone")%>%
  rename(total_plots = n.x, measured_plots = n.y)%>% 
  mutate(PercentOfTotal = total_plots/sum(total_plots)*100) %>%
  mutate(PercentSampled = measured_plots/sum(measured_plots)*100) %>%
  filter(row_number() <= n()-1)







#graphs of deliverables####################
BEC_comparison_long <- gather(BEC_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
ParentSoil_comparison_long <- gather(ParentSoil_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
SoilName_comparison_long <- gather(SoilName_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
Texture_comparison_long <- gather(Texture_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
Drainage_comparison_long <- gather(Drainage_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
Cofrag_comparison_long <- gather(Cofrag_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
FireYear_comparison_long <- gather(FireYear_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
FireNum_comparison_long <- gather(FireNum_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
FireSeverity_comparison_long <- gather(FireSeverity_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
Overstory_comparison_long <- gather(Overstory_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
CrownClosure_comparison_long <- gather(CrownClosure_comparison, key = "Variable", value = "Value", total_plots, measured_plots)
Disturbance_comparison_long <- gather(Disturbance_comparison, key = "Variable", value = "Value", total_plots, measured_plots)

#BEC zone
ggplot(data = BEC_comparison_long, aes(x = Zone, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by BEC Zone",
       x = "BEC Zone",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Parent Material
ggplot(data = ParentSoil_comparison_long, aes(x = PARENT_SOILS, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Parent SOil",
       x = "Parent Soil",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Soil Name
ggplot(data = SoilName_comparison_long, aes(x = SOILNAME_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Soil Name",
       x = "Soil Name",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Soil Texture
ggplot(data = Texture_comparison_long, aes(x = TEXTURE_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Soil Texture",
       x = "Soil Texture",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Soil Drainage
ggplot(data = Drainage_comparison_long, aes(x = DRAIN_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Soil Drainage",
       x = "Soil Drainage",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Coarse Fragment
ggplot(data = Cofrag_comparison_long, aes(x = COFRAG_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Soil Coarsefragment %",
       x = "Coarse fragment %",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Fire Year
ggplot(data = FireYear_comparison_long, aes(x = Fire_Year_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Fire Year",
       x = "Year",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Fire Number 1
ggplot(data = FireNum_comparison_long, aes(x = Fire_Number_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Fire Number",
       x = "Fire Number",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Fire Severity
ggplot(data = FireSeverity_comparison_long, aes(x = Fire_Severity_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Fire Severity",
       x = "Fire Severity",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Overstory Species
ggplot(data = Overstory_comparison_long, aes(x = Species_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Pre-Fire OVerstory",
       x = "Species",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Crown Closure
ggplot(data = CrownClosure_comparison_long, aes(x = Crown_Closure_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by % Crown Closure",
       x = "% Crown Closure",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Pre-Fire Disturbance
ggplot(data = Disturbance_comparison_long, aes(x = Disturbance_Type_1, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Variable") +
  scale_fill_manual(values = c("total_plots" = "blue", "measured_plots" = "red")) +
  labs(title = "Comparison of Total and Measured Plots by Pre-Fire Disturbance",
       x = "Disturbance Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#nothing####################



#deliverables#################################################
#Anya wants conifer count
#Drainage
regen_drainage <- regen_GIS %>% 
  select(Card:Spruce_x130,DRAIN_1) %>% 
  group_by(DRAIN_1) %>% 
  filter(!is.na(DRAIN_1))

regen_drainage$Con_count <- regen_drainage$FDI_count + regen_drainage$PLI_count + regen_drainage$Spruce_count
regen_drainage$Con_tph <- regen_drainage$Con_count*200

options(pillar.sigfig = 6) 

regen_drainage %>% 
  summarise(across(c(31,32), sd))

regen_drainage %>% 
  filter(Con_count > 5) %>%
  summarise(count = n())


regen_drainage %>% 
  summarise(percentage = sum(Con_count > 5) / n() * 100)

ggplot(regen_drainage) +
  geom_histogram(aes(x = Con_count, fill = DRAIN_1), alpha = 0.7, binwidth = 50) +
  labs(x = "Conifer Seedlings per Plot", y = "Frequency", title = "Distribution of Conifer Seedlings per Plot") +
  theme_minimal() +
  facet_wrap(vars(DRAIN_1))

#Fire severity
regen_FS <- regen_GIS %>% 
  select(Card:Spruce_x130,Fire_Severity_1) %>% 
  group_by(Fire_Severity_1) %>% 
  filter(!is.na(Fire_Severity_1))

regen_FS$Con_count <- regen_FS$FDI_count + regen_FS$PLI_count + regen_FS$Spruce_count
regen_FS$Con_tph <- regen_FS$Con_count*200

options(pillar.sigfig = 6) 

regen_FS %>% 
  summarise(across(c(31,32), sd))

regen_FS %>% 
  filter(Con_count > 5) %>%
  summarise(count = n())


regen_FS %>% 
  summarise(percentage = sum(Con_count > 5) / n() * 100)

ggplot(regen_FS) +
  geom_histogram(aes(x = Con_count, fill = Fire_Severity_1), alpha = 0.7, binwidth = 50) +
  labs(x = "Conifer Seedlings per Plot", y = "Frequency", title = "Distribution of Conifer Seedlings per Plot") +
  theme_minimal() +
  facet_wrap(vars(Fire_Severity_1))

#write.csv(FireSeverity_comparison, "C:/Users/nmac2000/Documents/regen project/Data/FireSeverity.csv")

#BEC zones
regen_BEC <- regen_GIS %>% 
  select(Card:Spruce_x130,Zone) %>% 
  group_by(Zone) %>% 
  filter(!is.na(Zone))

regen_BEC$Con_count <- regen_BEC$FDI_count + regen_BEC$PLI_count + regen_BEC$Spruce_count
regen_BEC$Con_tph <- regen_BEC$Con_count*200

options(pillar.sigfig = 6) 

regen_BEC %>% 
  summarise(across(c(31,32), sd))

regen_BEC %>% 
  filter(Con_count > 0) %>%
  summarise(count = n())


regen_BEC %>% 
  summarise(percentage = sum(PLI_count > 5) / n() * 100)

ggplot(regen_BEC) +
  geom_histogram(aes(x = Con_count, fill = Zone), alpha = 0.7, binwidth = 50) +
  labs(x = "Conifer Seedlings per Plot", y = "Frequency", title = "Distribution of Conifer Seedlings per Plot") +
  theme_minimal() +
  facet_wrap(vars(Zone))


## add subzone
regen_subzone <- regen_GIS %>% 
  select(Card:Spruce_x130,Zone,Subzone) %>% 
  group_by(Zone,Subzone) %>% 
  filter(!is.na(Zone))

regen_subzone$Con_count <- regen_BEC$FDI_count + regen_BEC$PLI_count + regen_BEC$Spruce_count
regen_subzone$Con_tph <- regen_BEC$Con_count*200

options(pillar.sigfig = 6) 

regen_subzone %>% 
  summarise(across(c(31,32), sd))

regen_subzone %>% 
  filter(Con_count > 0) %>%
  summarise(count = n())


regen_subzone %>% 
  summarise(percentage = sum(PLI_count > 5) / n() * 100)

ggplot(regen_subzone) +
  geom_histogram(aes(x = Con_count, fill = Subzone), alpha = 0.7, binwidth = 50) +
  labs(x = "Conifer Seedlings per Plot", y = "Frequency", title = "Distribution of Conifer Seedlings per Plot") +
  theme_minimal() +
  facet_wrap(vars(Subzone))

#Overstory


regen_Overstory <- regen_GIS %>% 
  select(Card:Spruce_x130,Species_1) %>% 
  group_by(Species_1) %>% 
  filter(!is.na(Species_1))

regen_Overstory$Species_1 <- sub("\\bSX\\b", "Spruce", regen_Overstory$Species_1)
regen_Overstory$Species_1 <- sub("\\bS\\b", "Spruce", regen_Overstory$Species_1)
regen_Overstory$Species_1 <- sub("\\bSE\\b", "Spruce", regen_Overstory$Species_1)
regen_Overstory$Species_1 <- sub("\\bPL\\b", "PLI", regen_Overstory$Species_1)
regen_Overstory$Species_1 <- sub("\\bFD\\b", "FDI", regen_Overstory$Species_1)
regen_Overstory$Species_1 <- sub("\\bPA\\b", "PLI", regen_Overstory$Species_1)

regen_Overstory$Con_count <- regen_Overstory$FDI_count + regen_Overstory$PLI_count + regen_Overstory$Spruce_count
regen_Overstory$Con_tph <- regen_Overstory$Con_count*200

options(pillar.sigfig = 6) 

regen_Overstory %>% 
  summarise(across(c(31,32), sd))

regen_Overstory %>% 
  filter(Con_count > 0) %>%
  summarise(count = n())


regen_Overstory %>% 
  summarise(percentage = sum(PLI_count > 5) / n() * 100)

ggplot(regen_Overstory) +
  geom_histogram(aes(x = Con_count, fill = Species_1), alpha = 0.7, binwidth = 50) +
  labs(x = "Conifer Seedlings per Plot", y = "Frequency", title = "Distribution of Conifer Seedlings per Plot") +
  theme_minimal() +
  facet_wrap(vars(Species_1))

##############################Which plots have unknown burn severity##########################

for_table <- regen_GIS %>% 
  filter(Fire_Severity_1 == "Unknown") %>% 
  select(Card, Fire_Year_1, Fire_Year_2, Fire_Severity_1, Fire_Severity_2, Fire_Number_1, Fire_Number_2, Longitude, Latitude)

#write.csv(for_table, "C:/Users/nmac2000/Documents/regen project/Data/Unknown_Severity_Measured_Plots.csv")


#Wedges###############################
# now i want to have columns for each species
regen_wedge <- regen
regen_wedge$FDI_total <- ifelse(regen_wedge$Species == "FDI", regen_wedge$total, 0)
regen_wedge$FDI_x0 <- ifelse(regen_wedge$Species == "FDI", regen_wedge$x0, 0)
regen_wedge$FDI_x10 <- ifelse(regen_wedge$Species == "FDI", regen_wedge$x10, 0)
regen_wedge$FDI_x30 <- ifelse(regen_wedge$Species == "FDI", regen_wedge$x30, 0)
regen_wedge$FDI_x130 <- ifelse(regen_wedge$Species == "FDI", regen_wedge$x130, 0)
regen_wedge$PLI_total <- ifelse(regen_wedge$Species == "PLI", regen_wedge$total, 0)
regen_wedge$PLI_x0 <- ifelse(regen_wedge$Species == "PLI", regen_wedge$x0, 0)
regen_wedge$PLI_x10 <- ifelse(regen_wedge$Species == "PLI", regen_wedge$x10, 0)
regen_wedge$PLI_x30 <- ifelse(regen_wedge$Species == "PLI", regen_wedge$x30, 0)
regen_wedge$PLI_x130 <- ifelse(regen_wedge$Species == "PLI", regen_wedge$x130, 0)
regen_wedge$NonCon_total <- ifelse(regen_wedge$Species == "NonCon", regen_wedge$total, 0)
regen_wedge$NonCon_x0 <- ifelse(regen_wedge$Species == "NonCon", regen_wedge$x0, 0)
regen_wedge$NonCon_x10 <- ifelse(regen_wedge$Species == "NonCon", regen_wedge$x10, 0)
regen_wedge$NonCon_x30 <- ifelse(regen_wedge$Species == "NonCon", regen_wedge$x30, 0)
regen_wedge$NonCon_x130 <- ifelse(regen_wedge$Species == "NonCon", regen_wedge$x130, 0)
regen_wedge$Spruce_total <- ifelse(regen_wedge$Species == "Spruce", regen_wedge$total, 0)
regen_wedge$Spruce_x0 <- ifelse(regen_wedge$Species == "Spruce", regen_wedge$x0, 0)
regen_wedge$Spruce_x10 <- ifelse(regen_wedge$Species == "Spruce", regen_wedge$x10, 0)
regen_wedge$Spruce_x30 <- ifelse(regen_wedge$Species == "Spruce", regen_wedge$x30, 0)
regen_wedge$Spruce_x130 <- ifelse(regen_wedge$Species == "Spruce", regen_wedge$x130, 0)


# remove NR rows for plots that have other species present
regen_wedge <- regen_wedge %>% 
  group_by(Card) %>% 
  mutate(has_nr = any(Species == "NR"), num_rows = n()) %>% 
  filter(num_rows == 1 | !(has_nr & Species == "NR")) %>% 
  select(-has_nr, -num_rows) %>% 
  select(-Plot_Sp)

#regen_wedge <- regen_wedge %>% 
  rename("total_x0" = "x0",
         "total_x10" = "x10",
         "total_x30" = "x30",
         "total_x130" = "x130",
         "total_count" = "total")




regen_wedge1 <- regen_wedge %>% 
  group_by(Card, Quandrant) %>% 
  summarize(SpeciesAll = paste(Species, collapse = ", "),
            total_count = sum(total),
            total_tph = total_count*200,
            total_x0 = sum(x0),
            total_x10 = sum(x10),
            total_x30 = sum(x30),
            total_x130 = sum(x130),
            FDI_count = sum(FDI_total),
            FDI_tph = FDI_count*200,
            FDI_x0 = sum(FDI_x0),
            FDI_x10 = sum(FDI_x10),
            FDI_x30 = sum(FDI_x30),
            FDI_x130 = sum(FDI_x130),
            PLI_count = sum(PLI_total),
            PLI_tph = PLI_count*200,
            PLI_x0 = sum(PLI_x0),
            PLI_x10 = sum(PLI_x10),
            PLI_x30 = sum(PLI_x30),
            PLI_x130 = sum(PLI_x130),
            NonCon_count = sum(NonCon_total),
            NonCon_x0 = sum(NonCon_x0),
            NonCon_x10 = sum(NonCon_x10),
            NonCon_x30 = sum(NonCon_x30),
            NonCon_x130 = sum(NonCon_x130),
            Spruce_count = sum(Spruce_total),
            Spruce_x0 = sum(Spruce_x0),
            Spruce_x10 = sum(Spruce_x10),
            Spruce_x30 = sum(Spruce_x30),
            Spruce_x130 = sum(Spruce_x130)) %>% 
  ungroup()



test <- regen_wedge1 %>% 
  group_by(Card) %>% 
  summarize(total_sd = sqrt(sum((total_count-sum(total_count)/4)^2)/3),
            PLI_sd = sqrt(sum((PLI_count-sum(PLI_count)/4)^2)/3),
            FDI_sd = sqrt(sum((FDI_count-sum(FDI_count)/4)^2)/3))

options(scipen=999)

test <- test %>%
  gather(key = "Species", value = "SD", total_sd, PLI_sd, FDI_sd)

ggplot(data=test, aes(x = SD, fill = Species)) +
  geom_histogram(position = "dodge", bins = 30, color = "white", alpha = 0.7) +
  facet_wrap(~Species) +
  theme_minimal() +
  labs(x="SD (Seedlings per plot)", y="Frequency", title="In Plot Variability") +
  scale_fill_discrete()

ggplot(data=filter(test, SD > 0), aes(x = SD, fill = Species)) +
  geom_histogram(position = "dodge", bins = 30, color = "white", alpha = 0.7) +
  facet_wrap(~Species) +
  theme_minimal() +
  labs(x="SD (Seedlings per plot)", y="Frequency", title="In Plot Variability") +
  scale_fill_discrete()

options(scipen=0)


regen_GIS %>% 
  filter(PLI_count <1) %>% 
  select(PLI_count, Fire_Year_1) %>% 
  table()
 
regen_GIS$Card <- as.numeric(regen_GIS$Card)
#write.csv(regen_GIS, "C:/Users/nmac2000/Documents/regen project/Data/GIS_clipped3.csv")

#northeast-to-southwest facing scale (Beers et al., 1996)
#Taspect = sin(aspect + 45) + 1

##this should be moved
regen_GIS$Con_count <- regen_GIS$FDI_count + regen_GIS$PLI_count + regen_GIS$Spruce_count
regen_GIS$Con_tph <- regen_GIS$Con_count*200
##

asp <- regen_GIS 
asp$aspect <- sin(regen_GIS$Aspect + 45) + 1

ggplot(asp, aes(aspect, Con_count)) +
  geom_point(aes(color=SpeciesAll))

#graph for presentation
regen_binary <- regen_GIS %>% 
  mutate(total_count_bin = ifelse(total_count>0,1,0),
         total_x0_bin = ifelse(total_x0>0,1,0),
         total_x10_bin = ifelse(total_x10>0,1,0),
         total_x30_bin = ifelse(total_x30>0,1,0),
         total_x130_bin = ifelse(total_x130>0,1,0),
         FDI_count_bin = ifelse(FDI_count>0,1,0),
         FDI_x0_bin = ifelse(FDI_x0>0,1,0),
         FDI_x10_bin = ifelse(FDI_x10>0,1,0),
         FDI_x30_bin = ifelse(FDI_x30>0,1,0),
         FDI_x130_bin = ifelse(FDI_x130>0,1,0),
         PLI_count_bin = ifelse(PLI_count>0,1,0),
         PLI_x0_bin = ifelse(PLI_x0>0,1,0),
         PLI_x10_bin = ifelse(PLI_x10>0,1,0),
         PLI_x30_bin = ifelse(PLI_x30>0,1,0),
         PLI_x130_bin = ifelse(PLI_x130>0,1,0),
         NonCon_count_bin = ifelse(NonCon_count>0,1,0),
         NonCon_x0_bin = ifelse(NonCon_x0>0,1,0),
         NonCon_x10_bin = ifelse(NonCon_x10>0,1,0),
         NonCon_x30_bin = ifelse(NonCon_x30>0,1,0),
         NonCon_x130_bin = ifelse(NonCon_x130>0,1,0),
         Spruce_count_bin = ifelse(Spruce_count>0,1,0),
         Spruce_x0_bin = ifelse(Spruce_x0>0,1,0),
         Spruce_x10_bin = ifelse(Spruce_x10>0,1,0),
         Spruce_x30_bin = ifelse(Spruce_x30>0,1,0),
         Spruce_x130_bin = ifelse(Spruce_x130>0,1,0))

create_binary_columns <- function(data) {
  binary_cols <- c("total_count", "total_x0", "total_x10", "total_x30", "total_x130",
                   "FDI_count", "FDI_x0", "FDI_x10", "FDI_x30", "FDI_x130",
                   "PLI_count", "PLI_x0", "PLI_x10", "PLI_x30", "PLI_x130",
                   "NonCon_count", "NonCon_x0", "NonCon_x10", "NonCon_x30", "NonCon_x130",
                   "Spruce_count", "Spruce_x0", "Spruce_x10", "Spruce_x30", "Spruce_x130")
  
  for (col in binary_cols) {
    binary_col <- paste0(col, "_bin")
    data[[binary_col]] <- ifelse(data[[col]] > 0, 1, 0)
  }
  
  return(data)
}

regen_binary1 <- create_binary_columns(regen_GIS) %>% 
  select(Card, total_count_bin:Spruce_x130_bin)


regen_binary <- regen_binary %>% 
  select(Card, total_count_bin:Spruce_x130_bin)

library(testthat)
test_that("Test if data frames are same", {
  expect_equal(regen_binary,regen_binary1)
})

names(regen_binary)
regen_for_table <- regen_binary %>% 
  summarise(total_no_occurrence = n()-sum(total_count_bin),
            x0_no_occurrence = n()-sum(total_x0_bin),
            x10_no_occurrence = n()-sum(total_x10_bin),
            x30_no_occurrence = n()-sum(total_x30_bin),
            x130_no_occurrence = n()-sum(total_x130_bin),
            FDI_count_occurrence = sum(FDI_count_bin),
            FDI_x0_occurrence = sum(FDI_x0_bin),
            FDI_x10_occurrence = sum(FDI_x10_bin),
            FDI_x30_occurrence = sum(FDI_x30_bin),
            FDI_x130_occurrence = sum(FDI_x130_bin),
            PLI_count_occurrence = sum(PLI_count_bin),
            PLI_x0_occurrence = sum(PLI_x0_bin),
            PLI_x10_occurrence = sum(PLI_x10_bin),
            PLI_x30_occurrence = sum(PLI_x30_bin),
            PLI_x130_occurrence = sum(PLI_x130_bin),
            NonCon_count_occurrence = sum(NonCon_count_bin),
            NonCon_x0_occurrence = sum(NonCon_x0_bin),
            NonCon_x10_occurrence = sum(NonCon_x10_bin),
            NonCon_x30_occurrence = sum(NonCon_x30_bin),
            NonCon_x130_occurrence = sum(NonCon_x130_bin),
            Spruce_count_occurrence = sum(Spruce_count_bin),
            Spruce_x0_occurrence = sum(Spruce_x0_bin),
            Spruce_x10_occurrence = sum(Spruce_x10_bin),
            Spruce_x30_occurrence = sum(Spruce_x30_bin),
            Spruce_x130_occurrence = sum(Spruce_x130_bin)
            )

regen_table <- gather(regen_for_table, "AgeClass", "Count",1:25)
regen_table$Species <- c("No Regen","No Regen","No Regen","No Regen","No Regen","FDI","FDI",
                         "FDI","FDI","FDI","PLI","PLI","PLI","PLI","PLI","Non Conifer",
                         "Non Conifer","Non Conifer","Non Conifer","Non Conifer", 
                         "Spruce","Spruce","Spruce","Spruce", "Spruce")
regen_table$AgeClass <- c("All heights", "x0", "x10", "x30", "x130",
                          "All heights", "x0", "x10", "x30", "x130",
                          "All heights", "x0", "x10", "x30", "x130",
                          "All heights", "x0", "x10", "x30", "x130",
                          "All heights", "x0", "x10", "x30", "x130")

class_order <- c("All heights", "x0", "x10", "x30", "x130")
species_order <- c("PLI","FDI","Spruce","Non Conifer","No Regen")
ggplot(regen_table, aes(x = factor(AgeClass, levels = class_order), y = Count)) +
  geom_col(aes(fill = factor(Species, levels = species_order)), position = "dodge") +
  xlab("Height Class (cm)") +
  ylab("# of Plots with Occurrence") +
  ggtitle("Occurrence of Regen by Height Class and Species") +
  labs(fill = "Species") +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(labels = c("total" = "All heights", "x0" = "0-9", "x10" = "10-29", "x30" = "30-129", "x130" = "130+")) +
  theme_bw() +
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        plot.title = element_text(size = 20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20))
  

  