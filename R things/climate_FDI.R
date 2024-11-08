##############################
# Graphs for FDI copy climate variables
# Nat
# October 23, 2024
#############################


bin_dis_GIS <- bin_dis_GIS1 %>% 
  select(SampleSite_ID, PLI_count, PLI_count_bin, FDI_count, FDI_count_bin, years_since)

zscores_regen <-left_join(zscores, bin_dis_GIS, by="SampleSite_ID")
zscores.regen <- left_join(zscore.sums, bin_dis_GIS, by="SampleSite_ID")

zscores.regen <- zscores.regen %>% 
  mutate(hot.dry.avg = hot.dry.sum/5) %>% 
  mutate(cool.wet.avg = cool.wet.sum/5)

ggplot(zscores_regen, aes(x=hot.dry.anomalies.PAS, y=PLI_count)) +
  geom_point()

ggplot(zscores.regen, aes(y=hot.dry.avg, x=as.factor(FDI_count_bin))) +
  geom_boxplot()

names(zscores_regen)

zscores.regen$PLI.f <- as.factor(zscores.regen$PLI_count_bin)
zscores.regen$FDI.f <- as.factor(zscores.regen$FDI_count_bin)


pairs(~ cool.wet.anomalies.MAT + cool.wet.anomalies.MWMT + cool.wet.anomalies.MAP + 
        cool.wet.anomalies.PAS + cool.wet.anomalies.AHM + cool.wet.anomalies.SHM + 
        cool.wet.anomalies.Tave_sp + cool.wet.anomalies.Tave_sm + cool.wet.anomalies.Tmax_sp + 
        cool.wet.anomalies.Tmax_sm + cool.wet.anomalies.Tmin_sp + cool.wet.anomalies.Tmin_sm + 
        cool.wet.anomalies.PAS_sp + cool.wet.anomalies.PAS_wt + cool.wet.anomalies.PPT_sm + 
        pos.anomalies.NFFD_sp,
      data = zscores, main = "Scatterplot Matrix for Cool/Wet and Positive Anomalies")

ggplot(bin_dis_GIS1, aes(x=PATCHSIZE)) +
  geom_histogram()

  