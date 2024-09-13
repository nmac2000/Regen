#########################################
# Looking at error
#########################################

df <- data.frame(
  x = bin_dis_GIS1$SampleSite_ID,
  y = bin_dis_GIS1$FDI.f,
                 x1 = bin_dis_GIS1$BEC_Subzone,
                 x2 = bin_dis_GIS1$PARENT_SOILS,
                 x3 = bin_dis_GIS1$years_since)

model <- glm(y ~ x3 + x1 + x2,
                   family=binomial(link = "logit"), data=df)
summary(model)

#use fitted model to predict response values
df$y_pred = predict(model, df, type="response")

#view updated data frame
df

bin_dis_GIS2 <- bin_dis_GIS1 %>% 
  filter(!SampleSite_ID %in% c(1876, 1981, 2311, 2460, 2662) )

#looking at parent material and drainage

bin_dis_GIS1 %>% 
  filter(SampleSite_ID %in% c(1876, 1981, 2311, 2460, 2662)) %>% 
  select(SampleSite_ID, BEC_Zone, BEC_Subzone, PARENT_SOILS)


