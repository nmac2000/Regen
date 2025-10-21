#################################################
# PLI site models with slope/aspect interaction #
#################################################

library(lmtest)
bin_dis_GIS1 <- read.csv("https://raw.githubusercontent.com/nmac2000/Regen/refs/heads/main/Dataframes/bin_dis_GIS3.csv")


site.PLI.a <- glm(PLI.f ~ years_since ,
                  family=binomial(link = "logit"), data=bin_dis_GIS1)
summary(site.PLI.a)

site.PLI.2a <- glm(PLI.f ~ years_since + BEC_Subzone,
                   family=binomial(link = "logit"), data=bin_dis_GIS1)
summary(site.PLI.2a) #AIC SCORE: 282
lrtest(site.PLI.2a, site.PLI.a)

site.PLI.2c <- glm(PLI.f ~ years_since + BEC_Subzone +  Slope + I(Slope*sin(Aspect*(pi/180))) + I(Slope*cos(Aspect*(pi/180))) ,
                   family=binomial(link = "logit"), data=bin_dis_GIS1)
summary(site.PLI.2c) #SLOPE SIGNIFICANT, INTERACTIONS ARE NOT, AIC SCORE: 281
lrtest(site.PLI.2c, site.PLI.2a) #NOT SIGNIFICANT

site.PLI.2d <- glm(PLI.f ~ years_since + BEC_Subzone +  Slope ,
                   family=binomial(link = "logit"), data=bin_dis_GIS1)
summary(site.PLI.2d) #SLOPE SIGNIFICANT, AIC SCORE: 278
lrtest(site.PLI.2d, site.PLI.2a) #SIGNIFICANT


site.PLI.2a <- glm(PLI.f ~ years_since +  Slope + I(Slope*sin(Aspect*(pi/180))) + I(Slope*cos(Aspect*(pi/180))) ,
                   family=binomial(link = "logit"), data=bin_dis_GIS1)
summary(site.PLI.2a) #SLOPE SIGNIFICANT, INTERACTIONS ARE NOT, AIC SCORE: 281
lrtest(site.PLI.2c, site.PLI.2a) #NOT SIGNIFICANT

