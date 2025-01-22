########################
# Official Final Models#
########################

# FDI
FDI.7 <- glm(FDI.f ~ years_since + FDI_percent + MCMT + PARENT_SOILS + Distance + 
               years_since*MCMT,
             family=binomial(link = "logit"), data=bin_dis_GIS1)

climate.FDI.2f <- glm(FDI.f ~ years_since + MCMT + PAS + CMI_sm,
                      family = binomial(link="logit"), data=bin_dis_GIS1)

site.FDI.3k <- glm(FDI.f ~ years_since + BEC_Subzone + PARENT_SOILS  + BEC_Subzone*years_since ,
                   family=binomial(link = "logit"), data=bin_dis_GIS1)

structure.FDI.3j <- glm(FDI.f ~   Distance +years_since + as.factor(BARC.x) +FDI_percent + years_since*FDI_percent,
                        family=binomial(link = "logit"), data=bin_dis_GIS1)

# PLI
PLI.6 <- glm(PLI.f ~ years_since + PLI_percent + BEC_Subzone + BARC.x ,
             family=binomial(link = "logit"), data=bin_dis_GIS1)

climate.PLI.2d <- glm(PLI.f ~ years_since + NFFD_sp + cool.wet.anomalies.MCMT ,
                      family = binomial(link="logit"), data=bin_dis_GIS1)

site.PLI.2i <- glm(PLI.f ~ years_since + BEC_Subzone + Slope,
                   family=binomial(link = "logit"), data=bin_dis_GIS1)

structure.PLI.5a <- glm(PLI.f ~ years_since  + as.factor(BARC.x)  +PLI_percent,
                        family=binomial(link = "logit"), data=bin_dis_GIS1)
