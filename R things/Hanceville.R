#################
#Hanceville Fire 
#################

hanceville <- bin_dis_GIS1 %>% 
  filter(FIRE_NUMBER_1 == "C50647") 

table(hanceville$PARENT_SOILS, hanceville$BEC_Subzone)


Hanceville.null <- glm(PLI.f ~ 1 ,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.null)

Hanceville.a <- glm(PLI.f ~ years_since ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.a)
lrtest(Hanceville.a, Hanceville.null)

Hanceville.b <- glm(PLI.f ~ Slope  ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.b)
lrtest(Hanceville.b, Hanceville.null)

Hanceville.c <- glm(PLI.f ~ Elevation ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.c)
lrtest(Hanceville.c, Hanceville.null)

Hanceville.d <- glm(PLI.f ~ Solar_Radiation ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.d)
lrtest(Hanceville.d, Hanceville.null)

Hanceville.e <- glm(PLI.f ~ TWI ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.e)
lrtest(Hanceville.e, Hanceville.null)

Hanceville.f <- glm(PLI.f ~ BEC_Zone ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.f)
lrtest(Hanceville.f, Hanceville.null)

Hanceville.g <- glm(PLI.f ~ BEC_Subzone ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.g)
lrtest(Hanceville.g, Hanceville.null)

Hanceville.h <- glm(PLI.f ~ PARENT_SOILS ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.h)
lrtest(Hanceville.h, Hanceville.null)

Hanceville.i <- glm(PLI.f ~ Latitude,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.i)
lrtest(Hanceville.i, Hanceville.null)

Hanceville.j <- glm(PLI.f ~ Longitude,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.j)
lrtest(Hanceville.j, Hanceville.null)

Hanceville.k <- glm(PLI.f ~ Slope + I(Slope*sin(Aspect*(pi/180))) + I(Slope*cos(Aspect*(pi/180))) ,
                  family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.k)
lrtest(Hanceville.k, Hanceville.null)

Hanceville.l <- glm(PLI.f ~ SOILNAME_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.l)
lrtest(Hanceville.l, Hanceville.null)

Hanceville.m <- glm(PLI.f ~ TEXTURE_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.m)
lrtest(Hanceville.m, Hanceville.null)

Hanceville.n <- glm(PLI.f ~ PERCENT_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.n)
lrtest(Hanceville.n, Hanceville.null)

Hanceville.o <- glm(PLI.f ~ DRAIN_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.o)
lrtest(Hanceville.o, Hanceville.null)

hanceville$COFRAG_1 <- ifelse(is.na(hanceville$COFRAG_1), 0, hanceville$COFRAG_1)

Hanceville.p <- glm(PLI.f ~ COFRAG_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.p)
lrtest(Hanceville.p, Hanceville.null)

####


Hanceville.1a <- glm(PLI.f ~ BEC_Subzone,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1a)
lrtest(Hanceville.1a, Hanceville.a)

Hanceville.1b <- glm(PLI.f ~ BEC_Subzone + Elevation, 
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1b)
lrtest(Hanceville.1b, Hanceville.1a)

Hanceville.1c <- glm(PLI.f ~ BEC_Subzone + PARENT_SOILS,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1c)
lrtest(Hanceville.1c, Hanceville.1a)

Hanceville.1d <- glm(PLI.f ~ BEC_Subzone + TEXTURE_1,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1d)
lrtest(Hanceville.1d, Hanceville.1a)

Hanceville.1e <- glm(PLI.f ~ BEC_Subzone + TEXTURE_1 + Slope,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1e)
lrtest(Hanceville.1e, Hanceville.1d)

Hanceville.1f <- glm(PLI.f ~ BEC_Subzone +  Slope,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1f)
lrtest(Hanceville.1e, Hanceville.1f)

Hanceville.1g <- glm(PLI.f ~ BEC_Subzone + Slope + PARENT_SOILS,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1g)
lrtest(Hanceville.1g, Hanceville.1e)

Hanceville.1h <- glm(PLI.f ~ BEC_Subzone + TEXTURE_1 + Slope + I(Slope*sin(Aspect*(pi/180))) + I(Slope*cos(Aspect*(pi/180)))  ,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1h)
lrtest(Hanceville.1h, Hanceville.1e)

Hanceville.1i <- glm(PLI.f ~ BEC_Subzone + TEXTURE_1 + Slope + Solar_Radiation,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1i)
lrtest(Hanceville.1i, Hanceville.1e)

Hanceville.1j <- glm(PLI.f ~ BEC_Subzone + TEXTURE_1 + Slope + Elevation,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1j)
lrtest(Hanceville.1j, Hanceville.1e)

Hanceville.1k <- glm(PLI.f ~ BEC_Subzone + TEXTURE_1 + Slope + DRAIN_1,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1k)
lrtest(Hanceville.1k, Hanceville.1e)

hanceville.full <- glm(PLI.f ~ BEC_Subzone  + Slope ,
                   family=binomial(link = "logit"), data=hanceville)
summary(hanceville.full)
lrtest(hanceville.full, Hanceville.1e)

hanceville$yhat.PLI.full <- fitted(hanceville.full)
pROC::auc(hanceville$PLI.f, hanceville$yhat.PLI.full)

hanceville$yhat.PLI.1e <- fitted(Hanceville.1e)
pROC::auc(hanceville$PLI.f, hanceville$yhat.PLI.1e)

Hanceville.1e <- glm(PLI.f ~ BEC_Subzone + DRAIN_1 + Slope,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.1e)
lrtest(Hanceville.1e, Hanceville.1d)

Hanceville.full <- glm(PLI.f ~ BEC_Subzone + PARENT_SOILS + Slope,
                     family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.full)
lrtest(Hanceville.1e, Hanceville.1d)

ggplot(hanceville, aes(x=Latitude, y=Longitude)) +
  geom_point(aes(colour = BEC_Subzone, shape = PARENT_SOILS, size = as.factor(PLI.f))) +
  theme_classic()

###
PLI_base <- glm(PLI.f ~ BEC_Subzone +  Slope,
                family=binomial(link = "logit"), data=hanceville)
summary(PLI_base)

PLI_parent <- glm(PLI.f ~ BEC_Subzone + PARENT_SOILS + Slope,
                       family=binomial(link = "logit"), data=hanceville)
summary(PLI_parent)
lrtest(PLI_parent, PLI_base)

PLI_texture <- glm(PLI.f ~ BEC_Subzone + TEXTURE_1 + Slope,
                  family=binomial(link = "logit"), data=hanceville)
summary(PLI_texture)
lrtest(PLI_texture, PLI_base)

PLI_drain <- glm(PLI.f ~ BEC_Subzone + DRAIN_1 + Slope,
                   family=binomial(link = "logit"), data=hanceville)
summary(PLI_drain)
lrtest(PLI_drain, PLI_base)

PLI_cofrag <- glm(PLI.f ~ BEC_Subzone + COFRAG_1 + Slope,
                   family=binomial(link = "logit"), data=hanceville)
summary(PLI_cofrag)
lrtest(PLI_cofrag, PLI_base)

hanceville %>% 
  filter(!is.na(COFRAG_1)) %>% 
  summarise(avg = mean(COFRAG_1))

hanceville$COFRAG_1 <- ifelse(hanceville$SOILNAME_1 == "TYEE", 20, hanceville$COFRAG_1)
hanceville$COFRAG_1 <- ifelse(hanceville$SOILNAME_1 == "CHASM", 23, hanceville$COFRAG_1)

###
FDI_base <- glm(FDI.f ~ BEC_Subzone ,
                family=binomial(link = "logit"), data=hanceville)
summary(FDI_base)

FDI_parent <- glm(FDI.f ~ BEC_Subzone + PARENT_SOILS,
                  family=binomial(link = "logit"), data=hanceville)
summary(FDI_parent)
lrtest(FDI_base, FDI_parent)

FDI_texture <- glm(FDI.f ~ BEC_Subzone + TEXTURE_1,
                  family=binomial(link = "logit"), data=hanceville)
summary(FDI_texture)
lrtest(FDI_base, FDI_texture)

FDI_drain <- glm(FDI.f ~ BEC_Subzone + DRAIN_1,
                   family=binomial(link = "logit"), data=hanceville)
summary(FDI_drain)
lrtest(FDI_base, FDI_drain)

FDI_cofrag <- glm(FDI.f ~ BEC_Subzone + COFRAG_1,
                   family=binomial(link = "logit"), data=hanceville)
summary(FDI_cofrag)
lrtest(FDI_base, FDI_cofrag)

###
Hanceville.F.null <- glm(FDI.f ~ 1 ,
                       family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.null)

Hanceville.F.a <- glm(FDI.f ~ years_since ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.a)
lrtest(Hanceville.F.a, Hanceville.F.null)

Hanceville.F.b <- glm(FDI.f ~ Slope  ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.b)
lrtest(Hanceville.F.b, Hanceville.F.null)

Hanceville.F.c <- glm(FDI.f ~ Elevation ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.c)
lrtest(Hanceville.F.c, Hanceville.F.null)

Hanceville.F.d <- glm(FDI.f ~ Solar_Radiation ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.d)
lrtest(Hanceville.F.d, Hanceville.F.null)

Hanceville.F.e <- glm(FDI.f ~ TWI ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.e)
lrtest(Hanceville.F.e, Hanceville.F.null)

Hanceville.F.f <- glm(FDI.f ~ BEC_Zone ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.f)
lrtest(Hanceville.F.f, Hanceville.F.null)

Hanceville.F.g <- glm(FDI.f ~ BEC_Subzone ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.g)
lrtest(Hanceville.F.g, Hanceville.F.null)

Hanceville.F.h <- glm(FDI.f ~ PARENT_SOILS ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.h)
lrtest(Hanceville.F.h, Hanceville.F.null)

Hanceville.F.i <- glm(FDI.f ~ Latitude,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.i)
lrtest(Hanceville.F.i, Hanceville.F.null)

Hanceville.F.j <- glm(FDI.f ~ Longitude,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.j)
lrtest(Hanceville.F.j, Hanceville.F.null)

Hanceville.F.k <- glm(FDI.f ~ Slope + I(Slope*sin(Aspect*(pi/180))) + I(Slope*cos(Aspect*(pi/180))) ,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.k)
lrtest(Hanceville.F.k, Hanceville.F.null)

Hanceville.F.l <- glm(FDI.f ~ SOILNAME_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.l)
lrtest(Hanceville.F.l, Hanceville.F.null)

Hanceville.F.m <- glm(FDI.f ~ TEXTURE_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.m)
lrtest(Hanceville.F.m, Hanceville.F.null)

Hanceville.F.n <- glm(FDI.f ~ PERCENT_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.n)
lrtest(Hanceville.F.n, Hanceville.F.null)

Hanceville.F.o <- glm(FDI.f ~ DRAIN_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.o)
lrtest(Hanceville.F.o, Hanceville.F.null)

Hanceville.F.p <- glm(FDI.f ~ COFRAG_1,
                    family=binomial(link = "logit"), data=hanceville)
summary(Hanceville.F.p)
lrtest(Hanceville.F.p, Hanceville.F.null)

ggplot(hanceville, aes(x=Latitude, y=Longitude)) +
  geom_point(aes(colour = BEC_Subzone, shape = TEXTURE_1, size = as.factor(PLI.f))) +
  theme_classic()
