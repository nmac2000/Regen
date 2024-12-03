#######
# AUC #
#######

#install.packages("Metrics")
#install.packages("pROC")
library(pROC)
library(Metrics)

# For FDI
predict.FDI.site <- predict(site.FDI.2f ) 
fitted.FDI.site <- fitted(site.FDI.2f ) 


cutoff <- 0.05 # you can play around with different cutoff values
n <- dim(bin_dis_GIS1)[1]
#bin_dis_GIS1$FDI_count_bin <- as.numeric(levels(bin_dis_GIS1$FDI_count_bin))[bin_dis_GIS1$FDI_count_bin]
obs <- bin_dis_GIS1$FDI_count_bin
pred <- fitted.FDI.site

auc(obs, pred)
GLM2.auc <-pROC::auc(bin_dis_GIS1$FDI_count_bin,bin_dis_GIS1$yhat.GLM2a)
plot(pROC::roc(bin_dis_GIS1$FDI_count_bin,bin_dis_GIS1$yhat.GLM2a),auc.polygon=T)


pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.GLM1)




bin_dis_GIS1$yhat.GLM1 <- fitted(climate.PLI.2d)

#Structure
bin_dis_GIS1$yhat.PLI.structure <- fitted(structure.PLI.3a)
bin_dis_GIS1$yhat.FDI.structure <- fitted(structure.FDI.3i)

pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI.structure)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI.structure)

#Site

bin_dis_GIS1$yhat.PLI.site <- fitted(site.PLI.2j)
bin_dis_GIS1$yhat.FDI.site <- fitted(site.FDI.2f)

pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI.site)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI.site)


#Climate
bin_dis_GIS1$yhat.PLI.climate <- fitted(climate.PLI.2d)
bin_dis_GIS1$yhat.FDI.climate<- fitted(climate.FDI.2e)

pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI.climate)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI.climate)


#Combo
bin_dis_GIS1$yhat.PLI <- fitted(PLI.7a)
bin_dis_GIS1$yhat.FDI <- fitted(FDI.6)

pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI)

