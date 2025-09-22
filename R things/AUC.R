#######
# AUC #
#######

#install.packages("Metrics")
#install.packages("pROC")
library(pROC)
library(Metrics)


#Structure
bin_dis_GIS1$yhat.PLI.structure <- fitted(structure.PLI.5a)
bin_dis_GIS1$yhat.FDI.structure <- fitted(structure.FDI.3j)

pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI.structure)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI.structure)

#Site

bin_dis_GIS1$yhat.PLI.site <- fitted(site.PLI.2j)
bin_dis_GIS1$yhat.FDI.site <- fitted(site.FDI.3k)



pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI.site)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI.site)



#Climate
bin_dis_GIS1$yhat.PLI.climate <- fitted(climate.PLI.2d)
bin_dis_GIS1$yhat.FDI.climate<- fitted(climate.FDI.2f)

pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI.climate)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI.climate)


#Combo
bin_dis_GIS1$yhat.PLI <- fitted(PLI.6)
bin_dis_GIS1$yhat.FDI <- fitted(FDI.6)

pROC::auc(bin_dis_GIS1$PLI.f, bin_dis_GIS1$yhat.PLI)
plot(pROC::roc(bin_dis_GIS1$PLI.f,bin_dis_GIS1$yhat.PLI),auc.polygon=T)
pROC::auc(bin_dis_GIS1$FDI.f, bin_dis_GIS1$yhat.FDI)
plot(pROC::roc(bin_dis_GIS1$FDI.f,bin_dis_GIS1$yhat.FDI),auc.polygon=T)

#Hanceville
hanceville$yhat.PLI <- fitted(PLI_base)
pROC::auc(hanceville$PLI.f, hanceville$yhat.PLI)


