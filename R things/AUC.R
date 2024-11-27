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