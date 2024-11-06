#######
# AUC #
#######

install.packages("Metrics")
library(Metrics)

# For FDI
bin_dis_GIS1$linpred.GLM2a <- predict(site.FDI.2f ) # Estimated logit using the linear part
bin_dis_GIS1$yhat.GLM2a <- fitted(site.FDI.2f ) # Estimated probability using inverse logityhat values
bin_dis_GIS1$resid.GLM2a <- resid(site.FDI.2f ) # the deviances 

cutoff <- 0.05 # you can play around with different cutoff values
n <- dim(bin_dis_GIS1)[1]
#bin_dis_GIS1$FDI_count_bin <- as.numeric(levels(bin_dis_GIS1$FDI_count_bin))[bin_dis_GIS1$FDI_count_bin]
obs <- bin_dis_GIS1$FDI_count_bin
pred <- bin_dis_GIS1$yhat.GLM2a

auc(obs, pred)




###
bin_dis_GIS1$GLM2a <- ifelse( obs == 1 & (obs - pred) == 0, 1, 0) ; a <- sum(bin_dis_GIS1$GLM2a) # a: true positives
bin_dis_GIS1$GLM2c <- ifelse( obs == 1 & (obs - pred) == 1, 1, 0) ; c <- sum(bin_dis_GIS1$GLM2c) # c: false negative
bin_dis_GIS1$GLM2b <- ifelse( obs == 0 & (obs - pred) == -1, 1, 0) ; b <- sum(bin_dis_GIS1$GLM2b) # b: false positives
bin_dis_GIS1$GLM2d <- ifelse( obs == 0 & (obs - pred) == 0, 1, 0) ; d <- sum(bin_dis_GIS1$GLM2d) # d: true negatives

# general confusion matrix layout
matrix(c("a","c" , "b","d"), nrow=2, ncol=2)
CM.GLM2 <- matrix(c(a,c , b,d), nrow=2, ncol=2)
CM.GLM2



# overall accuracy = (a+d)/n
GLM2.oa <- (a+d)/n
GLM2.sensitivity <- a/(a+c)
GLM2.specificity <- d/(b+d)
kappa.bit <- (a+b)*(a+c)+(c+d)*(d+b)
GLM2.kappa <- (  ((a+d)/n)  - (kappa.bit/n^2) ) / (1- (kappa.bit/n^2) )
GLM2.TSS <- GLM2.sensitivity + GLM2.specificity -1

GLM2.predacc <- c(GLM2.oa, GLM2.sensitivity, GLM2.specificity, GLM2.kappa,GLM2.TSS)

GLM2.auc <-pROC::auc(bin_dis_GIS1$FDI_count_bin,bin_dis_GIS1$yhat.GLM2a)
plot(pROC::roc(bin_dis_GIS1$FDI_count_bin,bin_dis_GIS1$yhat.GLM2a),auc.polygon=T)