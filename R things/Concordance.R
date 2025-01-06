#####################
# Concordance Tests #
#####################

#### To load this Concordance Function into your R session to use it, you must 
#### RUN FROM HERE.... 
# Concordance Function for Logistic Regression Models in R
# Source: https://gist.github.com/inkhorn/2151594
# Assuming the input is a stored binomial GLM object
# Modified by Richard Schuster (mail@richard-schuster.com)
# 2014-03-24
Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones[,1])*length(zeros[,1]))
  disc = rep(NA, length(ones[,1])*length(zeros[,1]))
  ties = rep(NA, length(ones[,1])*length(zeros[,1]))
  cnt <- 1
  
  ones.m <- as.matrix(ones)
  zeros.m <- as.matrix(zeros)
  for (ii in 1:length(ones.m[,1])) {
    for (jj in 1: length(zeros.m[,1])){
      # This tests for concordance
      if (ones.m[ii,2] > zeros.m[jj,2])
      {conc[cnt] = 1
      disc[cnt] = 0
      ties[cnt] = 0}
      # This tests for a tie
      else if (ones.m[ii,2] == zeros.m[jj,2])
      {
        conc[cnt] = 0
        disc[cnt] = 0
        ties[cnt] = 1
      }
      # This should catch discordant pairs.
      else if (ones.m[ii,2] < zeros.m[jj,2])
      {
        conc[cnt] = 0
        disc[cnt] = 1
        ties[cnt] = 0
      }
      cnt <- cnt + 1
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
}
## TO HERE to add in the function to get the counts of correct/incorrect

Concordance(structure.PLI.5a)
Concordance(structure.FDI.3j )
Concordance(site.PLI.2j)
Concordance(site.FDI.2g)
Concordance(climate.PLI.2d)
Concordance(climate.FDI.2f)
Concordance(PLI.6)
Concordance(FDI.6)
