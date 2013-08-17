#############################################
## Bayesian regression results as dot plot ##
######## Workflow for R2Jags/R2WinBUGS ######
#############################################

## Johannes Karreth
## johannes.karreth@colorado.edu
## 7/5/2012

## if angellfit is your R2jags/R2WinBUGS output object, and 
## angellfit.mcmc is that object as mcmc object:

## using the mcmcplots package:

caterplot(angellfit.mcmc, parms = c("beta1", "beta2"), labels = c("Diversity", "Mobility"), val.lim = c(-0.27, 0.05))
abline(v = 0)

## using the arm package:

coef.vect <- angellfit$BUGSoutput$summary[2:3, 1]
sd.vect <- angellfit$BUGSoutput$summary[2:3, 2]
short.names <- rownames(angellfit$BUGSoutput$summary[2:3,])
long.names <- c("Diversity", "Mobility")

library(arm)
coefplot(coef.vect, sd.vect, varnames=long.names, main="", xlim = c(-0.3, 0.05))
