###########################################
## Bayesian regression results as tables ##
###### Workflow for R2Jags/R2WinBUGS ######
###########################################

## Johannes Karreth
## johannes.karreth@colorado.edu
## 7/5/2012

## if angellfit is your R2jags/R2WinBUGS output object
## use John Baumgartner's jagstools package:

regtable <- jagsresults(x = angellfit, params = c("alpha", "beta1", "beta2"), exact = FALSE)

## pick out the parameters of interest (here, the coefficients and SDs)

regtable1 <- regtable[, 1:2]

regtable1 <- xtable(regtable1)
print(regtable1, type = "latex")
print(regtable1, type = "html")

## you can save the HTML table as a .html file and open it with MS word. You might have to make some modifications, but the basic layout should be functionable.

## pick out the parameters of interest (here, the coefficients and CIs)

regtable2 <- regtable[, c(1,3,7)]

xtable(regtable3)







