########################################
## Bayesian factor scores as dot plot ##
#### Workflow for R2Jags/R2WinBUGS #####
########################################

## Johannes Karreth
## johannes.karreth@colorado.edu
## 7/5/2012

library(lattice)
library(ggplot2)
library(stats)

## if fa.fit.upd is your factor model output from R2jags or R2WinBUGS: 

fa.out <- fa.fit.upd$BUGSoutput$summary

dat <- as.data.frame(fa.out[-1, ])

## name your predicted factor latent.mean, and the CI between latent.lower and latent.upper

dat$latent.mean <- dat[,1]
dat$latent.lower <- dat[,3]
dat$latent.upper <- dat[,7]
dat$subject <- rownames(dat)

## here, take only 50 of the 1500 observations (just for demonstration)
## dat <- dat[sample(1:nrow(dat), 50, replace=FALSE),]

## order the data by the factor scores, for better visualization

dat <- dat[order(dat$latent.mean), ]

## order the observation IDs as well (seems redundant, but is necessary)

dat$subject2 <- reorder(dat$subject, dat$mean)

## define range of the scores, for the axis limits in the plot

rg <- diff(range(c(dat$latent.upper, dat$latent.lower)))

## make plot using the lattice package:

dotplot(subject2 ~ latent.mean, data=dat,scales=list(y=list(cex=.45)), xlim=c(min(dat$latent.lower)-.1*rg, max(dat$latent.upper)+.1*rg),main="Latent trait", panel=function(x,y, subscripts){
    panel.abline(h = as.numeric(y), col = "gray80", lty = 2)
    panel.segments(dat$latent.lower[subscripts], y, dat$latent.upper[subscripts], y, lty=1, col="gray40")
    panel.points(x,y, pch=16, col="black")})
    
## make plot using the ggplot2 package:
    
factorplot <- ggplot(dat, aes(x = latent.mean, y = subject2)) + geom_point() + geom_segment(aes(x = dat$latent.lower, xend = dat$latent.upper, y = dat$subject2, yend = dat$subject2)) + ylab("") + xlab("Latent trait") + opts(title = "Latent trait")
factorplot

