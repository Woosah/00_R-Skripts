############################################################################################
#### Plot predicted probabilities and other things after a Bayesian ordered logit model ####
############################################################################################

## Johannes Karreth
## johannes.karreth@colorado.edu
## 7/8/2012

## Based on code by Dave Armstrong, armstrod@uwm.edu

library(foreign)
library(lattice)
library(coda)
setwd("~/R/Bayes/hw5/ologit")

## Read in two functions. dump.format and read.coda (originally by Dave Armstrong)
source("http://sobek.colorado.edu/~joka5204/Bayes/data_handling.R")

## Observed data for the model
dat <- read.dta("~/R/Bayes/hw5/ordered.logit.dta")
dat <- na.omit(dat[, c("interact", "orgmembs", "indmembs", "age", "taxexmpt")])

## JAGS/BUGS USERS: Read the coda output from Bugs or Jags into R
chains <- rbind(read.coda("ologit_chain1.txt", "ologit_index.txt"), 
	read.coda("ologit_chain2.txt", "ologit_index.txt"))
	
## Or: R2JAGS/R2WINBUGS USERS, if hw5fit is your R2jags/R2WinBUGS object:
chains <- hw5fit$BUGSoutput$sims.matrix

## Be sure to remember the order of your coefficients. In this case:
## b1: orgmembs
## b2: indmembs
## b3: age
## b4: taxexmpt
	
#####################################################
## FIRST, PREDICTED PROBABILITIES ON OBSERVED DATA ##
#####################################################

## Define vectors with the values of the predicted probabilities (pulled from the coda files)
## grep("p[",) pulls all columns that start with p[
probs <- chains[, grep("p[", colnames(chains), fixed=T)]
## Note: this will be in the order p[observation, outcome category]

## Now, make a new list with n.iter (here, 2000) elements, 
## where each is a matrix of the probability of being in one of the categories,
## hence ncol = number of categories (here, 4) and nrow (not specified) = 772 (N of 
## the original data.
prob.list <- lapply(1:2000, function(x)matrix(probs[x, ], ncol=4))	## replace 2000 with the N of your simulations (if several chains, the N must be the number of all iterations combined, i.e. the N of chain1 + the N of chain2, etc.)

##
## Summarize classification and proportional reduction of error (PRE)
##

## Identify the predicted category (simply the largest value of the four columns)
pred.cats <- sapply(prob.list, function(x)apply(x, 1, which.max))

## % correctly predicted: average # of simulations where pred.cats = observed outcome
## Here, dat is the dataframe of the observed data, and interact is the observed DV
pcp <- apply(pred.cats, 2, function(x)mean(x == as.numeric(dat$interact)))

## PMC: percentage of observations in the modal category of the observed data
## This would be the ``naive'' guess in a null model - simply predict the modal category
pmc <- max(table(as.numeric(dat$interact))/sum(table(as.numeric(dat$interact))))

## PRE is defined as \frac{PCP - PMC}{1 - PMC}, so use this formula
pre <- (pcp - pmc)/(1-pmc)

## Neat: remember, we are doing all this over a list of 2000 simulations
## Hence, PRE is actually a distribution
summary(pre)
mean(pre > 0)

## Again, PRE is a distribution
plot(density(pre, bw=.05))
## Terrible model - but recall that almost all obs. had interact=={4}
## So the modal null model should already fare pretty well.

## For an alternative to PRE, see Herron in Political Analysis 8(1), 1999
## Suggests ePRE (more details in that article)
pr.y <- sapply(prob.list, function(x)x[cbind(1:nrow(x), as.numeric(dat$interact))])
epcp <- apply(pr.y, 2, mean)
epmc <- mean((table(dat$interact)/sum(table(dat$interact)))[as.numeric(dat$interact)])
epre <- (epcp-epmc)/(1-epmc)
plot(density(pre, bw=.05), xlab="", ylab="Density", main="")
lines(density(epre, bw=.05), lty=2)
legend("topright", c("PRE", "ePRE"), lty=c(1,2), inset=.01)

## Again, note the nice fact that we have a distribution for PRE, 
## and thus we can evaluate it on different grounds.

## How does the Bayesian output fare compared to the frequentist model?

freq.ologit <- polr(as.factor(interact) ~ orgmembs + indmembs + age + taxexmpt, data = dat)
summary(freq.ologit)

## Use the pre() function from Dave Armstrong's DAMisc package:
pre(freq.ologit)

## PRE and ePRE should be quite similar to the posterior means from the Bayesian model.

#################################################################
######### SECOND, OUT-OF-SAMPLE PREDICTED PROBABILITIES, ########
## across the range of X1 and given values of other covariates ##
#################################################################

## Generate dataset with simulated data (i.e. where your explanatory variables are set to min->max or held constant)
newdat <- data.frame(
	age = seq(min(dat$age), max(dat$age), length=145),	## length: number of individual values of this continuous expl. var.
	orgmembs = median(dat$orgmembs),
	indmembs = median(dat$indmembs),
	taxexmpt = median(dat$taxexmpt)
)

## Define vectors with the values of the coefficients (pulled from the coda files)
## in this case, note that all my coefficients were named b[j], where j = number of coefficients
b <- chains[,grep("b[", colnames(chains), fixed=T)]	

## Define X matrix (explanatory variables)
X <- model.matrix(~orgmembs + indmembs + age + taxexmpt, data=newdat)
X <- X[,-1]

## Multiply X by the betas from your JAGS/BUGS output

Xb <- t(X %*% t(b))

## Define vectors with the values of the cutoff points (pulled from the coda files)
## Note that in my model, the cutpoints were called theta
## Theta will only be in your chains if you monitored them earlier
kaps <- chains[, grep("theta", colnames(chains), fixed=T)]
q1 <- plogis(kaps[,1] - Xb)
q2 <- plogis(kaps[,2] - Xb)
q3 <- plogis(kaps[,3] - Xb)

## Probabilities to be in each of the 4 (j) categories
p1 <- q1
p2 <- q2-q1
p3 <- q3-q2
p4 <- 1-q3

## ... and the respective credible intervals
p1.ci <- apply(p1, 2, quantile, probs=c(.025,.975))
p2.ci <- apply(p2, 2, quantile, probs=c(.025,.975))
p3.ci <- apply(p3, 2, quantile, probs=c(.025,.975))
p4.ci <- apply(p4, 2, quantile, probs=c(.025,.975))

## Get the simulation mean for each quantity of interest
kap.mean <- apply(kaps, 2, mean)
b.mean <- apply(b, 2, mean)
mean.q1 <- plogis(kap.mean[1] - X %*% b.mean)
mean.q2 <- plogis(kap.mean[2] - X %*% b.mean)
mean.q3 <- plogis(kap.mean[3] - X %*% b.mean)
mean.p1 <- mean.q1
mean.p2 <- mean.q2-mean.q1
mean.p3 <- mean.q3-mean.q2
mean.p4 <- 1-mean.q3

## Generate data set used for the two plots below
plot.dat <- data.frame(
	means = c(mean.p1, mean.p2, mean.p3, mean.p4), 	## means of the pred. probabilities
	lower = c(p1.ci[1,] , p2.ci[1,], p3.ci[1,], p4.ci[1,]), 	## upper CI
	upper = c(p1.ci[2,], p2.ci[2,], p3.ci[2,], p4.ci[2,]), 		## lower CI
	interact = factor(rep(c(1,2,3,4), each=145), levels=c(1,2,3,4), labels=c("Never", "Seldom", "Occasionally", "Frequently")),  ## Outcome variable
	age = rep(newdat$age, 4))	## Explanatory variable of interest (here: age)
	
## PLOT 1 (four separate panels for each outcome)
	
xyplot(means ~ age | interact, data=plot.dat, as.table=T, 
	ylim=c(min(plot.dat$lower), max(plot.dat$upper)), xlab="Age", ylab="Probability",
	panel = function(x,y,subscripts){
		panel.lines(x,y,lty=1, col="black")
		panel.lines(x, plot.dat$lower[subscripts], lty=2, col="red")
		panel.lines(x, plot.dat$upper[subscripts], lty=2, col="red")})
		
## PLOT 2 (all outcomes in one panel)	
		
xyplot(mean.p1 ~ newdat$age, ylim=c(0,1), xlab="Age", ylab="Probability",
	key=list(space=list("top"), rectangles=list(col=c(rgb(1,0,0, alpha=.35), rgb(0,1,0, alpha=.35), 
	rgb(0,0,1,alpha=.35), rgb(1,1,0,alpha=.35))), text=list(c("Pr(Interact=Never)", "Pr(Interact=Seldom)", "Pr(Interact=Occasionally)", "Pr(Interact=Frequently)"))),
	panel=function(x,y){
		panel.polygon(x=c(x,rev(x),x[1]), y=c(p1.ci[1,], rev(p1.ci[2,]), p1.ci[1,1]), 
			col=rgb(1,0,0,alpha=.35), border=NA)
		panel.polygon(x=c(x,rev(x),x[1]), y=c(p2.ci[1,], rev(p2.ci[2,]), p2.ci[1,1]), 
			col=rgb(0,1,0,alpha=.35), border=NA)
		panel.polygon(x=c(x,rev(x),x[1]), y=c(p3.ci[1,], rev(p3.ci[2,]), p3.ci[1,1]), 
			col=rgb(0,0,1,alpha=.35), border=NA)
		panel.polygon(x=c(x,rev(x),x[1]), y=c(p4.ci[1,], rev(p4.ci[2,]), p4.ci[1,1]), 
			col=rgb(1,1,0,alpha=.35), border=NA)	
		panel.lines(x, mean.p1, col="red")
		panel.lines(x, mean.p2, col="green")
		panel.lines(x, mean.p3, col="blue")
		panel.lines(x, mean.p4, col="yellow")
	})

#### Questions: Johannes Karreth, johannes.karreth@colorado.edu