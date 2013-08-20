##################################################################################
## Plot predicted probabilities over simulated range of data after Logit models ##
############# Using the turnout model from Simon Jackman's examples ##############
######################### Workflow for R2Jags/R2WinBUGS ##########################
##################################################################################

## Johannes Karreth
## johannes.karreth@colorado.edu
## 7/8/2012

## Fit your Bayesian model, monitor the coefficients (in this example, named b[]) 
## and the cut points (in this example, named theta[])
## and if you like, the predicted probability of y=1 for each case (in this example, named p[])

#####################################################
## FIRST, PREDICTED PROBABILITIES ON OBSERVED DATA ##
#####################################################

## In R, create matrix of observed data, containing all variables in your model
turnout.dat <- as.data.frame(cbind(y, educ, age, south, govelec, closing))

## WINBUGS USERS: copy and paste 'stats' for p[i] from BUGS into R

## R2WINBUGS/R2JAGS USERS, extract the posterior distributions from your jags/bugs object:
turnout.out <- turnoutfit$BUGSoutput$summary

## OR: WINBUGS/JAGS users, read in your coda files.
turnout.out <- rbind(read.coda("turnout_chain1.txt", "turnout_index.txt"), 
	read.coda("turnout_chain2.txt", "turnout_index.txt"))
	
## Define vector of predicted probabilities on observed data points
p <- turnout.out[8:3007, 2]

## Plot predicted probability (of y=1) against *observed* values of age \
## (most likely an ugly plot, b/c we have several y(x_i))
plot(p[1]~turnout.dat.dat$age,xlim=c(min(turnout.dat.dat$age), max(turnout.dat.dat$age)))

#################################################################
######### SECOND, OUT-OF-SAMPLE PREDICTED PROBABILITIES, ########
## across the range of X1 and given values of other covariates ##
#################################################################

## Import the chains containing the coefficients from your BUGS model, 
## after monitoring *only* the coefficients (in this example, named b)

## R2JAGS/R2WINBUGS USERS:
turnout.mcmc <- as.mcmc(turnoutfit)
turnout.mat <- as.matrix(turnout.mcmc)
b <- turnout.mat[ , 1:6] ## one column for each coefficient, in this case I had 6 coefficients

## OR: BUGS/JAGS USERS: READ IN YOUR CODA FILES
turnout.chains <- rbind(read.coda("turnout_chain1.txt", "turnout_index.txt"), 
	read.coda("turnout_chain2.txt", "turnout_index.txt"))
b <- turnout.chains[ , 1:6]

# Generate vector with the simulated range of X1 (here, age)
new.age <- seq(min(turnout.dat$age), max(turnout.dat$age))

# Generate vectors set at desired values of the other covariates
new.education <- rep(median(turnout.dat$educ), length(new.age))
new.closing <- rep(median(turnout.dat$closing), length(new.age))
new.govelec <- rep(median(turnout.dat$govelec), length(new.age))
new.south <- rep(median(turnout.dat$south), length(new.age))
# Need value of 1 for the constant
constant <- rep(1, length(new.age))

# Generate dataframe with simulated values
turnout.sim <- cbind(constant,new.education,new.age, new.south, new.govelec, new.closing)   ## cbind: combine (bind) columns

# Or: generate two dataframes to plot PPs for each value of the South dummy (continued further below)
turnout.sim.s <- cbind(constant,new.education,new.age,rep(1,max(turnout.dat$age)-min(turnout.dat$age)+1), new.govelec, new.closing)
turnout.sim.n <- cbind(constant,new.education,new.age, rep(0,max(turnout.dat$age)-min(turnout.dat$age)+1), new.govelec, new.closing)

# Multiply X by the betas from your BUGS output
Xb <- t(turnout.sim%*% t(b))

# Transform linear prediction to probability
turnout.pp.age <- exp(Xb)/(1+exp(Xb))

# Get CIs (for plotting)
turnout.ci.age <- apply(turnout.pp.age, 2, quantile, probs=c(.025,.975)) ## apply(a, b, c): apply function (c) to object(a), by(b: 1 for row, 2 for column)

# Get mean predictions over the n (from BUGS/JAGS iterations) sampled values of b
mean.turnout.pp.age <- apply(turnout.pp.age, 2, mean)
mean.turnout.ci.age <- apply(turnout.ci.age, 2, quantile, probs=c(.025,.975))

# Plot mean probability against the full (simulated) range of X (=age)
plot(new.age, mean.turnout.pp.age, pch=19, main="Predicted probability of voting", xlab="Age", ylab="Pr(Voting)", xlim=c(min(turnout.dat$age), max(turnout.dat$age)), ylim=c(0,1))

# Add standard errors as vertical lines (could also do this using 2.5% and 97.5% values from p.chains)
segments(new.age, mean.turnout.ci.age[1, ], new.age, mean.turnout.ci.age[2, ], lty=1)

## Continue two predictions for south=[0,1]

# Multiply X by the betas from your BUGS output
Xb.s <- t(turnout.sim.s %*% t(b))
Xb.n <- t(turnout.sim.n %*% t(b))

# Transform linear prediction to probability
turnout.pp.age.s <- exp(Xb.s)/(1+exp(Xb.s))
turnout.pp.age.n <- exp(Xb.n)/(1+exp(Xb.n))

# Get mean linear predictions & SDS over the n (from BUGS iterations) sampled values of b
mean.turnout.pp.age.s <- apply(turnout.pp.age.s, 2, mean)
mean.turnout.pp.age.n <- apply(turnout.pp.age.n, 2, mean)
turnout.s.ci <- apply(turnout.pp.age.s, 2, quantile, probs=c(.025,.975))
turnout.n.ci <- apply(turnout.pp.age.n, 2, quantile, probs=c(.025,.975))

#####################
## Plot 1 (simple) ##
#####################

# Plot mean probability against the full (simulated) range of X (=age)
adjust <- rep(.3, max(turnout.dat$age)-min(turnout.dat$age)+1)  ## slightly adjust the position of the "North" points to avoid overlay
plot(new.age, mean.turnout.pp.age.s, pch=19, main="Predicted probability of voting", xlab="Age", ylab="Pr(Voting)", col="red", xlim=c(min(turnout.dat$age), max(turnout.dat$age)), ylim=c(0,1))
points(new.age + adjust, mean.turnout.pp.age.n, pch=19, col="black")
segments(new.age, turnout.s.ci[1, ], new.age, turnout.s.ci[2, ], lty=1, col="red")
segments(new.age + adjust, turnout.n.ci[1, ], new.age + adjust, turnout.n.ci[2, ], lty=1, col="black")
legend("bottomright", c("South", "Rest of the U.S."), col=c("red", "black"), pch=19, inset=.01, bty="n")

#####################
## Plot 2 (panels) ##
#####################

## Generate data set used for the two plots below
plot.dat <- data.frame(
	means = c(mean.turnout.pp.age.s , mean.turnout.pp.age.n), 	## means of the pred. probabilities
	lower = c(turnout.s.ci[1, ] , turnout.n.ci[1, ]), 	## upper CI
	upper = c(turnout.s.ci[2, ], turnout.n.ci[2, ]),		## lower CI
	south = factor(rep(c(1,0), each=max(turnout.dat$age)-min(turnout.dat$age)+1), levels=c(1,0), labels=c("South", "Rest of the U.S.")),  ## Outcome variable
	age = rep(new.age, 2))	## Explanatory variable of interest (here: age)
	
xyplot(means ~ age | south, data=plot.dat, as.table=T, 
	ylim=c(min(plot.dat$lower), max(plot.dat$upper)), xlab="Age", ylab="Pr(Voting)", main="Probability of Voting",
	panel = function(x,y,subscripts){
		panel.lines(x,y,lty=1, col="black")
		panel.lines(x, plot.dat$lower[subscripts], lty=2, col="red")
		panel.lines(x, plot.dat$upper[subscripts], lty=2, col="red")})

#################################
## Plot 3 (transparent colors) ##
#################################

xyplot(mean.turnout.pp.age.s ~ new.age, ylim=c(0,1), xlab="Age", ylab="Pr(Voting)", main="Probability of voting",
	key=list(space=list("right"), rectangles=list(col=c(rgb(1,0,0, alpha=.35), rgb(0,0,1, alpha=.35))), text=list(c("South", "Rest of the U.S."))),
	panel=function(x,y){
		panel.polygon(x=c(x,rev(x),x[1]), y=c(turnout.s.ci[1,], rev(turnout.s.ci[2,]), turnout.s.ci[1,1]), 
			col=rgb(1,0,0,alpha=.35), border=NA)
		panel.polygon(x=c(x,rev(x),x[1]), y=c(turnout.n.ci[1,], rev(turnout.n.ci[2,]), turnout.n.ci[1,1]), 
			col=rgb(0,0,1,alpha=.35), border=NA)
		panel.lines(x, mean.turnout.pp.age.s, col="red")
		panel.lines(x, mean.turnout.pp.age.n, col="blue")
		})