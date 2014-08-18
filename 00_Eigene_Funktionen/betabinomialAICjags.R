betabinomialAICjags <- function(X, Y, jagsOUT, n) {
	# Function to get an AIC from a beta-binomial model in JAGS that matches
	# with frequentist approaches (like from the aodml()-function in the 
	# aods3-package).
	#
	# Inputs are: 
	#
	# X       = model.matrix from JAGS-data
	# Y       = Dependent variable from JAGS-data
	# jagsOUT = JAGS-object (stripped from the "xxx$BUGSoutput"-part!)
	# n       = number of trials for the binomial-distribution...
	
	Beta  <- jagsOUT$mean$beta
	theta <- jagsOUT$mean$theta
	
	Eta  <- as.vector(X %*% Beta)
	
	Pi <- exp(Eta) / (1+exp(Eta))
	Shape1 <- theta * Pi
	Shape2 <- theta * (1-Pi)
	DevianceMean <- -2 * sum(lgamma(theta) - lgamma(Pi * theta) -
													 lgamma((1 - Pi) * theta) + lgamma(n +1) -
													 lgamma(Y + 1) - lgamma((n - Y) + 1) +
													 lgamma(Y + Pi * theta) +
													 lgamma(n - Y + (1 - Pi) * theta) -
													 lgamma(n + theta))
	p <- length(Beta) + length(theta)
	AIC <- DevianceMean + 2 * p
	return(AIC)
}