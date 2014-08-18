residPearson <- function(x,...) UseMethod("residPearson")
residPearson.glmmadmb <- function(Object, ...) {
	
	dist   <- Object$family
	linkfn <- Object$link
	zeroTF <- Object$zeroInflation
	reTF   <- is.null(Object$S)
	nbinom1Family <- any(grepl("nbinom1", Object$call))
	
	if (!reTF) stop("residPearson is not implemented for models with random effects!")
	
	if (dist == "gaussian") {
		stop("There seem to be problems for the default residual methods\nfor the gaussian distribution in glmmADMB...sorry!")
	
	} else if (dist == "gamma") {
		stop("There seem to be problems for the default residual methods\nfor the gamma distribution in glmmADMB...sorry!")

	} else if (dist == "truncpoiss") {
		stop("residPearson is not implemented for truncated poisson models!")
	
	} else if (dist == "truncnbinom" && nbinom1Family == FALSE) {
		stop("residPearson is not implemented for truncated negative-binomial models!")
	
	} else if (dist == "truncnbinom" && nbinom1Family == TRUE) {
		stop("residPearson is not implemented for truncated negative-binomial-1 models!")

	} else if (dist == "beta") {
		stop("residPearson is not implemented for models using the beta distribution!")
		
	} else 	if (dist == "logistic") {
		stop("There seem to be problems for the default residual methods\nfor the logistic distribution in glmmADMB...sorry!")
		
	} else if (dist == "binom") { # Normal behavior from residuals.glmmadmb()-function
		Residuals <- Object$residuals/Object$sd.est
		if (zeroTF == TRUE) warning("zeroInflation seems to be silently ignored for binomial models!")
		
	} else if (dist == "betabinom") {
		if (linkfn != "logit") stop("residPearson is not implemented for beta-binomial models without the canonical logit-link!")
		if (zeroTF == TRUE) warning("zeroInflation seems to be silently ignored for beta-binomial models!")
		AV <- Object[["frame"]][[1]][,1]
		Data <- Object[["frame"]]
		Ntrials <- sum(Object[["frame"]][1]) / nrow(Object[["frame"]][1])
		Beta  <- coef(Object)
		theta <- Object[["alpha"]]
		Matrix <- model.matrix(Object[["formula"]], data = Data)
		Eta  <- as.vector(Matrix %*% Beta)
		Pi <- exp(Eta) / (1+exp(Eta))
		Exp <- Ntrials * Pi
		Var <- Ntrials * Pi * (1 - Pi) * (1 + ((Ntrials - 1) / (theta + 1)))
		Residuals <- (AV - Exp) / sqrt(Var)
		
	} else if (dist == "poisson" && zeroTF == FALSE) { # Normal behavior...
		Residuals <- Object$residuals/Object$sd.est
		
	} else if (dist == "poisson" && zeroTF == TRUE) { # Pearson residuals as calculated by pscl
		if (linkfn != "log") stop("residPearson is not implemented for zeroinflated\npoisson models without the canonical log-link!")
		AV <- Object[["frame"]][1]
		Data <- Object[["frame"]]
		Eta  <- Object[["fitted"]]
		Beta  <- coef(Object)
		Matrix <- model.matrix(Object[["formula"]], data = Data)
		mu <- exp(as.vector(Matrix %*% Beta))
		psi <- Object[["pz"]]
		Var <- Eta * (1 + (psi) * mu)
		Residuals <- (AV - Eta) / sqrt(Var)		

	} else if (dist == "nbinom" && nbinom1Family == TRUE && zeroTF == FALSE) {
		Residuals <- Object$residuals/Object$sd.est
		warning("Be careful: I did not evaluate if the negative-binomial-1\ndistribution returns pearson residuals correctly!")
				
	} else if (dist == "nbinom" && nbinom1Family == TRUE && zeroTF == TRUE) {
		stop("residPearson is not implemented for zeroinflated negative-binomial-1 models!")
		
	} else if (dist == "nbinom" && nbinom1Family == FALSE && zeroTF == FALSE) { # Normal behavior...
		Residuals <- Object$residuals/Object$sd.est
		
	}	else if (dist == "nbinom" && nbinom1Family == FALSE && zeroTF == TRUE) {
		if (linkfn != "log") stop("residPearson is not implemented for zeroinflated\nnegative-binomial models without the canonical log-link!")
		AV <- Object[["frame"]][1]
		Data <- Object[["frame"]]
		Eta  <- Object[["fitted"]]
		Beta  <- coef(Object)
		Matrix <- model.matrix(Object[["formula"]], data = Data)
		mu <- exp(as.vector(Matrix %*% Beta))
		psi <- Object[["pz"]]
		theta1 <- 1 / Object[["alpha"]]
		Var <- Eta * (1 + (psi + theta1) * mu)
		Residuals <- (AV - Eta) / sqrt(Var)
	}
	unlist(Residuals)
}