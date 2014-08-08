# Displaying Odds-Ratios for logistic GLMs with CIs:

OR.glm <- function(x, conf.level = 0.95){
	## x = glm logistic regression model object
	sumx <- summary(x)$coefficients
	Z <- qnorm((1 + conf.level)/2)
	expor <- exp(sumx[,"Estimate"])
	lcl <- exp(sumx[,"Estimate"] - Z*sumx[,"Std. Error"])
	ucl <- exp(sumx[,"Estimate"] + Z*sumx[,"Std. Error"])
	coef <- sumx
	or <- cbind(coef=sumx[,"Estimate"], "exp(coef)"=expor, lcl, ucl)
	list(coef = coef, odds.ratio = or)
}