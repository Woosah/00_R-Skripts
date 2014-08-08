## load boot package
require(boot)
## output coefficients from your original model
## these can be used as starting values for your bootstrap model
## to help speed up convergence and the bootstrap
dput(round(coef(zeroinfl.fit, "count"), 3))
dput(round(coef(zeroinfl.fit, "zero"), 3))

## function to pass to the boot function to fit your model
## needs to take data, an index (as the second argument!) and your new data
f <- function(data, i, newdata) {
	require(pscl)
	m <- zeroinfl(count ~ child + camper | persons, data = data[i, ],
								start = list(count = c(1.598, -1.0428, 0.834), zero = c(1.297, -0.564)))
	mparams <- as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
	yhat <- predict(m, newdata, type = "response")
	return(c(mparams, yhat))    
}

## set the seed and do the bootstrap, make sure to set your number of cpus
## note this requires a fairly recent version of R
set.seed(10)
res <- boot(dat, f, R = 1200, newdata = Predict, parallel = "snow", ncpus = 4)

## get the bootstrapped percentile CIs
## the 10 here is because in my initial example, there were 10 parameters before predicted values
yhat <- t(sapply(10 + (1:nrow(Predict)), function(i) {
	out <- boot.ci(res, index = i, type = c("perc"))
	with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))

## merge CIs with predicted values
Predict<- cbind(Predict, yhat)