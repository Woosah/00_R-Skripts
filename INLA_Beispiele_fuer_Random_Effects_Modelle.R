********************************
	
x <- sort(unique(runif(100, 0, 20)))
y1 <- 60 - 2*x + rnorm(length(x), 0, 4)
y2 <- 70 - 0.6*x + rnorm(length(x), 0, 4)
y3 <- 45 + 1*x + rnorm(length(x), 0, 4)
y4 <- 35 + 1.3*x + rnorm(length(x), 0, 4)

plot(x,y1, ylim=c(20,80), xlim=c(0,30))
points(x, y2, col="red")
points(x, y3, col="green")
points(x, y4, col="orange")
abline(a=coef(lm(y1~x))[1], b=coef(lm(y1~x))[2])
abline(a=coef(lm(y2~x))[1], b=coef(lm(y2~x))[2])
abline(a=coef(lm(y3~x))[1], b=coef(lm(y3~x))[2])
abline(a=coef(lm(y4~x))[1], b=coef(lm(y4~x))[2])

mydata <- data.frame(x1 = rep(x, times = 4),
										 y = c(y1,y2,y3,y4),
										 bloc = rep(c(1,2,3,4),
										 					 each = length(x)),
										 fuzzy = rep(1, 400))



require(lme4)
require(INLA)

## no random effect
##

model_no_random <- lm(y ~ x1, data = mydata)
summary(model_no_random)

formula_no_random = y ~ x1

model_no_random_INLA = inla(formula = formula_no_random,
														data = mydata,
														family = "gaussian",
														control.compute=list(dic=TRUE))
summary(model_no_random_INLA)


## random intercept only
##

model_intercept <- lmer( y ~ x1 + (1|bloc), data = mydata)
summary(model_intercept)

formula_random_intercept_only = y ~ x1 + f(bloc, model = "iid")
model_random_intecept_only_INLA <- inla(formula = formula_random_intercept_only,
																				data = mydata,
																				family = "gaussian",
																				control.compute = list(dic=TRUE),
					 															control.fixed = list(prec.intercept = 0.0001))
summary(model_random_intecept_only_INLA)
1/ sqrt(model_random_intecept_only_INLA$summary.hyperpar[,1])

## random slope only
##

model_slope <- lmer( y ~ x1 + (0 + x1|bloc), data = mydata)
summary(model_slope)

formula_random_slope_only = y ~ x1 + f(bloc, x1, model = "iid")

model_random_slope_only_INLA <- inla(formula = formula_random_slope_only,
																		 data = mydata,
																		 family = "gaussian",
																		 control.compute = list(dic=TRUE),
																		 control.fixed = list(prec.intercept = 0.0001))
summary(model_random_slope_only_INLA)
1/ sqrt(model_random_slope_only_INLA$summary.hyperpar[,1])

## random slope of x1 within bloc with correlated intercept
##

model_slope_intercept_correlated <- lmer(y ~ x1 + (x1|bloc), data = mydata)
summary(model_slope_intercept_correlated)

n.block = max(mydata$bloc) ## = 4
mydata$i.intercept = mydata$bloc
mydata$j.intercept = mydata$bloc + n.block  ## see doc for iid2d

formula_random_correlated_intercept_slope = y ~ x1 +
	f(i.intercept,  model="iid2d", n=2*n.block) +
	f(j.intercept, x1, copy="i.intercept")

random_correlated_intercept_slope_INLA = inla(formula = formula_random_correlated_intercept_slope,
																							data = mydata,
																							family = "gaussian",
																							control.compute=list(dic=TRUE))
summary(random_correlated_intercept_slope_INLA)
1/ sqrt(random_correlated_intercept_slope_INLA$summary.hyperpar[1:3,1])

## uncorrelated random intercept and random slope within bloc
##

model_slope_intercept_uncorrelated <- lmer(y ~ x1 + (1|bloc) + (0 + x1| bloc), data = mydata)
summary(model_slope_intercept_uncorrelated)

formula_random_uncorrelated_intercept_slope = y ~ x1 +
	f(i.intercept,  model="iid") + f(j.intercept, x1, model="iid")

random_uncorrelated_intercept_slope_INLA <- inla(formula = formula_random_uncorrelated_intercept_slope,
																								 data = mydata,
																								 family = "gaussian",
																								 control.compute = list(dic=TRUE),
																								 control.fixed = list(prec.intercept = 0.0001))
summary(random_uncorrelated_intercept_slope_INLA)
1/ sqrt(random_uncorrelated_intercept_slope_INLA$summary.hyperpar[,1])
