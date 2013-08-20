############################################################
# QIC for GEE models
# Daniel J. Hocking
############################################################
QIC = function(model.R) {
  library(MASS)
  model.indep = update(model.R, corstr = "independence")
  # Quasilikelihood
  mu.R = model.R$fitted.values
  y = model.R$y
  type = family(model.R)$family
  quasi.R = switch(type,
                   poisson = sum((y*log(mu.R)) - mu.R),
                   gaussian = sum(((y - mu.R)^2)/-2),
                   binomial = sum(y*log(mu.R/(1 - mu.R)) + log(1 - mu.R)),
                   Gamma = sum(-y/(mu.R - log(mu.R))),
                   stop("Error: distribution not recognized"))
  # Trace Term (penalty for model complexity)
  omegaI = ginv(model.indep$geese$vbeta.naiv) # Omega-hat(I) via Moore-Penrose generalized inverse of a matrix in MASS package
  #AIinverse = solve(model.indep$geese$vbeta.naiv) # solve via indenity
  Vr = model.R$geese$vbeta
  trace.R = sum(diag(omegaI %*% Vr))
  px = length(mu.R) # number non-redunant columns in design matrix
  # QIC
  QIC = 2*(trace.R - quasi.R) [EDIT: original post was missing '*']
  #QICu = (-2)*quasi.R + 2*px    # Approximation assuming model structured correctly
  output = c(QIC, quasi.R, trace.R, px)
  names(output) = c('QIC', 'Quasi Lik', 'Trace', 'px')
  return(output)
}