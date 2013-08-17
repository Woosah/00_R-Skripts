## computing  estimated scale  ( binomial model)
#following  D. Bates :
#That quantity is the square root of the penalized residual sum of
#squares divided by n, the number of observations, evaluated as:

dispersion_glmer <- function(modelglmer)
{
  n <- length(modelglmer@resid)
  
  return(  sqrt( sum(c(modelglmer@resid, modelglmer@u) ^2) / n ) )
}

#Zweite Variante von glmm.wikidot.com/faq
#

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(lme4::VarCorr(model),vpars))+length(lme4::fixef(model))
  (rdf <- nrow(model@frame)-model.df)
  rp <- lme4::residuals(model)
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

