#Estimation of CIs for lme4 - GLM (Fixed Effects) - 
#Funktion für Confidence-Intervals für die fixed effects des Modells:



coef_lmer <- function(model) {
  lower <- coef(summary(model))[,1] + qnorm(.025)*coef(summary(model))[,2]
  upper <- coef(summary(model))[,1] + qnorm(.975)*coef(summary(model))[,2]
  cbind(coef(summary(model)), lower, upper)        
}