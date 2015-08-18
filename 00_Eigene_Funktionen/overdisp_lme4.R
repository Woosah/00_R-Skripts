overdisp_lme4 <- function(model, dig = 2) {
    fam <- tolower(unclass(family(model))$family)
    if (fam != "poisson" && fam != "binomial") stop("model-family must be either poisson or binomial!")
    ## number of variance parameters in 
    ## an n-by-n variance-covariance matrix
    if (inherits(model, "merMod")) {
        vpars <- function(m) {
            nrow(m) * (nrow(m) + 1) / 2
        }
        model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
    } else if (inherits(model, "glm")) {
        model.df <- length(coef(model))
    }
    rdf <- nrow(model.frame(model)) - model.df
    rp <- residuals(model, type = "pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq / rdf
    pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
    round(c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval), digits = dig)
}
