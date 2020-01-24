## Script adapted from https://stackoverflow.com/a/16511206
# NOTE: use geom_errorbarh (note the *h*) here because ggplot2 can't use coord_flip() 
# with facet_wrap(..., scales = "free") 
# NOTE: fixed to work for models w/o covariance component

## re = object of class ranef.mer
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE, ci_level=0.95) {
  require(ggplot2)
  f <- function(re_name) {
    x <- re[[re_name]]
    
    pv <- attr(x, "postVar")
    tmpf <- function(pv_i) {
      cols <- 1:(dim(pv_i)[1])
      unlist(lapply(cols, function(i) sqrt(pv_i[i, i, ])))
    }
    if (!is.list(pv)) {
      se <- tmpf(pv)
    } else {  # need this for models w/o covariance component
      se <- unlist(lapply(pv, tmpf))
    }
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(value=unlist(x)[ord],
                       ci=qnorm(1 - (1 - ci_level)/2)*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(value, nQQ)) + 
        facet_wrap(~ ind, scales="free_x") + 
        xlab("Random effects") + 
        ylab("Standard normal quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(value, ID))
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_wrap(~ ind, scales="free_x")
      }
      p <- p + 
        xlab("Random effects") + 
        ylab("Levels")
    }
    
    p <- p + 
      geom_errorbarh(aes(xmin = value - ci, xmax = value + ci), height = 0) +
      geom_vline(xintercept = 0, lty = 5) +
      geom_point() + 
      ggtitle(re_name) +
      theme(plot.title = element_text(hjust = 0.5))
    
    return(p)
  }
  
  setNames(lapply(names(re), f), names(re))
}

# demonstrate functionality and compare with lattice ---------------------------

# require(lme4)                            ## for lmer(), sleepstudy
# require(lattice)                         ## for dotplot()
# 
# fit <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
# 
# ggCaterpillar(ranef(fit, condVar=TRUE))  ## using ggplot2
# qqmath(ranef(fit, condVar=TRUE))         ## for comparison
# 
# ggCaterpillar(ranef(fit, condVar=TRUE), QQ=FALSE)
# dotplot(ranef(fit, condVar=TRUE))
# 
# # Sometimes, it might be useful to have different scales for the random effects - 
# # something which dotplot() enforces
# ggCaterpillar(ranef(fit, condVar=TRUE), QQ=FALSE, likeDotplot=FALSE)
# 
# # Works for models w/o covariance component
# fit_uncor <- lmer(Reaction ~ Days + (Days||Subject), sleepstudy)
# ggCaterpillar(ranef(fit_uncor, condVar=TRUE), QQ=FALSE)
