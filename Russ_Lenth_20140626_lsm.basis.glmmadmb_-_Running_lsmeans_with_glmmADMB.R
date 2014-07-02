# I saw a posting a week or two ago about obtaining lsmeans in glmmadmb objects. 
# I have something that seems to work, based on slight adaptations of methods for
# lm and lme objects. We need to create two functions:

recover.data.glmmadmb = lsmeans:::recover.data.lm

lsm.basis.glmmadmb = function (object, trms, xlev, grid)
{
    contrasts = object$contrasts
    m = model.frame(trms, grid, na.action = na.pass, xlev = xlev)
    X = model.matrix(trms, m, contrasts.arg = contrasts)
    bhat = fixef(object)
    V = vcov(object)
    misc = list()
    if (!is.null(object$family)) {
        fam = object$family
        misc$tran = object$link
        misc$inv.lbl = "response"
        if (!is.na(pmatch(fam,"binomial")))
            misc$inv.lbl = "prob"
        else if (!is.na(pmatch(fam,"poisson")))
            misc$inv.lbl = "rate"
    }
    nbasis = matrix(NA)
    dffun = function(...) NA
    list(X = X, bhat = bhat, nbasis = nbasis, V = V, dffun = dffun,
        dfargs = list(), misc = misc)
}

# Here is an example:

# R> library(glmmADMB)
# R> library(lsmeans)

# R> example(glmmadmb)  # creates an object named 'bfit'

# R> ( bfit.lsm = lsmeans(bfit, "trt") )
# trt       lsmean        SE df  asymp.LCL asymp.UCL
# placebo 1.949557 0.6686323 NA  0.6389030  3.260211
# drug    0.582841 0.8866400 NA -1.1551519  2.320834
# drug+   1.166833 0.8801796 NA -0.5584956  2.892163

# Confidence level used: 0.95

# R> pairs(bfit.lsm)
# contrast          estimate        SE df    z.ratio p.value
# placebo - drug   1.3667158 0.6771400 NA  2.0183652  0.1078
# placebo - drug+  0.7827233 0.6832600 NA  1.1455717  0.4860
# drug - drug+    -0.5839925 0.9108192 NA -0.6411728  0.7974

# P value adjustment: tukey method for a family of 3 means
# P values are asymptotic

# R> summary(.Last.value, type = "response")
# contrast        odds.ratio        SE df    z.ratio p.value
# placebo - drug   3.9224475 2.6560461 NA  2.0183652  0.1078
# placebo - drug+  2.1874213 1.4945775 NA  1.1455717  0.4860
# drug - drug+     0.5576674 0.5079342 NA -0.6411728  0.7974

# P value adjustment: tukey method for a family of 3 means
# P values are asymptotic
# Tests are performed on the linear-predictor scale

# I'm not sure if this covers all the possibilities. There may well be fine points that make this fail when some modeling options are used. I'd encourage the glmmADMB # developers to check these over and consider incorporating these methods in their package (and exporting them). The lsmeans package has a man page (?"extending-lsmeans") # as well as a vignette to help in figuring this stuff out.

# Hope this helps

# Russ

# Russell V. Lenth  -  Professor Emeritus
# Department of Statistics and Actuarial Science
# The University of Iowa  -  Iowa City, IA 52242  USA
# Voice (319)335-0712 (Dept. office)  -  FAX (319)335-3017