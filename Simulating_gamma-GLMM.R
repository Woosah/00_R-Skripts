################################################################################
#
# 30.03.2015
#
# Mail from Ben Bolker to R-Sig-ME regarding a question on how to extract the
# scale-parameter from a gamma GLMM in lme4. Here is the answer including code:
#
# This is in fact not documented anywhere that I can see, but 1/sigma(fit)^2
# appears to do the trick for the shape parameter.  For the purposes of
# GL(M)Ms, the Gamma is parameterized as (mean,shape), so the values you're
# estimating will be the mean -- if you want the scale parameters, use
# mean = shape*scale -> scale = mean/shape ...
#
################################################################################

library("lme4")
set.seed(101)
d <- expand.grid(block = LETTERS[1:26], rep = 1:100, KEEP.OUT.ATTRS = FALSE)
d$x <- runif(nrow(d))  ## sd=1
reff_f <- rnorm(length(levels(d$block)), sd=1)

## need intercept large enough to avoid negative values

d$eta0 <- 4 + 3 * d$x  ## fixed effects only
d$eta <- d$eta0 + reff_f[d$block]
shapevec <- c(1, 2, 5)
res <- expand.grid(rep = 1:10, shape = shapevec, est = NA) ## order matters

k <- 1
for (i in seq_along(shapevec)) {
    cat(".")
    for (j in 1:10) {
        dgl <- d
        dgl$mu <- exp(d$eta)
        dgl$y <- rgamma(nrow(d), scale = dgl$mu / 2, shape = shapevec[i])
        ggl1 <- glmer(y ~ x + (1|block), data = dgl, family = Gamma(link="log"))
        res[k,"est"] <- 1 / sigma(ggl1)^2
        k <- k+1
    }
}
library(ggplot2)
theme_set(theme_bw())
ggplot(res, aes(shape, est)) + geom_point()