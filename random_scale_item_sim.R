library(MASS) # for mvrnorm()
library(lme4) # for lmer()

# function to simulate data, fit models, do model comparison
sim <- function(n, # number of subjects
                m, # number of items per test
                mu, # mean for both tests
                SD, # subject SD in test means
                r, # correlation between subject test means
                diff, # SD of item difficulties (intercepts)
                error){ # SD of errors
  # make data
  dat <- within(expand.grid(sub=seq(n), item=seq(m), test=c("A","B")), {
    item <- paste(item, test)
  })
  # subject-level covariance matrix for test means
  Sigma <- matrix(c(SD^2, r*SD^2, r*SD^2, SD^2), nrow=2)
  # add subject effects. subject means get named "X1" and "X2"
  dat <- merge(dat, data.frame(sub=seq(n), mvrnorm(n, mu=c(mu, mu), Sigma=Sigma)))
  # add items effects
  dat <- merge(dat, data.frame(item=unique(dat$item), difficulty=rnorm(2*m, sd=diff)))
  # add response
  dat <- within(dat, {
    y <- (test=="A")*X1 + (test=="B")*X2 + difficulty + rnorm(n*m*2, sd=error)
  })
  
  # fit large model w/ random items
  # note that this allows for unequal item difficulty variances
  # even though they are "truly" equal in the simulation
  modA1 <- lmer(y ~ 0 + test + (0+test|sub) + 
                  (0+as.numeric(test=="A")|item) + (0+as.numeric(test=="B")|item),
                data=dat, REML=FALSE)

  # fit small model w/ random items
  modC1 <- lmer(y ~ 0 + test +
                  (0+as.numeric(test=="A")|sub) + (0+as.numeric(test=="B")|sub) + 
                  (0+as.numeric(test=="A")|item) + (0+as.numeric(test=="B")|item),
                data=dat, REML=FALSE)
  
  # fit large model w/ fixed items
  modA2 <- lmer(y ~ 0 + test + (0+test|sub), data=dat, REML=FALSE)
  
  # fit small model w/ fixed items
  modC2 <- lmer(y ~ 0 + test +
                 (0+as.numeric(test=="A")|sub) + (0+as.numeric(test=="B")|sub),
                data=dat, REML=FALSE)
  
  # return correlation and p-value for both cases
  return(c(sqrt(diag(VarCorr(modA1)$sub)),
         itemA = sqrt(VarCorr(modA1)$item),
         itemB = sqrt(VarCorr(modA1)$item.1),
         error = attr(VarCorr(modA1), "sc"),
         r_random = attr(VarCorr(modA1)$sub, "correlation")[2,1],
         p_random = anova(modA1, modC1)$Pr[2],
         r_fixed = attr(VarCorr(modA2)$sub, "correlation")[2,1],
         p_fixed = anova(modA2, modC2)$Pr[2]))
} 

# test once
sim(n=30, m=10, mu=100, SD=15, r=.5, diff=10, error=5)

# run sim with true r = 0.5
# elapsed time for 1000 iterations on my MacBook Pro = 8.5 mins
system.time({
  results <- replicate(1000,
                       sim(n=30, m=10, mu=100, SD=15, r=.5, diff=10, error=5))
})

rowMeans(results)
#       testA       testB       itemA       itemB       error    r_random 
# 14.73880335 14.72330902  9.45127834  9.37610172  4.99220700  0.48466196 
#   p_random     r_fixed     p_fixed 
# 0.04270353  0.51275337  0.04131488 
mean(results["p_random",] < .05)
# [1] 0.817
mean(results["p_fixed",] < .05)
# [1] 0.827
hist(results["r_random",], col="gray")
hist(results["r_fixed",], col="gray")

# run sim with true r = 0
system.time({
  results2 <- replicate(1000,
                       sim(n=30, m=10, mu=100, SD=15, r=0, diff=10, error=5))
})

# examine results
rowMeans(results2)
#        testA        testB        itemA        itemB        error     r_random 
# 14.740990019 14.743167758  9.473208991  9.484094487  4.998384887  0.001477996 
#    p_random      r_fixed      p_fixed 
# 0.487966943  0.001642746  0.483784582 
mean(results2["p_random",] < .05)
# [1] 0.067
mean(results2["p_fixed",] < .05)
# [1] 0.071
hist(results2["r_random",], col="gray")
hist(results2["r_fixed",], col="gray")

# are the distributions of p-values uniform under the null?
hist(results2["p_random",], col="gray")
hist(results2["p_fixed",], col="gray")

# how well do the pairs of p-values agree?
plot(y=results["p_random",], x=results["p_fixed",])
plot(y=results2["p_random",], x=results2["p_fixed",])
