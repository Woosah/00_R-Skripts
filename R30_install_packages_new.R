schools_code <- '
  data {
    int<lower=0> J; // number of schools 
    real y[J]; // estimated treatment effects
    real<lower=0> sigma[J]; // s.e. of effect estimates 
  }
  parameters {
    real mu; 
    real<lower=0> tau;
    real eta[J];
  }
  transformed parameters {
    real theta[J];
    for (j in 1:J)
      theta[j] <- mu + tau * eta[j];
  }
  model {
    eta ~ normal(0, 1);
    y ~ normal(theta, sigma);
  }
'

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(model_code = schools_code, data = schools_dat, 
            iter = 1000, chains = 4)

print(fit)
plot(fit)


vector.of.installed.packages2 <- rownames(installed.packages())

vector.of.packages.to.install2 <- setdiff(vector.of.installed.packages,
                                         vector.of.installed.packages2)

## Install these packages
install.packages(pkgs = vector.of.packages.to.install2,
                 repos = "http://R-Forge.R-project.org", type = "source")


save(vector.of.packages.to.install2, file = "vector.of.packages.to.install2.RData")

c("AGSDest", "bayespack", "BPHO", "BradleyTerry", "coefplot2", 
  "concord", "covRobust", "gamlss.boot", "gamlss.rsm", "gamlss.sparse", 
  "HandyStuff", "jagstools", "mblm", "mimR", "MISA", "mixer", "moc", 
  "mprobit", "panel", "pcurve", "pisa", "predbayescor", "ReadImages", 
  "spikeslab", "ThreeGroups", "ZeligNetwork")

install.packages("astsa")
install.packages("lsmeans")
install.packages("lmerTest")



