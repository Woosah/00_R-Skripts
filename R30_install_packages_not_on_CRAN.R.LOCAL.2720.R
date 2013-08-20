remove.packages('rstan')
options(repos = c(getOption("repos"), rstan = "http://wiki.stan.googlecode.com/git/R"))
install.packages('rstan', type = 'source')

library(rstan)
set_cppo("fast")


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
fit3 <- stan(fit = fit2, data = schools_dat, iter = 1000000, chains = 4)
print(fit3)
plot(fit2)

save(fit3, file = "fit3.RData")



require(devtools)
install_github("slidify", "ramnathv")
install_github("slidifyLibraries", "ramnathv")

install.packages('devtools')
library(devtools)
install_github(repo='jagstools', username='johnbaums')


install.packages("D:/05 R-Literatur/13 GAMLSS/gamlss.boot_1.6-5.zip", repos = NULL)
install.packages("D:/05 R-Literatur/13 GAMLSS/gamlss.rsm_1.0.zip", repos = NULL)
install.packages("D:/05 R-Literatur/13 GAMLSS/gamlss.sparse_0.0-1.zip", repos = NULL)
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/concord_1.4-9.tar.gz", 
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/covRobust_1.0.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/mblm_0.11.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/mimR_2.6.2.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/MISA_2.11.1-1.0.1.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/mixer_1.5.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/moc_1.0.5.1.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/mprobit_0.9-3.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/pcurve_0.6-3.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/predbayescor_1.1-4.tar.gz",
                 repos = NULL, type = "source")
install.packages("D:/05 R-Literatur/10 R-Packages_Manuals/ThreeGroups_0.1.tar.gz",
                 repos = NULL, type = "source")
install_github('pisa', 'jbryer')
install_github('TriMatch', 'jbryer')
install_github('irutils', 'jbryer')
install_github('makeR', 'jbryer')
install_github('sqlutils', 'jbryer')
install_github('retention', 'jbryer')
install_github('likert', 'jbryer')

install.packages("BradleyTerry2")
install.packages("coefplot2", repos="http://www.math.mcmaster.ca/bolker/R",type="source")
install.packages("spikeslab", type = "source")

install_github(repo = "HandyStuff", username = "bryanhanson", ref = "master")


install.packages("latticist")
install.packages("rggobi")
install.packages("playwith", depend = TRUE)
install.packages("googleVis")











































