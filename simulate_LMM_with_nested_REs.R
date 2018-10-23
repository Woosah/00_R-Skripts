# (c) by Thierry Onkelinx
#
# Posted on R-sig-ME 21.10.2018

library(lme4)
library(dplyr)

create_fake <- function(
  n_site = 100, n_subject_site = 10, n_time = 10,
  intercept = 10, trend = 0.1, effect = 0.5,
  sigma_site = 5, sigma_subject = 2, sigma_noise = 1
){
  re.site <- rnorm(n_site, mean = 0, sd = sigma_site)
  re.subject <- rnorm(n_site * n_subject_site, mean = 0, sd = sigma_subject)
  
  expand.grid(
    time = seq(0, 2, length = n_time),
    site = seq_len(n_site),
    subject = seq_len(n_subject_site)
  ) %>%
    mutate(
      subject = interaction(site, subject),
      treatment = sample(0:1, size = n_site * n_subject_site,
                         replace = TRUE)[subject],
      fixed = intercept + effect * treatment + trend * time,
      random = re.site[site] + re.subject[subject],
      mu = fixed + random,
      y = rnorm(n(), mean = mu, sd = sigma_noise)
    )
}
dataset <- create_fake()
m <- lmer(y ~ treatment + time + (1|site/subject), data = dataset)
summary(m)
