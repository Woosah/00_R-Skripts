library(ggplot2)
library(mediation)
library(boot)

n <- 1000

i1 <- 10
i2 <- 8
a <- 4
b <- -5
c <- 2

sigma1 <- 1.5
sigma2 <- 3

x <- runif(n, min = 0, max = 1.5)

m <- i1 + a * x + rnorm(n, mean = 0, sd = sqrt(sigma1))
y <- i2 + b * m + c * x + rnorm(n, mean = 0, sd = sqrt(sigma2))
data.med <- data.frame(x = x, m = m, y = y)

mod.1 <- lm(m ~ x, data = data.med)
summary(mod.1)
qplot(x = m, y = y, geom = c("point", "smooth"), method = "lm", ymin = 0, xmin = 0,
      data = data.med)
mod.2 <- lm(y ~ m + x, data = data.med)
summary(mod.2)
qplot(x = x, y = y, geom = c("point", "smooth"), method = "lm", ymin = 0, xmin = 0,
      data = data.med)

med.1 <- mediate(mod.1, mod.2, sims = 1000, boot = TRUE, boot.ci.type = "bca",
                 mediator = "m", treat = "x")
summary(med.1)
plot(med.1)

med.boot <- function(d, idx) {
    newdat <- d[idx,]
    lm1 <- lm(m ~ x, data = newdat)
    coef1 <- coef(lm1)[-1]
    lm2 <- lm(y ~ m + x, data = newdat)
    coef2 <- coef(lm2)[-1]
    a <- coef1[1]
    b <- coef2[1]
    c <- coef2[2]
    ab <- a * b    
    c(a,b,c,ab)
}

res.boot <- boot(data.med, statistic = med.boot, R = 10000, parallel = "multicore", ncpus = 8)
str(res.boot)
CI_a <- boot.ci(res.boot, index = 1, type = "bca")
CI_a
plot(res.boot, index = 1)
CI_b <- boot.ci(res.boot, index = 2, type = "bca")
CI_b
plot(res.boot, index = 2)
CI_c <- boot.ci(res.boot, index = 3, type = "bca")
CI_c
plot(res.boot, index = 3)
CI_ab <- boot.ci(res.boot, index = 4, type = "bca")
CI_ab
plot(res.boot, index = 4)

