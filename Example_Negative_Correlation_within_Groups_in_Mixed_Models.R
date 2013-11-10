# Dominik Grathwohl's example on negative correlation
# leading to a collapse of prediction variance

library(nlme)
library(mvtnorm)
library(lattice)

# positive correlation
set.seed(171109)

N <- 20

(sigma <- matrix(c(4,2,2,3), ncol=2))

x <- rmvnorm(n = N, mean = c(1,2), sigma = sigma)

dim(x)
colMeans(x)
sd(x)
cov(x)

sqrt(cov(x)[2,1]) # between subject sd

(sd.w <- sd(x[,1] - x[,2]) / sqrt(2)) # within subject sd 

# check with mixed model 
df <- data.frame(id = rep(1:N,2), trt = gl(2,N), y = c(x[,1], x[,2])) 

sp <- list(superpose.symbol = list(pch = rep(16,7))) 

xyplot(y ~ trt, groups=factor(id), data = subset(df, id < 101), 
                type = c("g", "p", "l"),
                xlab = "trt", ylab = "y", par.settings = sp)

summary(m1 <- lme(y ~ trt, data = df, random = ~ 1 | id)) 
plot(m1 <- lme(y ~ trt, data = df, random = ~ 1 | id), pch = 16)

plot(m1,  resid(.,type = "p") ~ fitted(.) | trt, pch=16)

# negative correlation
set.seed(171109)

N <- 20

(sigma <- matrix(c(4,-2,-2,3), ncol = 2))

x <- rmvnorm(n = N, mean = c(1,2), sigma = sigma)

dim(x)
colMeans(x)
sd(x)
cov(x)

sqrt(cov(x)[2,1]) # between subject sd

(sd.w <- sd(x[,1] - x[,2]) / sqrt(2)) # within subject sd 

# check with mixed model 

df <- data.frame(id = rep(1:N,2), trt = gl(2,N), y = c(x[,1], x[,2]))

sp <- list(superpose.symbol = list(pch = rep(16,7))) 
xyplot(y ~ trt, groups = factor(id), data = subset(df, id < 101), 
                type=c("g", "p", "l"),
                xlab = "trt", ylab = "y", par.settings = sp)

summary(m1 <- lme(y ~ trt, data = df, random = ~ 1 | id)) 
plot(m1 <- lme(y ~ trt, data = df, random = ~ 1 | id), pch = 16)

plot(m1,  resid(.,type="p") ~ fitted(.) | trt, pch = 16)
