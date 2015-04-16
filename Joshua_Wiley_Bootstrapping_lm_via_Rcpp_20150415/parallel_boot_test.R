set.seed(1234)
dmat <- cbind(1, matrix(rnorm(1000 * 50), ncol = 50))
beta <- matrix(c(2, runif(50)))
yvec <- as.vector(dmat %*% beta + rnorm(1000, sd = 3))
dall <- cbind(y = yvec, as.data.frame(dmat[,-1]))
myindex <- matrix(sample(0:(1000 - 1), 1000 * 500, TRUE), ncol = 500)

library(Rcpp)
library(RcppEigen)
library(RcppParallel)

sourceCpp("parallel_boot_lm.cpp")

setThreadOptions(numThreads = 4)

system.time(res1 <- parallelFit(dmat, yvec, myindex))
system.time(res2 <- apply(myindex, 2, function(i) coef(lm(y ~ ., data = dall[i+1, ]))))

par(mfrow = c(2, 1))
hist(res1[1,], breaks = 30)
hist(res2[1,], breaks = 30)
