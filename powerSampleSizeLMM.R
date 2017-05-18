library(simr)

# Newdata that contains two treatments measured over five timepoints for n individuals
# (every individual only gets one of the two treatments):

n <- 120 # take only even numbers!
ID <- paste0("ID", rep(1:n))

# number of timepoints:
timeN <- 5
time <- paste0("T", rep(0:(timeN - 1)))

# two treatments:
group <- c('TAU', "Sham", 'New')

X <- data.frame(ID = rep(ID, each = timeN, length.out = n * timeN), 
                time = rep(time, times = n),
                group = rep(group, each = timeN, length.out = n * timeN))
X$group <- factor(X$group, levels = c("TAU", "Sham", "New"))
X$weeks <- as.numeric(X$time) - 1
X$weeks[X$weeks == 4] <- 24



# fixed and random effects for bigger model m1
b <- c(20, 0.05, -0.05, -2, -15, -10, -7, 0, 0, -0.5, -1, -2, -4, -2, -4) # fixed effects
V <- matrix(c(5, 2, 2, 0.25), 2) # random intercept and slope variance-covariance matrix
V2 <- matrix(c(5, 2), 2)
    
s <- 6 # residual variance

m1 <- makeLmer(y ~ group + time + time:group + (1 + weeks | ID), fixef = b, VarCorr = V, sigma = s, data = X)
summary(m1)

T1 <- doTest(m1, fcompare(. ~ group + time, method = "kr"))
T1


pow1 <- powerSim(m1, nsim = 10, fcompare(. ~ group + time, method = "kr"))
pow1

