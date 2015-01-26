library(ggplot2)
library(lme4)
library(mvtnorm)

fixed.intercept <- 1
n.subject <- 10
n.replicate <- 10
mean.subject <- c(1, 2, 3, 3, 2, 1)
sd.subject <- 0.1 * diag(length(mean.subject))
sd.noise <- 0.5

rf <- rmvnorm(n.subject, mean = mean.subject, sigma = sd.subject)
rf <- cbind(seq_len(n.subject), rf)
rf.long <- rbind(
  rf[, c(1, 2:3)], 
  rf[, c(1, 4:5)], 
  rf[, c(1, 6:7)] 
)
rf.long <- as.data.frame(rf.long)
colnames(rf.long) <- c("ID", "RandomIntercept", "RandomSlope")
rf.long$Condition <- rep(letters[1:3], each = n.subject)
dataset <- expand.grid(
  ID = seq_len(n.subject),
  Condition = letters[1:3],
  X = runif(20),
  Replicate = seq_len(n.replicate)
)
dataset <- merge(
  dataset, 
  rf.long
)
dataset$Y <- fixed.intercept + with(dataset, RandomIntercept + RandomSlope * X) + rnorm(nrow(dataset), sd.noise)

ggplot(dataset, aes(x = X, y = Y, colour = Condition)) + geom_point() + facet_wrap(~ID)
ggplot(dataset, aes(x = X, y = Y, colour = Condition)) + geom_point()

m.lm <- lm(Y ~ Condition * X, data = dataset)
m.lmm <- lmer(Y ~ Condition * X + (Condition * X|ID), data = dataset)

coef(m.lm)
fixef(m.lmm)
coef(summary(m.lm))
coef(summary(m.lmm))
