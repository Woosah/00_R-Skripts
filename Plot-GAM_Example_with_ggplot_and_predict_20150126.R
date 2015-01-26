# Generate data, example from help files of mgcv 1.8-4, function "s":
library(mgcv)

set.seed(0)
n <- 200
sig2 <- 4
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
fac <- c(rep(1, n/2), rep(2, n/2)) # create factor
fac.1 <- rep(0, n) + (fac == 1)
fac.2 <- 1 - fac.1 # and dummy variables
fac <- as.factor(fac)
f1 <- exp(2 * x1) - 3.75887
f2 <- 0.2 * x1^11 * (10 * (1 - x1))^6 + 10 * (10 * x1)^3 * (1 - x1)^10
f <- f1 * fac.1 + f2 * fac.2 + x2
e <- rnorm(n, 0, sqrt(abs(sig2)))
y <- f + e

# Putting everything in a dataframe:

data <- data.frame(cbind(fac, x1, x2, y))
colnames(data) <- c("fac", "x1", "x2", "y")
data$fac <- factor(data$fac)


# Housekeeping:
rm(e, f, f1, f2, fac, fac.1, fac.2, n, sig2, x1, x2, x3, y)

# Model fitting:
b <- gam(y ~ fac + s(x1, by = fac) + x2, data = data) 
summary(b)
plot(b, pages = 1, shade = TRUE, ylim = c(-7,7))

# Plotting with predict and ggplot2:
library(ggplot2)

pred <- expand.grid(fac = factor(c(1,2)),
                    x2 = mean(data$x2),
                    x1 = seq(from = min(data$x1), 
                             to = max(data$x1),
                             length = 100))

# using the predict.gam-function with type = "terms" is essential:
predNew <- predict(b, newdata = pred, type = "terms", se.fit = TRUE)

# Filling in the right fit and se.fit-values at the correct factor levels,
# making use of the same names according to summary(b) for the smoother-levels
# and to the column-names of predNew (you'll need one assignment per factor-level):
pred$fit[pred$fac == 1] <- predNew$fit[pred$fac == 1, rownames(summary(b)$s.table)[1]]
pred$fit[pred$fac == 2] <- predNew$fit[pred$fac == 2, rownames(summary(b)$s.table)[2]]
pred$se.fit[pred$fac == 1] <- predNew$se.fit[pred$fac == 1, rownames(summary(b)$s.table)[1]]
pred$se.fit[pred$fac == 2] <- predNew$se.fit[pred$fac == 2, rownames(summary(b)$s.table)[2]]

# CIs:
pred$CI_Low <- pred$fit - qnorm(0.975) * pred$se.fit
pred$CI_High <- pred$fit + qnorm(0.975) * pred$se.fit

# Plotting with a coloured rug to highlight the difference:
ggplot(pred, aes(y = fit, x = x1)) + geom_line() + 
    facet_grid(. ~ fac) + theme_bw() + 
    scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.4)) +
    scale_y_continuous(limits = c(-7,7), breaks = seq(-6,6, by = 2)) +
    geom_ribbon(aes(x = x1, ymin = CI_Low, ymax = CI_High), alpha = 0.3) +
    geom_rug(data = data, aes(y = y, x = x1, colour = fac),
             size = 0.6, alpha = 0.5, sides = "b") + 
    theme(legend.position = "none")






