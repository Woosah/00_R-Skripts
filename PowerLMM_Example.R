#SevillaR talk

#The problem:

time <- c(2000:2015)
abundance <- rnorm(16, 150, 50) #poison??
plot(abundance ~ time, t = "l")


#can I detect a trend?
m <- lm(abundance ~ time)
summary(m)
abline(m, col = "red")

#but, can I detect a trend? Power analysis.

library(pwr)

pwr.r.test(n = 16, r = cor(time, abundance), sig.level = 0.05, power = NULL) 

#how many more years would I need?
(sample_needed <- pwr.r.test(n = NULL, r = cor(time, abundance), sig.level = 0.05, power = 0.80))

sample_needed[1] < length(time)

#same for anovas (pwr.anova.test)

# simple, right? but data is complex

abundance2 <- jitter(abundance, 1000)
abundance3 <- jitter(abundance, 1000)
plot(abundance ~ time, t = "p")
points(abundance2 ~ time, col = "red")
points(abundance3 ~ time, col = "blue")

#build a data frame
data <- data.frame(time = rep(time, 3), abundance = c(abundance, abundance2, abundance3), 
                   site = c(rep("uno", 16), rep("dos", 16), rep("tres", 16)))
head(data)

library(lme4)

m <- lmer(abundance ~ time + (1|site), data)
summary(m)
abline(fixef(m))

#so, what is my power? how many years? #do simulations!

library(devtools)
install_github("pitakakariki/simr")
library(simr)

#Specify the effect size
fixef(m)
fixef(m)["time"]
fixef(m)["time"] <- -2 #a 10% change in 10 years, i.e. 20 individuos sobre 200 en 10 años
fixef(m)
abline(fixef(m), col = "red")

#Calculate power
powerSim(m) #slow pre-calculated
ps <- lastResult()

#Use extend to increase the sample size.
model2 <- extend(m, along = "time", n=30)
powerSim(model2)

#Power curve over range of sample sizes.
pc2 <- powerCurve(model2)
print(pc2)
plot(pc2)

#Increase the number or size of groups.
#Power curve with more groups.
model3 <- extend(m, along="site", n=5) #this is equivalent at saying more sites, equal number of years
pc3 <- powerCurve(model3, along="site") 
print(pc3)
plot(pc3)

#Power curve with larger groups.
model4 <- extend(m, within="time+site", n=5) #this is like taking more than one value per site (same years and sites)
pc4 <- powerCurve(model4, within="time+site", nsim = 99)
print(pc4)
plot(pc4)


#If pilot data is not available, simr can be used to create lme4 objects from scratch as a starting point.
#https://github.com/pitakakariki/simr/blob/master/vignettes/fromscratch.Rmd


