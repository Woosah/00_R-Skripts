#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Squirrels")
SQ <- read.table(file = "RedSquirrels.txt", 
                 header = TRUE, 
                 dec = ".")
                 
str(SQ)
names(SQ)
##############################################################




##############################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
source(file = "/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises/HighstatLibV6.R")  
source(file = "/Users/Highstat/applicat/HighlandStatistics/MCMC/R/MCMCSupportHighstat.R")
##############################################################




##############################################################
MyVar <- c("SqCones", "Ntrees", "DBH", 
           "TreeHeight", 
           "CanopyCover")

Mydotplot(SQ[,MyVar])
#Ouch....1 large DBH value!
#A few small Canopy values!
#A few large Ntrees values.
#  There may be a patch with large
#  DBH and NTrees value!

#Remove the value with extreme large DBH value
SQ2 <- subset(SQ, DBH < 0.6)
dim(SQ)
dim(SQ2)

#Collinearity
MyVar <- c("Ntrees", "DBH", 
           "TreeHeight", 
           "CanopyCover")

pairs(SQ2[, MyVar], 
      lower.panel = panel.cor)

corvif(SQ2[,MyVar])
#In principle that is ok!!


#Relationships
MyVar <- c("Ntrees", "DBH", 
           "TreeHeight", 
           "CanopyCover")
Myxyplot(SQ2, MyVar, "SqCones")
#Yes...that point is going to cause trouble!!!

#Zero inflation?
plot(table(SQ2$Squcones))
#No
##############################################################






##############################################################
#Section 2.4
SQ2$Ntrees.std      <- as.numeric(scale(SQ2$Ntrees))
SQ2$TreeHeight.std  <- as.numeric(scale(SQ2$TreeHeight))
SQ2$CanopyCover.std <- as.numeric(scale(SQ2$CanopyCover))

M1 <- glm(SqCones ~ Ntrees.std +  
                    TreeHeight.std + CanopyCover.std,
          family = "poisson",
          data = SQ2)

print(summary(M1), digits=3, signif.stars = FALSE)


#Figure 2.4
par(mfrow = c(1,3), mar = c(5,5,2,2))
plot(x=SQ2$Ntrees.std , y = SQ2$SqCones,
     xlab = "Standardized number of trees",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$Ntrees.std)
MyData <- data.frame(Ntrees.std = seq(-1.1, 4, length = 25),
                     TreeHeight.std  = 0,
                     CanopyCover.std = 0)
P1 <- predict(M1, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$Ntrees.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$Ntrees.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$Ntrees.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     

plot(x=SQ2$TreeHeight.std , y = SQ2$SqCones,
     xlab = "Standardized tree height",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$TreeHeight.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = seq(-3, 1.5, length = 25),
                     CanopyCover.std = 0)
P1 <- predict(M1, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     


plot(x=SQ2$CanopyCover.std , y = SQ2$SqCones,
     xlab = "Standardized canopy cover",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$CanopyCover.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = 0,
                     CanopyCover.std = seq(-4, 1, length = 25))
P1 <- predict(M1, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     


##################################################
#Assess overdispersion:
E1 <- resid(M1, type = "pearson")
sum(E1^2) / (nrow(SQ2) - length(coef(M1)))
#[1] 13.9802



#Figure 2.5
F1 <- fitted(M1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, y = E1, xlab = "Fitted values",
     ylab = "Pearson residuals", cex.lab = 1.5)
abline(h=0, lty = 2)
     
plot(cooks.distance(M1), type = "h",
     xlab = "Observation", ylab = "Cook distance",
     cex.lab =  1.5)

plot(y = SQ2$SqCones, x = F1,
     xlab = "Fitted values",
     ylab = "Observed data",
     cex.lab = 1.5,xlim = c(0,60), ylim = c(0,60) )
abline(coef=c(0, 1), lty = 2)   


#Figure 2.6
MyVar <- c("Ntrees.std", "DBH", 
           "TreeHeight.std", 
           "CanopyCover.std")

SQ2$E1 <- E1    
Myxyplot(SQ2, MyVar, "E1")

sum(SQ2$SqCone==0) / nrow(SQ2)  
     
     


########################################################
library(MASS)
M2 <- glm.nb(SqCones ~ Ntrees.std +  
             TreeHeight.std + CanopyCover.std,
             data = SQ2)

#Assess overdispersion:
E2 <- resid(M2, type = "pearson")
p <- length(coef(M2)) + 1
sum(E2^2) / (nrow(SQ2) - p)

summary(M2)
drop1(M2, test = "Chi")


M2A <- glm.nb(SqCones ~   
             TreeHeight.std + CanopyCover.std,
             data = SQ2)

anova(M2, M2A, test = "Chi")


######################################################
#Figure 2.7
par(mfrow = c(1,3), mar = c(5,5,2,2))
plot(x = SQ2$Ntrees.std, 
     y = SQ2$SqCones,
     xlab = "Standardized number of trees",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$Ntrees.std)
MyData <- data.frame(Ntrees.std = seq(-1.1, 4, length = 25),
                     TreeHeight.std  = 0,
                     CanopyCover.std = 0)
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$Ntrees.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$Ntrees.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$Ntrees.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     

plot(x=SQ2$TreeHeight.std , y = SQ2$SqCones,
     xlab = "Standardized tree height",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$TreeHeight.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = seq(-3, 1.5, length = 25),
                     CanopyCover.std = 0)
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     


plot(x=SQ2$CanopyCover.std , y = SQ2$SqCones,
     xlab = "Standardized canopy cover",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$CanopyCover.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = 0,
                     CanopyCover.std = seq(-4, 1, length = 25))
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
###############################################################





###############################################################
#Section 2.5.2
library(msme)    
nb2 <- nbinomial(SqCones ~ Ntrees.std +  
                 TreeHeight.std + CanopyCover.std,
                 data = SQ2)
summary(nb2)


nbH <- nbinomial(SqCones ~ Ntrees.std +  
                           TreeHeight.std + 
                           CanopyCover.std,
                 formula2 =~ Ntrees.std +  
                             TreeHeight.std + 
                             CanopyCover.std,
                 family = "negBinomial",
                 mean.link = "log",
                 scale.link = "log_s", 
                 data = SQ2)
summary(nbH)
################################################






#################################################
#Section 2.6
library(R2jags)

win.data <- list(SqCones     = SQ2$SqCones,
                 Ntrees      = SQ2$Ntrees.std,
                 TreeHeight  = SQ2$TreeHeight.std,
                 CanopyCover = SQ2$CanopyCover.std,
                 N           = nrow(SQ2)
           )

win.data


sink("GLMPoisson.txt")
cat("
model{
  #1. Priors for regression coefficients
  for (i in 1:4) { beta[i] ~ dnorm(0,0.0001)} 
  
  #2. Likelihood function
  for (i in 1:N){  
     SqCones[i]  ~  dpois(mu[i])
     log(mu[i]) <- max(-20, min(20, eta[i]))  
     eta[i]  <- beta[1] + 
                beta[2] * Ntrees[i] + 
                beta[3] * TreeHeight[i] +
                beta[4] * CanopyCover[i]                  
  }    
}
",fill = TRUE)
sink()


#Set the initial values for the betas and sigma
inits <- function () {
   list(
    beta       = rnorm(4, 0, 0.1)
       )  }

#Parameters to estimate
params <- c("beta")


K1 <- jags(data       = win.data, 
           inits      = inits, 
           parameters.to.save = params,
           model.file = "GLMPoisson.txt",
           n.chains   = 3,
           n.iter     = 20000,
           n.thin     = 10,
           n.burnin   = 10000)
           
print(K1,intervals=c(0.025, 0.975), digits = 3)
           
K2 <- update(K1, n.iter = 10000)           
print(K2,intervals=c(0.025, 0.975), digits = 3)


vars <- c("beta[1]", "beta[2]","beta[3]",
          "beta[4]") 
OUT1 <- MyBUGSOutput(K1$BUGSoutput, vars) 
print(OUT1, digits =3)


#########################################################
#Section 2.6.7
#Generalize the JAGS code
X <- model.matrix(~Ntrees.std + TreeHeight.std + CanopyCover.std,
                   data = SQ2)
K <- ncol(X)
                   
win.data <- list(SqCones = SQ2$SqCones,
                 X       = X,
                 K       = K,
                 N       = nrow(SQ2)
           )


sink("GLMPoisson.txt")
cat("
model{
  #1. Priors for regression coefficients
  for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) } 

  #2. Likelihood function
  for (i in 1:N){  
     SqCones[i]  ~  dpois(mu[i])
     log(mu[i]) <- max(-20, min(20, eta[i]))  
     eta[i]  <- inprod(beta[], X[i,]) 
  }    
}
",fill = TRUE)
sink()

#Set the initial values for the betas and sigma
inits <- function () {
   list(
    beta = rnorm(K, 0, 0.1)
       )  }

#Parameters to estimate
params <- c("beta")

K1 <- jags(data       = win.data, 
           inits      = inits, 
           parameters.to.save = params,
           model.file = "GLMPoisson.txt",
           n.chains   = 3,
           n.iter     = 20000,
           n.thin     = 10,
           n.burnin   = 10000)
K2 <- update(K1, n.iter = 10000)           
print(K1,intervals=c(0.025, 0.975), digits = 3)


####################################################
#Section 2.7 Assess mixing of chains
MyBUGSChains(K1$BUGSoutput, vars)
####################################################



####################################################
#Section 2.8 Model validation 
X <- model.matrix(~Ntrees.std + TreeHeight.std + CanopyCover.std,
                   data = SQ2)
K <- ncol(X)
                   
win.data <- list(SqCones = SQ2$SqCones,
                 X       = X,
                 K       = K,
                 N       = nrow(SQ2)
           )

sink("GLMPoisson.txt")
cat("
model{
  #1. Priors for regression coefficients
  for (i in 1:K) { beta[i] ~ dnorm(0,0.0001) } 
  
  #2. Likelihood function
  for (i in 1:N){  
     SqCones[i]  ~  dpois(mu[i])
     log(mu[i]) <- max(-20, min(20, eta[i]))  
     eta[i]     <- inprod(beta[], X[i,]) 
          
     #Discrepancy measures (used for checking overdispersion)
     YNew[i]   ~ dpois(mu[i])   #New data
     expY[i]    <- mu[i] 
     varY[i]    <- mu[i]
     PRes[i]    <- (SqCones[i]  - expY[i]) / sqrt(varY[i])
     PResNew[i] <- (YNew[i] - expY[i]) / sqrt(varY[i])
     D[i]       <- pow(PRes[i], 2)
     DNew[i]    <- pow(PResNew[i], 2)
  }    
     Fit         <- sum(D[1:N])
     FitNew      <- sum(DNew[1:N])
}
",fill = TRUE)
sink()

#Set the initial values for the betas and sigma
inits <- function () {
   list(
    beta = rnorm(K, 0, 0.1)
       )  }

#Parameters to estimate
params <- c("beta", "PRes", "Fit", "FitNew")

K1 <- jags(data       = win.data, 
           inits      = inits, 
           parameters.to.save = params,
           model.file = "GLMPoisson.txt",
           n.chains   = 3,
           n.iter     = 20000,
           n.thin     = 10,
           n.burnin   = 10000)

mean(K1$BUGSoutput$sims.list$FitNew >  
     K1$BUGSoutput$sims.list$Fit) 



##################################################
#Section 2.8.2
#Opion 1:
out <- K1$BUGSoutput
E <- out$mean$Res

#Option 2:
beta1 <- K1$BUGSoutput$sims.matrix[,"beta[1]"]
beta2 <- K1$BUGSoutput$sims.matrix[,"beta[2]"]
beta3 <- K1$BUGSoutput$sims.matrix[,"beta[3]"]
beta4 <- K1$BUGSoutput$sims.matrix[,"beta[4]"]

PRes <- matrix(nrow = 51, ncol = 3000)
for (k in 1:3000){
     eta <- beta1[k] + 
            beta2[k] * win.data$X[,2] + 
            beta3[k] * win.data$X[,3] +
            beta4[k] * win.data$X[,4]
     mu  <- exp(eta)      
     PRes[,k] <- (win.data$SqCones - mu) / sqrt(mu)
     }


PRes.pm <-rowSums(PRes)/3000
PRes.pm  

#Option 3
beta1 <- mean(K1$BUGSoutput$sims.matrix[,"beta[1]"])
beta2 <- mean(K1$BUGSoutput$sims.matrix[,"beta[2]"])
beta3 <- mean(K1$BUGSoutput$sims.matrix[,"beta[3]"])
beta4 <- mean(K1$BUGSoutput$sims.matrix[,"beta[4]"])

PRes <- vector(length = 51)
eta <- beta1 + 
       beta2 * win.data$X[,2] + 
       beta3 * win.data$X[,3] +
       beta4 * win.data$X[,4]
mu  <- exp(eta)      
PRes <- (win.data$SqCones - mu) / sqrt(mu)
############################################################



############################################################
#Section 2.9
X <- model.matrix(~Ntrees.std + TreeHeight.std + CanopyCover.std,
                   data = SQ2)
K <- ncol(X)

win.data <- list(SqCones = SQ2$SqCones,
                 X       = X,
                 K       = K,
                 N       = nrow(SQ2)
           )


sink("GLMNB.txt")
cat("
model{
  #1. Priors for regression coefficients
  for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) } 
  #Prior for size
  size ~ dunif(0.001, 5)

  #2. Likelihood function
  for (i in 1:N){  
     SqCones[i]  ~ dnegbin(p[i], size)
     p[i]       <- size / (size + mu[i])  
     log(mu[i]) <- max(-20, min(20, eta[i]))  #Safety
     eta[i]     <- inprod(beta[], X[i,])
                  
     #Discrepancy measures (used for checking overdispersion)
     YNew[i]   ~ dnegbin(p[i], size)   #New data
     expY[i]    <- mu[i] 
     varY[i]    <- mu[i] + pow(mu[i],2) / size
     PRes[i]    <- (SqCones[i]  - expY[i]) / sqrt(varY[i])
     PResNew[i] <- (YNew[i] - expY[i]) / sqrt(varY[i])
     D[i]       <- pow(PRes[i], 2)
     DNew[i]    <- pow(PResNew[i], 2)
  }    
  
     Fit         <- sum(D[1:N])
     FitNew      <- sum(DNew[1:N])
}
",fill = TRUE)
sink()


#Set the initial values for the betas and sigma
inits <- function () {
   list(
    beta = rnorm(K, 0, 0.1),
    size = runif(0.001, 5)
       )  }

#Parameters to estimate
params <- c("beta", "Fit", "FitNew", "size", "PRes")

K1 <- jags(data = win.data, 
           inits = inits, 
           parameters.to.save = params,
           model.file = "GLMNB.txt",
           n.chains = 3,
           n.iter = 20000,
           n.thin = 10,
           n.burnin = 10000)
K2 <- update(K1, n.iter = 10000)           



vars <- c("beta[1]", "beta[2]","beta[3]",
          "beta[4]", "size") 

OUT1 <- MyBUGSOutput(K1$BUGSoutput, vars) 
print(OUT1, digits =3)
summary(M2)


###################################################
#2.10 Mixing of chains
MyBUGSChains(K1$BUGSoutput, vars)
MyBUGSACF(K1$BUGSoutput, vars)



###################################################
#2.11 Model validation
vars <- c("beta[1]", "beta[2]","beta[3]",
          "beta[4]", "size") 
OUT1 <- MyBUGSOutput(K1$BUGSoutput, vars) 
beta <- OUT1[1:4,1]
k <- OUT1[5,1]
eta <- win.data$X %*% beta
mu <- exp(eta)
EP <- (win.data$SqCones - mu) / sqrt(mu + mu^2 / k)  

par(mar = c(5,5,3,3))
plot(x = mu, 
     y = EP, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

SQ2$EP <- EP
Myxyplot(SQ2, MyVar,"EP")


mean(K1$BUGSoutput$sims.list$FitNew >  
     K1$BUGSoutput$sims.list$Fit) 


###########################################
#Section 2.12 Model interpretation
MyBUGSHist(K1$BUGSoutput, vars)


#Sketch fitted values

#First approach
sink("GLMNB.txt")
cat("
model{
  #1. Priors for regression coefficients
  for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) } 
  #Prior for size
  size ~ dunif(0.001, 5)

  #2. Likelihood function
  for (i in 1:N){  
     SqCones[i]  ~  dnegbin(p[i], size)
     p[i] <- size / (size + mu[i])  
     log(mu[i]) <- max(-20, min(20, eta[i]))  #Safety
     eta[i]  <- inprod(beta[], X[i,])
                  
     #Discrepancy measures (used for checking overdispersion)
     YNew[i]   ~ dnegbin(p[i], size)   #New data
     expY[i]    <- mu[i] 
     varY[i]    <- mu[i] + pow(mu[i],2) / size
     PRes[i]    <- (SqCones[i]  - expY[i]) / sqrt(varY[i])
     PResNew[i] <- (YNew[i] - expY[i]) / sqrt(varY[i])
     D[i]       <- pow(PRes[i], 2)
     DNew[i]    <- pow(PResNew[i], 2)
     
     MUf1[i] <- exp(beta[1] + beta[2] * X[i,2])
     MUf2[i] <- exp(beta[1] + beta[3] * X[i,3])
     MUf3[i] <- exp(beta[1] + beta[4] * X[i,4])
     pf1[i] <- size / (size + MUf1[i])  
     Yf1[i]   ~ dnegbin(pf1[i], size) 
  }    
     Fit         <- sum(D[1:N])
     FitNew      <- sum(DNew[1:N])
}
",fill = TRUE)
sink()


#Set the initial values for the betas and sigma
inits <- function () {
   list(
    beta = rnorm(K, 0, 0.1),
    size = runif(0.001, 5)
       )  }

#Parameters to estimate
params <- c("beta", "Fit", "FitNew", "size", "PRes",
            "MUf1", "MUf2", "MUf3")


K1 <- jags(data = win.data, 
           inits = inits, 
           parameters.to.save = params,
           model.file = "GLMNB.txt",
           n.chains = 3,
           n.iter = 20000,
           n.thin = 10,
           n.burnin = 10000)
K2 <- update(K1, n.iter = 10000)           



out  <- K2$BUGSoutput
OUT1 <- MyBUGSOutput(out, uNames("MUf1",51))
print(OUT1, digits = 3)


#Figure 2.16
par(mfrow = c(1,3), mar = c(5,5,2,2))
OUT1 <- MyBUGSOutput(out, uNames("MUf1",51))
CV <- sort(win.data$X[,"Ntrees.std"])
I1 <- order(win.data$X[,"Ntrees.std"])
par(mar = c(5,5,2,2))
plot(x = CV, OUT1[I1,1], type = "l",
     ylim = c(0,30),
     xlab = "Standardized number of trees",
     ylab = "Predicted values",
     cex.lab = 2)
lines(x=CV, OUT1[I1,3], type = "l", lty = 2)
lines(x=CV, OUT1[I1,4], type = "l", lty = 2)


OUT1 <- MyBUGSOutput(out, uNames("MUf2",51))
CV <- sort(win.data$X[,"TreeHeight.std"])
I1 <- order(win.data$X[,"TreeHeight.std"])
par(mar = c(5,5,2,2))
plot(x=CV, OUT1[I1,1], type = "l",
     ylim = c(0,30),
     xlab = "Standardized tree height",
     ylab = "Predicted values",
     cex.lab = 2 )
lines(x=CV, OUT1[I1,3], type = "l", lty = 2)
lines(x=CV, OUT1[I1,4], type = "l", lty = 2)


OUT1 <- MyBUGSOutput(out, uNames("MUf3",51))
CV <- sort(win.data$X[,"CanopyCover.std"])
I1 <- order(win.data$X[,"CanopyCover.std"])
par(mar = c(5,5,2,2))
plot(x=CV, OUT1[I1,1], type = "l",
     ylim = c(0,30),
     xlab = "Standardized canopy cover",
     ylab = "Predicted values",
     cex.lab =2 )
lines(x=CV, OUT1[I1,3], type = "l", lty = 2)
lines(x=CV, OUT1[I1,4], type = "l", lty = 2)


  




#####################################################################
#Second approach for a NB GLM: mixture of Gamma and Poisson distribution
sink("GLMNB2.txt")
cat("
model{

  #Priors for regression coefficients
  for (i in 1:K) {
  	beta[i] ~ dnorm(0,0.0001)
   } 
  #Prior for size
  size ~ dunif(0.001, 5)

  #Likelihood function
  for (i in 1:N){  
  	 SqCones[i] ~  dpois(g[i])
     g[i] ~ dgamma(size, rateParm[i])
     rateParm[i] <- size / mu[i] 
     log(mu[i]) <- max(-20, min(20, eta[i]))  #Safety
     eta[i]  <- inprod(beta[],X[i,])
                  
     #Discrepancy measures (used for checking overdispersion)
     YNew[i]   ~ dpois(g[i])   #New data
     expY[i]    <- mu[i] 
     varY[i]    <- mu[i] + pow(mu[i],2) / size
     PRes[i]    <- (SqCones[i]  - expY[i]) / sqrt(varY[i])
     PResNew[i] <- (YNew[i] - expY[i]) / sqrt(varY[i])
     D[i]       <- pow(PRes[i], 2)
     DNew[i]    <- pow(PResNew[i], 2)
  }    
  
     Fit         <- sum(D[1:N])
     FitNew      <- sum(DNew[1:N])
}
",fill = TRUE)
sink()


#Set the initial values for the betas and sigma
inits <- function () {
   list(
    beta       = rnorm(K, 0, 0.1),
    size         = runif(0.001,5)
       )  }

#Parameters to estimate
params <- c("beta", "Fit", "FitNew", "size")



K1 <- jags(data = win.data, 
           inits = inits, 
           parameters.to.save = params,
           model.file = "GLMNB2.txt",
           n.chains = 3,
           n.iter = 20000,
           n.thin = 10,
           n.burnin = 10000)
K2 <- update(K1, n.iter = 10000)           



