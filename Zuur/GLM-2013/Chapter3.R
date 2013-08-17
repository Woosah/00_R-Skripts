#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Ricardo")
PO <- read.table(file = "PolychaetaV3.txt",
                 header = TRUE)
str(PO)

#'data.frame':	144 obs. of  4 variables:
# $ Hsimilis: int  0 0 0 0 0 0 0 0 0 0 ...
# $ Level   : Factor w/ 2 levels "Lower","Upper": 1 1 1 1 1 1 2 1 1 1 ...
# $ Location: Factor w/ 3 levels "A","B","C": 1 1 1 1 1 1 1 1 1 1 ...
# $ MedSand : num  16.24 10.73 5.84 10.03 25.95 ...


###################################################################
#Load packages and library files
library(lattice)  #Needed for data exploration
library(mgcv)  # Needed for data exploration
library(coefplot2)# Needed for glm/JAGS comp
library(R2jags)  # MCMC
source(file = "/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises/HighstatLibV6.R")  
source(file = "/Users/Highstat/applicat/HighlandStatistics/MCMC/R/MCMCSupportHighstat.R")
##################################################################



##################################################################
#Data exploration
#Visualizing number of zeros and ones
table(PO$Hsimilis)

#Dotplot of the continuos covariates grouped by location
dotchart(PO$MedSand, xlab ="Medium sand values",
         ylab = "Order of the data",
         cex.lab = 1.5,
         groups = factor(PO$Location))

#Relationships
#Presence-absence vs all covariates
xyplot(Hsimilis ~ MedSand | Level * Location,
       data = PO, pch = 16, col =1,
       strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T,
                     x = list(relation = "free"),
                     y = list(relation = "same")),
       xlab = list(label = "Medium sand content (%)", cex = 1.5) ,
       ylab = list(label = "Presence/absence of H. similis", cex = 1.5)
  )


xyplot(Hsimilis ~ MedSand | Location,
       data = PO, pch = 16, col =1,
       layout = c(1, 3),
       strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T,
                     x = list(relation = "free"),
                     y = list(relation = "same")),
       xlab = list(label = "Medium sand content (%)", cex = 1.5) ,
       ylab = list(label = "Presence/absence of H. similis", cex = 1.5)
  )


#Visualizing main terms  
plot.design(Hsimilis ~ Level + Location, data = PO)

#Number of obervations per level and location
table(PO$Location, PO$Level)
####################################################





####################################################
#Section 3.4
#Standarizing continuouis covariate
PO$MedSandC <- (PO$MedSand - mean(PO$MedSand))/sd(PO$MedSand)

#Running the GLM function
M1 <- glm(Hsimilis ~ MedSandC + Level + Location +
                       MedSandC : Level +
                       MedSandC : Location +
                       Level : Location , data = PO, family = binomial)
                       
summary(M1)
drop1(M1, test = "Chi")

#Model selection using step function
M1A <- glm(Hsimilis ~ 1, data = PO, family = binomial)


step(M1A, 
    scope = list(lower = ~1, 
                 upper =~ MedSandC + Level + Location +
                       MedSandC : Level +
                       MedSandC : Location +
                       Level : Location ))


#Results from the optimal model (using Level and Location)
M2 <- glm(Hsimilis ~ Level + Location,
                       data = PO, family = binomial)
summary(M2)
drop1(M2, test = "Chi")

######################################################
#Model validation

E2 <- resid(M2, type = "pearson")
F2 <- fitted(M2, type = "response")
plot(x = F2, y = E2, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)     

plot(cooks.distance(M2), 
     type = "h", 
     ylim = c(0,1),
     cex.lab = 1.5,
     ylab = "Cook distance values")

plot(x=PO$MedSand, y = E2,
     xlab = "Median sand content", 
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)     


######################################################
#Section 3.4.8 Visualizing the model
MyData <- expand.grid(Level = c("Lower", "Upper"),
                      Location  = c("A", "B", "C"))
X <- model.matrix(~Level + Location, data = MyData)

eta <- X %*% coef(M2)
MyData$pi <- exp(eta) / (1 + exp(eta))

SE <- sqrt(diag(X %*% vcov(M2) %*% t(X)))
MyData$SEup <- exp(eta + 2 * SE) / (1 + exp(eta + 2 * SE))
MyData$SElo <- exp(eta - 2 * SE) / (1 + exp(eta - 2 * SE))

MyX <- 1:6
MyXLab <- paste(MyData[,1], MyData[,2], sep = " ")
pr(mar = c(5,5,2,2))
plot(x = MyX, cex.lab = 1.5,
     y = MyData$pi,
     xlab = "Covariate combinations",
     ylab = "Predicted probabilities",
     axes = FALSE,
     type = "p",
     pch = 16,
     ylim = c(0, 1))
axis(2)
axis(1, at = MyX, labels = MyXLab )
box()
#F. Plot predicted values +/- 2* SE 
segments(MyX, MyData$SElo,
         MyX, MyData$SEup)          
######################################################



######################################################
#Section 3.5

#Specifying the data for JAGs
X <- model.matrix(~ MedSandC + Level + Location +
                       MedSandC : Level +
                       MedSandC : Location +
                       Level : Location, 
                    data = PO)
K <- ncol(X)

win.data <- list(Y    = PO$Hsimilis,
                 N    = nrow(PO),
                 X    = X,
                 K    = K,
                 LogN = log(nrow(PO)),
                 b0   = rep(0, K),
                 B0   = diag(0.00001, K)
                 )


#Jags modelling code
sink("GLM.txt")
cat("
model{
    #1. Priors
    beta  ~ dmnorm(b0[], B0[,])  
 
    #2. Likelihood 
    for (i in 1:N){  
      Y[i] ~ dbern(p[i])
      logit(p[i]) <- max(-20, min(20, eta[i]))  
      eta[i]      <- inprod(beta[], X[i,])
      LLi[i] <- Y[i] * log(p[i]) +
                (1 - Y[i]) * log(1 - p[i])
  } 
  LogL <- sum(LLi[1:N])
  AIC <- -2 * LogL + 2 * K
  BIC <- -2 * LogL + LogN * K

}
",fill = TRUE)
sink()


#Set the initial values for the betas and sigma
inits <- function () {
  list(
    beta  = rnorm(K, 0, 0.1)
    )  }

#Parameters to estimate
params <- c("beta", "LogL", "AIC", "BIC")


######################################################
#Execute the JAGs code
J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLM.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

out <- J0$BUGSoutput

OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "LogL", "AIC", "BIC"))
print(OUT1, digits = 3)

#Compare output from JAGS to that of GLM in R
cbind(coef(M1),OUT1[1:K,1])
########################################################






########################################################
#Section 3.6 Model selection using AIC, DIC and BIC in JAGS

out
min(out$sims.list$AIC)


# Next step is to calculate the AIC, BIC or DIC for each potential model
# Or use it in a forward or backwards selection procedure
# therefore we need to adjust the matrix X and re run JAGs code each time
# We will present the results of the model with Location and Level.

X <- model.matrix(~ Level + Location, 
                    data = PO)
K <- ncol(X)
#Now rerun the win.data and jags code


print(OUT1, digits = 3)

#Compared results with M2
cbind(coef(M2),OUT1[1:K,1])

# Make a graph to compare these parameters using
# the frequentist approach vs MCMC results
# Load Coefplot2


beta1 <- coef(M2) #M2 results
se1   <- sqrt(diag(vcov(M2)))#M2 results


beta2 <- OUT1[1:K,1]# JAGS results
se2   <- OUT1[1:K,2] # JAGS results


coefplot2(beta1, se1, offset = 0, col =1, 
          varnames = names(beta1), xlim = c(-6,5),
          cex.var = 1.1, main = "")

coefplot2(beta2, se2, offset = 0.25, col =1, 
          varnames = names(beta1), add = TRUE)
#Top ones are JAGS
#########################################################




#########################################################
#Section 3.7 Model Interpretation
# Calculate 95% credible intervals inside JAGS
# Make a graph

X <- model.matrix(~ Level + Location,
                    data = PO)
K <- ncol(X)


MyData <- expand.grid(Level = c("Lower", "Upper"),
                      Location  = c("A", "B", "C"))
Xp <- model.matrix(~Level + Location, data = MyData)

# Matrices X and Xp are passed on to JAGs


win.data <- list(Y    = PO$Hsimilis,
                 N    = nrow(PO),
                 X    = X,
                 K    = K,
                 Xp   = Xp,
                 LogN = log(nrow(PO)),
                 b0   = rep(0, K),
                 B0   = diag(0.00001, K)
                 )

sink("GLM.txt")
cat("
model{
    #1. Priors
    beta  ~ dmnorm(b0[], B0[,])

    #2. Likelihood
    for (i in 1:N){
      Y[i] ~ dbern(p[i])
      logit(p[i]) <- max(-20, min(20, eta[i]))
      eta[i]      <- inprod(beta[], X[i,])
      l[i] <- Y[i] * log(p[i]) + (1 - Y[i]) * log(1 - p[i])
  }
  L <- sum(l[1:N])
  AIC <- -2 * L + 2 * K
  BIC <- -2 * L + LogN * K

  #Predict probabilities
  for (j in 1:6){
    etaP[j]      <- inprod(beta[], Xp[j,])
    logit(pi[j]) <- etaP[j]
  }
}
",fill = TRUE)
sink()


#Use the same initial values for the betas and sigma
#from the previous section

#Parameters to estimate
params <- c("beta", "L", "AIC", "BIC", "pi")


######################################################
#Execute the code

J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLM.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

J1.upd <- update(J0, n.iter=15000, n.thin = 10)
out <- J1.upd$BUGSoutput
OUT1 <- MyBUGSOutput(out, c(uNames("pi", 6)))

#Code to plot the posterior means and 95% credible intervals
MyX <- 1:6
MyXLab <- paste(MyData[,1], MyData[,2], sep = " ")
par(mar = c(5,5,2,2))
plot(x = MyX, cex.lab = 1.5,
     y = OUT1[,1],
     xlab = "Covariate combinations",
     ylab = "Predicted probabilities",
     axes = FALSE,
     type = "p",
     pch = 16,
     ylim = c(0, 1))
axis(2)
axis(1, at = MyX, labels = MyXLab )
box()
#F. Plot predicted values +/- 2* SE
segments(MyX, OUT1[,3],
         MyX, OUT1[,4])

##################################################

#Overdispersion
#Discrepancy measures (used for checking overdispersion)

#      YNew[i]   ~ dbern(p[i])   #New data
#      PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
#      D[i]       <- pow(PRes[i], 2)
#      DNew[i]    <- pow(PResNew[i], 2)

#Outside the loop add:

# Fit    <- sum(D[1:N])
# FitNew <- sum(DNew[1:N])

#Ajust the parameters to save (include Fit and FitNew) and calculate

# mean(out$sims.list$FitNew > out$sims.list$Fit)
#####################################################


#####################################################
#3.8 Discussion
#Why are there small differences?
#Simulate some data

set.seed(12345)
N <- 100
x <- sort(runif(N))
xm <- x - mean(x)
alpha <- 1
beta  <- 2
eta   <- alpha + beta*xm
pi    <- exp(eta) / (1 + exp(eta))
Y1 <- rbinom(N, size = 1, prob = pi) 

T1 <- glm(Y1 ~ xm,family = binomial)
summary(T1)           

    
X <- model.matrix(~ xm)    
X <- matrix(X, nrow = N)
K <- ncol(X)
win.data <- list(Y  = Y1,
                 N  = N,
                 X  = X,
                 K  = K,
                 LogN = log(N),
                 b0 = rep(0, K),
                 B0 = diag(0.00001, K)
                 )

sink("GLMSim.txt")
cat("
model{
    #Priors
    beta  ~ dmnorm(b0[], B0[,])  

    #######################
    #Likelihood 
    for (i in 1:N){  
      Y[i] ~ dbern(p[i])
      logit(p[i]) <- eta[i]
      eta[i]      <- inprod(beta[], X[i,])
      l[i]        <- Y[i] * log(p[i]) + (1 - Y[i]) * log(1 - p[i])
  } 
  L <- sum(l[1:N])
  AIC <- -2 * L + 2 * K
  BIC <- -2 * L + LogN * K
}
",fill = TRUE)
sink()


#Set the initial values for the betas and sigma
inits <- function () {
  list(
    beta  = rnorm(K, 0, 0.1)
    )  }

#Parameters to estimate
params <- c("beta", "L", "AIC", "BIC")


######################################################
J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLMSim.txt",
           n.thin = 100,
           n.chains = 3,
           n.burnin = 4000,
           n.iter   = 5000)

J1.upd <- update(J0, n.iter=15000, n.thin = 10)  
out <- J1.upd$BUGSoutput

min(out$sims.list$AIC)
AIC(M1)

min(out$sims.list$BIC)
BIC(M1)

print(out, digits = 3)
summary(T1)        
#Conclusion: For large N, results are identical
#            For small N up to 5% difference

#Conclusion from the actual analysis on the data:
#If the data is not good enough...then keep the model simple!

###############################END of CODE######################