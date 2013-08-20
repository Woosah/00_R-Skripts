#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Seedlings")
Seedlings <- read.table(file = "Seedling.txt",
                        header = TRUE)

names(Seedlings)

##################################################################
# Rename some of the variables
# The seedling diameters are often oval, rather than round-
# so we measure diameter twice and take the mean of diam.1 and diam.2

Seedlings$Biomass  <- (Seedlings$stem.biomass + Seedlings$leafy.biomass)
Seedlings$Diameter <- (Seedlings$diam.1 + Seedlings$diam.2)/2
Seedlings$Height   <- Seedlings$crown.mm
Seedlings$Leaves   <- Seedlings$leaves
##################################################################



###################################################################
#Load packages and library files
library(lattice)  #Needed for data exploration
library(mgcv)     #Needed for data exploration
library(R2jags)   #MCMC
source(file = "/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises/HighstatLibV6.R")  
source(file = "/Users/Highstat/applicat/HighlandStatistics/MCMC/R/MCMCSupportHighstat.R")
##################################################################



##################################################################
#Data exploration
#Outliers
MyVar <- c("Biomass", "Diameter", "Height", "Leaves")
Mydotplot(Seedlings[,MyVar])

#Number of observation per species
table(Seedlings$Species)

#Collinearity
MyVar <- c("Diameter", "Height", "Leaves")
Mypairs(Seedlings[, MyVar])


#Relationships
Myxyplot(Seedlings, MyVar, "Biomass")
Mybwplot(Seedlings,c("Biomass"),"Species")

#Create a new categorical variable, Gen_sp
Seedlings$Gen_Sp<-factor(paste(substr(Seedlings$Genus,1,1),
                         Seedlings$Species, sep=". "))


xyplot(Biomass ~ Diameter | Gen_Sp,
       data = Seedlings,
       strip = function(bg='white', ...) strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "same"),
                            y = list(relation = "same")),
              xlab = list(label = "Diameter", cex = 1.5),
              ylab = list(label = "Biomass", cex = 1.5),
              panel=function(x,y){
                panel.grid(h=-1, v= 2)
                panel.points(x,y,col=1,pch =1)
                tmp<-lm(y~x)
                MyData <- data.frame(x = seq(min(x), max(x), length = 25))
                p1 <- predict(tmp, newdata = MyData, type ="response")
                panel.lines(MyData$x,p1, col = 1, lwd = 2)
              })
##################################################################




##################################################################
#Section 6.4
M1 <- lm(Biomass  ~ Species * Diameter + Height + Leaves,
         data = Seedlings)

summary(M1)
drop1(M1, test = "F")

#Model validation
E1 <- rstandard(M1)
F1 <- fitted(M1)
plot(x = F1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)


########################################################
#Section 6.5
#Figure 6.7
par(mfrow = c(2,2), mar = c(5,5,2,2))
x <- seq(0,1, length = 25)
mu <- exp(1 + 2 * x)

plot(x,mu, type = "l", cex.lab = 1.5,
     xlab = "Covariate",
     ylab = "Biomass")
x1 = seq(0,25, length = 200)

Shape <- 15
Scale <- 5 / Shape
plot(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l",
     xlab = "Possible Biomass values",
     ylab = "Probability",
     xlim = c(0,25),
     cex.lab = 1.5)

Shape <- 10
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 5
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 3
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 2
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 1
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")


###########
Shape <- 15
Scale <- 10 / Shape
plot(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l",
     xlab = "Possible Biomass values",
     ylab = "Probability",
     xlim = c(0,25),
     cex.lab = 1.5)

Shape <- 10
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 5
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 3
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 2
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 1
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")



###############
Shape <- 15
Scale <- 15 / Shape
plot(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l",
     xlab = "Possible Biomass values",
     ylab = "Probability",
     xlim = c(0,25),
     cex.lab = 1.5)

Shape <- 10
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 5
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 3
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 2
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 1
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")
#############################################################




#Figure 6.8
z1 <- seq(0, 1, length = 50)
Bio <- exp(1 + 2 * z1)
Shape <- 15
Z<-matrix(nrow=50,ncol=50)
for (i in 1:50){
	Scale <- Bio[i] / Shape 
	Z[,i]<-dgamma(x=Bio, shape=Shape, scale = Scale )
}

persp(x = z1, y = Bio, z = Z,
                 scale = TRUE,
                 theta = 130, phi = 20, expand = 1,
                 ltheta = 120, shade = 0.5, 
                 ticktype = "detailed",
                 xlab = "Covariate", 
                 ylab = "Biomass", 
                 zlab = "Probability",
                 main = "")  -> res
round(res,3)
lines (trans3d(z1, y = Bio, 
               z = rep(0,50), 
               pmat = res), col = grey(0.5), lwd=5)

lines (trans3d(z1[45:50], y = Bio[45:50], 
               z = rep(0,6), 
               pmat = res), col = 1, lwd=5)
################################################



################################################
#Section 6.5.5
M2 <- glm(Biomass  ~ Diameter + Height + Leaves,
          data = Seedlings,
          family = Gamma(link ="log") )
          
print(summary(M2),signif.stars = FALSE, digits =3)


#Calculate Pearson residuals manually
X <- model.matrix(~ Diameter + Height + Leaves,
                    data = Seedlings)

eta <- X %*% coef(M2)
mu  <- exp(eta)
EPearson  <- (Seedlings$Biomass - mu)/(mu)
N <- nrow(Seedlings)
p <- length(coef(M2))
dispersion <- sum(EPearson^2)/( N - p)
dispersion

summary(M2)$dispersion

#Calculation of the SCALE parameter
Scale <- fitted(M2,type = response) / 13.46856


#Figure 6.9
par(mar = c(5,5,2,2))
plot(x=Seedlings$Diameter,
     y = Seedlings$Biomass,pch = 16, 
     col = grey(0.1), cex = 1,
     cex.lab = 1.5,
     xlab = "Diameter",
     ylab = "Biomass",
     ylim = c(0,40))

MyData <- data.frame(Diameter = seq(min(Seedlings$Diameter),
                                     max(Seedlings$Diameter),
                                     length =100),
                     Height = mean(Seedlings$Height),
                     Leaves = mean(Seedlings$Leaves)                 
                                     )
P3 <- predict(M2, newdata = MyData, type = "response")
lines(MyData$Diameter, P3, lwd = 3)

MyData <- data.frame(Diameter = seq(min(Seedlings$Diameter),
                                     max(Seedlings$Diameter),
                                     length =25),
                     Height = mean(Seedlings$Height),
                     Leaves = mean(Seedlings$Leaves)                 
                                     )
P3 <- predict(M2, newdata = MyData, type = "response")

for (i in 1:nrow(MyData)){
  Scale <- P3[i] / 13.46856
  yi <- rgamma(1000, shape = 13.46856, scale = Scale)
  points(rep(MyData$Diameter[i], 1000), 
         yi, cex = 0.5, col = grey(0.6), pch = 16)
}
#####################################################




###########################################################
#Section 6.5.7
M2B <- glm(Biomass  ~ Diameter + Height + Leaves,
          data = Seedlings,
          family = Gamma(link ="identity"),
          start = c(0, 0.5, 0, 0) )
          


MyData <- data.frame(Diameter = seq(min(Seedlings$Diameter),
                                     max(Seedlings$Diameter),
                                     length =100),
                     Height = mean(Seedlings$Height),
                     Leaves = mean(Seedlings$Leaves)                 
                                     )
P2B <- predict(M2B, newdata = MyData, type = "response")
par(mar=c(5,5,2,2))
plot(x=Seedlings$Diameter,
     y = Seedlings$Biomass,pch = 16, 
     col = grey(0.4), cex = 0.5,
     cex.lab = 1.5,
     xlab = "Diameter",
     ylab = "Biomass")
lines(MyData$Diameter, P2B, lwd = 3)
############################################################


          

############################################################
#Section 6.6

#Standarize covariates
Seedlings$DiameterS <- scale(Seedlings$Diameter)
Seedlings$HeightS   <- scale(Seedlings$Height)
Seedlings$LeavesS   <- scale(Seedlings$Leaves)

M3 <- glm(Biomass  ~ DiameterS + HeightS + LeavesS,
          data = Seedlings,
          family = Gamma(link ="log") )
          
print(summary(M3), digits =3)



####################################################
#Jags modelling code
#Create the  matrix X
X <- model.matrix(~ 1 + DiameterS + HeightS + LeavesS, data = Seedlings)
K <- ncol(X)

win.data <- list(Biomass  = Seedlings$Biomass,
                 X        = X,
                 N        = nrow(Seedlings),
                 b0       = rep(0, K),
                 B0       = diag(0.0001, K)
                 )


sink("SeedlingsGLM.txt")
cat("
model{

  #1. Priors for regression coefficients
  beta ~ dmnorm(b0[], B0[,])
  r ~ dgamma(0.01, 0.01 )

  #2. Likelihood
  for (i in 1:N){
     Biomass[i] ~  dgamma(r, lambda[i])
     lambda[i]  <- r / mu[i]
     log(mu[i]) <- max(-20, min(20, eta[i]))
     eta[i]     <- inprod(beta[],X[i,])
  }
}
",fill = TRUE)
sink()


#Initial values and parameters to store
inits <- function () {
   list(
    beta          = rnorm(K, 0, 0.01),
    r           = 1 )  }

#Parameters to estimate
params <- c("beta", "r")

J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "SeedlingsGLM.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

#Results
out <- J0$BUGSoutput
out

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), "r"))
print(OUT1, digits = 3)


#Estimate the scale using the formula from Veralbles and Ripley (2002)
library(MASS)
MyShape <- gamma.shape(M3)
MyShape

#To asses mixing, plot the cahins
MyBUGSChains(out,c(uNames("beta",K),"r"))

#No need for this:
#J1.upd <- update(J0, n.iter = 50000, n.thin = 10)
#out <- J1.upd$BUGSoutput

#MCMC chains

MyBUGSHist(out,c(uNames("beta",K),"r"))

AllPar <- out$sims.matrix[,c(uNames("beta", K),"r")]
#######################################################



#######################################################
#Section 6.6.8
Beta <- OUT1[1:4,1]
r    <- OUT1[5,1]

MyData <- data.frame(DiameterS = seq(min(Seedlings$DiameterS),
                                     max(Seedlings$DiameterS),
                                     length =100),
                     HeightS = 0,
                     LeavesS = 0)

X <- model.matrix(~ 1 + DiameterS + HeightS + LeavesS,
                  data = MyData)

eta <- X %*% Beta
mu  <- exp(eta)



par(mar = c(5,5,2,2))
plot(x=Seedlings$DiameterS,
     y = Seedlings$Biomass,pch = 16, 
     col = grey(0.1), cex = 1,
     cex.lab = 1.5,
     xlab = "Diameter",
     ylab = "Biomass",
     ylim = c(0,40))
lines(MyData$DiameterS, mu, lwd = 3)




Beta3000 <- AllPar[,1:4]
dim(Beta3000)
eta <- X %*% t(Beta3000)
mu  <- exp(eta)
dim(mu)


par(mfrow = c(1,2), mar = c(5,5,2,2))
plot(x=Seedlings$DiameterS,
     y = Seedlings$Biomass,pch = 16, 
     col = grey(0.1), cex = 1,
     cex.lab = 1.5,
     xlab = "Diameter",
     ylab = "Biomass",
     ylim = c(0,40))

for (i in 1:3000){
  lines(MyData$DiameterS, mu[,i], lwd = 1, col = grey(0.6))
}





plot(x=Seedlings$DiameterS,
     y = Seedlings$Biomass,pch = 16, 
     col = grey(0.5), cex = 1,
     cex.lab = 1.5,
     xlab = "Diameter",
     ylab = "Biomass",
     ylim = c(0,40))

Info <- matrix(nrow=100, ncol =3)
for (i in 1:100){
  CI <- quantile(mu[i,], probs = c(0.025, 0.975))
  Info[i,1] <- mean(mu[i,])
  Info[i,2:3] <- CI
}
lines(MyData$DiameterS, Info[,1], lwd = 3)
lines(MyData$DiameterS, Info[,2], lwd = 3)
lines(MyData$DiameterS, Info[,3], lwd = 3)









Beta3000 <-out$sims.matrix[,uNames("beta",K)]
dim(Beta3000)


eta <- X %*% t(Beta3000)
mu  <- exp(eta)
dim(mu)

# To present a pediction interval, add te following code to JAGS:

#for (j in 1:100){
#  etaNew[j]     <-inprod(beta[],X1[j,])
#  log(muNew[j]) <- etaNew[j]))
#  lambdaNew[j]  <- r/muNew[j]
#  YNew[j]   ~ dgamma(r,lambdaNew[j])
# }

################################################




################################################
#Section 6.6.9
#If J0 is used: 3,000 iterations
#If J1.upd us used, replace 3000 by 15000 
Beta3000   <- out$sims.matrix[,uNames("beta", K)]
r3000      <- out$sims.matrix[,"r"]
eta        <- win.data$X %*% t(Beta3000)
mu         <- exp(eta)
ExpY       <- mu
VarY       <-  (1/r3000) * mu^2

Biomass3000 <- matrix(rep(win.data$Biomass, 3000), nrow = 290)
E3000 <- (Biomass3000 - mu)  /sqrt(VarY)
E     <- rowSums(E3000) / 3000
Fit   <- rowSums(mu) / 3000



###############################################
#Figure 6.14
plot(x = Fit,
     y = E,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     pch  = 16)
abline(h = 0, v = 0, lty = 2)


boxplot(E ~ Species,
        data = Seedlings,
        xlab = "Species",
        ylab = "Pearson residuals")
abline(h = 0, v = 0, lty = 2)


#Figure 6.15
Seedlings$E <- E
MyVar <- c("DiameterS", "HeightS", "LeavesS")
Myxyplot(Seedlings, MyVar, "E")
#########################################################



#########################################################
#Section 6.7
X <- model.matrix(~ 1 + Species * DiameterS +
                  HeightS + LeavesS,
                  data = Seedlings)
K <- ncol(X)
#############################################################



#############################################################
#Section 6.81

#Define the matrix X
X <- model.matrix(~ 1 + DiameterS +
                    HeightS + LeavesS, data = Seedlings)
K <- ncol(X)

#We need to recode categorical variable Species
re <- as.numeric(Seedlings$Species)
NumSpecies <- length(unique(Seedlings$Species))

win.data <- list(Biomass  = Seedlings$Biomass,
                 X        = X,
                 N        = nrow(Seedlings),
                 b0       = rep(0, K),
                 B0       = diag(0.0001, K),
                 re       = re,
                 a0       = rep(0, NumSpecies),
                 A0       = diag(1, NumSpecies)
                 )

sink("SeedlingsGLMM.txt")
cat("
model{
  #1. Priors for regression coefficients
  beta     ~ dmnorm(b0[], B0[,])
  tau      ~ dgamma( 0.01, 0.01 )
  a        ~ dmnorm(a0[], tau.ri * A0[,])
  b        ~ dmnorm(a0[], tau.rs * A0[,])
  num.ri   ~ dnorm(0, 0.0016)
  num.rs   ~ dnorm(0, 0.0016)
  denom.ri ~ dnorm(0, 1)
  denom.rs ~ dnorm(0, 1)
  sigma.ri <- abs(num.ri / denom.ri)
  sigma.rs <- abs(num.rs / denom.rs)
  tau.ri   <- 1 / (sigma.ri * sigma.ri)
  tau.rs   <- 1 / (sigma.rs * sigma.rs)

  #2. Likelihood
  for (i in 1:N){
     Biomass[i] ~  dgamma(tau, mu.eff[i])
     mu.eff[i]  <- tau / mu[i]
     log(mu[i]) <- max(-20, min(20, eta[i]))
     eta[i]     <- inprod(beta[],X[i,])  + a[re[i]] + b[re[i]] * X[i,2]
  }
}
",fill = TRUE)
sink()

# Initial values and parameters to save:
inits <- function () {
   list(
    beta     = rnorm(K, 0, 0.01),
    tau      = 1,
    a        = rnorm(NumSpecies, 0, 0.1),
    b        = rnorm(NumSpecies, 0, 0.1),
    num.ri   = rnorm(1, 0, 25),
    num.rs   = rnorm(1, 0, 25),
    denom.ri = rnorm(1, 0, 1),
    denom.rs = rnorm(1, 0, 1)
 )  }

#Parameters to estimate
params <- c("beta", "tau", "sigma.ri", "sigma.rs","a","b")

#Run JAGS
J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "SeedlingsGLMM.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

J1.upd <- update(J0, n.iter = 50000, n.thin = 10)
out <- J1.upd$BUGSoutput

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), "tau","sigma.ri","sigma.rs"))
print(OUT1, digits = 3)
#################################################




#################################################
#Section 6.9
X <- model.matrix(~ 1 + DiameterS + HeightS + LeavesS, data = Seedlings)
K <- ncol(X)

win.data <- list(Biomass = Seedlings$Biomass,
                 X       = X,
                 N       = nrow(Seedlings),
                 b0      = rep(0, K),
                 B0      = diag(0.0001, K),
                 Zeros   = rep(0, nrow(Seedlings))
                 )

sink("SeedlingsLM.txt")
cat("
model{
  #1. Priors for regression coefficients
  beta ~ dmnorm(b0[], B0[,])
  num   ~ dnorm(0, 0.0016)
  denom ~ dnorm(0, 1)
  sigma <- abs(num / denom)

  #2. Likelihood
  C <- 10000
  for (i in 1:N){
  	 Zeros[i] ~ dpois(Zeros.mean[i])
     Zeros.mean[i] <- -L[i] + C
     l1[i] <- -0.5 * log(2 * 3.1415) - 0.5 * log(sigma)
     l2[i] <- -0.5 * pow(Biomass[i] - mu[i], 2) / sigma
     L[i]  <-  l1[i] + l2[i]
     mu[i] <- eta[i]
     eta[i] <- inprod(beta[],X[i,])
  }
}
",fill = TRUE)
sink()


#Initial values and parameters to save
inits <- function () {
   list(
    beta          = rnorm(K, 0, 0.01),
    num      = rnorm(1,0, 25),
    denom    = rnorm(1,0, 1)
 )  }

#Parameters to estimate
params <- c("beta", "sigma")

#Run JAGS
J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "SeedlingsLM.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

J1.upd <- update(J0, n.iter = 50000, n.thin = 10)

out <- J1.upd$BUGSoutput


OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma"))

print(OUT1, digits = 5)

#Compare to the results obtained by the lm function
M4 <- lm(Biomass ~ DiameterS + HeightS +
         LeavesS, data = Seedlings)
summary(M4)
###############################################



###############################################
#Section 6.9.3
# Tobit model in Jags

Seedlings$DiameterS <- scale(Seedlings$Diameter)
Seedlings$HeightS   <- scale(Seedlings$Height)
Seedlings$LeavesS   <- scale(Seedlings$Leaves)

X <- model.matrix(~ 1 + DiameterS + HeightS + LeavesS, data = Seedlings)
X <- as.matrix(X)
win.data <- list(Biomass = Seedlings$Biomass,
                 X       = X,
                 N       = nrow(Seedlings),
                 b0      = rep(0, ncol(X)),
                 B0      = diag(0.0001, ncol(X)),
                 Zeros   = rep(0, nrow(Seedlings))
                 )


sink("SeedlingsLMTrunc.txt")
cat("
model{
  #1. Priors for regression coefficients
  beta ~ dmnorm(b0[], B0[,])
  num   ~ dnorm(0, 0.0016)
  denom ~ dnorm(0, 1)
  sigma <- abs(num / denom)
  tau <- 1 / (sigma * sigma)

  #2. Likelihood
  C <- 10000
  for (i in 1:N){
  	 Zeros[i] ~ dpois(Zeros.mean[i])
     Zeros.mean[i] <- -L[i] + C
     l1[i] <- -0.5 * log(2 * 3.1415) - 0.5 * log(sigma)
     l2[i] <- -0.5 * pow(Biomass[i] - mu[i], 2) / sigma
     alpha[i] <- -mu[i] / sigma
     phi[i] <- (1 / sqrt(2 * 3.1415)) * exp(-0.5 *  (alpha[i]^2) )
     l3[i] <- log(1 - phi[i])
     L[i]  <-  l1[i] + l2[i] - l3[i]
     mu[i] <- eta[i]
     eta[i]     <- inprod(beta[],X[i,])
  }
}
",fill = TRUE)
sink()



inits <- function () {
   list(
    beta          = rnorm(ncol(X), 0, 0.01),
    num      = rnorm(1,0, 25), 
    denom    = rnorm(1,0, 1)
 )  }

#Parameters to estimate
params <- c("beta", "sigma")

#Run JAGS
J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "SeedlingsLMTrunc.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

#J1.upd <- update(J0, n.iter=50000, n.thin = 10)  

out <- J0$BUGSoutput

OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma"))
print(OUT1, digits = 5)





#########################################
# Tobit model with random effects in Jags
# You only need to change the predictor function eta

# eta[i] <- inprod (beta[],X[i,]) + a[re[i]]+
#                   b[re[i]]* X[i,2]
      
      
      
#########################################################
#Figures 6.16 and 6.17

Seedlings$DiameterS <- as.numeric(scale(Seedlings$Diameter))
Seedlings$HeightS   <- as.numeric(scale(Seedlings$Height))
Seedlings$LeavesS   <- as.numeric(scale(Seedlings$Leaves))


M4 <- lm(Biomass  ~ DiameterS + HeightS + LeavesS, 
          data = Seedlings)
print(summary(M4),signif.stars = FALSE, digits =3)



par(mar = c(5,5,2,2))
x<- seq(-4,4,length = 100)
y <- dnorm(x,0,1)
plot(x,y, type = "l", xlab = "Biomass", ylab ="Normal density curve", cex.lab=1.5)

x1<-x[x<0]
y1<-y[x<0]

polygon(c(x1, rev(x1)),
        c(0*y1, rev(y1)),
        col = 1,border=NULL,
        density = 30   )




range(Seedlings$DiameterS)
MyData <- data.frame(DiameterS = seq(-3.1, 3.3, length = 10), 
                     HeightS = 0,
                     LeavesS = 0)
                    
                     
P1 <- predict(M4, newdata = MyData, se = TRUE, interval = "prediction")

par(mar = c(5,5,2,2))
plot(MyData$DiameterS,P1$fit[,1], type = "l", lwd = 2,
     xlab = "Diameter", 
     ylab = "Predicted biomass", 
     cex.lab = 1.5,
     ylim = c(-3,13))
lines(MyData$DiameterS,P1$fit[,2], lty = 2)
lines(MyData$DiameterS,P1$fit[,3], lty =2)
abline(h=0, lty = 2)     
                     
                     
polygon(c(MyData$DiameterS, rev(MyData$DiameterS)),
        c(P1$fit[,2], rev(P1$fit[,3])),
        col = 1,border=NULL,
        density = 30   )





                   
