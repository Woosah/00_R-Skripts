#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Kiwi")
KW <- read.table(file = "pollen.txt", 
                 header = TRUE, 
                 dec = ".")

names(KW)
str(KW)


########################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
library(lme4)
#library(glmmADMB)
library(R2jags)
source(file = "/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises/HighstatLibV6.R")  
source("/Users/Highstat/applicat/HighlandStatistics/MCMC/R/MCMCSupportHighstat.R")
########################################################


########################################################
#House keeping
KW$fHive <- factor(KW$Hive)
########################################################




########################################################
#Data exploration
xyplot(Dandelion ~ Time | Treatment, 
       xlab = list("Time (days)", cex = 1.5),
       ylab = list("Number of dandelion pollen grains", cex = 1.5),
       data = KW, layout = c(3,1),
       groups = Hive,
       type = "l", col = 1,
       strip = strip.custom(bg = 'white',
                            par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same"))
)
########################################################





########################################################
#Section 5.5
M1 <- glmer(Dandelion ~ Time * Treatment + (1|fHive),
            data = KW, family = poisson)

print(summary(M1), digits = 2, signif.stars=FALSE)
drop1(M1, test = "Chi")


M1A <- glmer(Dandelion ~ Time + Treatment + (1|fHive),
            data = KW, family = poisson)

logLik(M1) - logLik(M1A)


#Check for overdispersion
E1 <- resid(M1, type = "pearson")
N  <- nrow(KW)
p  <- length(fixef(M1)) + 1
Overdispersion <- sum(E1^2) / (N - p)
Overdispersion


F1 <- fitted(M1, type ="response")
par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x = KW$Time, y = E1,
     xlab = "Time", 
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

boxplot(E1 ~ Treatment, data = KW, 
        xlab = "Treatment", 
        ylab = "Pearson residuals", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2)

boxplot(E1 ~ fHive, data = KW, 
        xlab = "Hive", 
        ylab = "Pearson residuals", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2)
#########################################################






#########################################################
#Section 5.6
KW$Timec <- KW$Time - mean(KW$Time)

X <- model.matrix(~ Time * factor(Treatment) , data = KW)
K <- ncol(X)
head(X)
Nre <- length(unique(KW$Hive))

win.data1 <- list(Y       = KW$Dandelion,
                  X       = X,
                  N       = nrow(KW),
                  re      = KW$Hive,
                  b0      = rep(0,K),
                  B0      = diag(0.0001, K),
                  a0      = rep(0,Nre),
                  A0      = diag(Nre))
win.data1


#Modelling code
sink("GLMM.txt")
cat("
model {
    #1. Diffuse priors for regression parameters
    beta ~ dmnorm(b0[], B0[,]) 

    #Diffuse Priors for random effect hive
    a ~ dmnorm(a0,  tau.re * A0[,])
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma.re <- abs(num / denom) 
    tau.re <- 1 / (sigma.re * sigma.re)
    
    #2. Likelihood
    for (i in 1:N) {
        Y[i]  ~  dpois(mu[i])
        log(mu[i]) <- eta[i]  #max(-20, min(20, eta[i]))
        eta[i]  <- inprod(beta[], X[i,]) + a[re[i]]
        
        #3. Discrepancy measures (used for checking overdispersion)
        YNew[i]   ~ dpois(mu[i])   #New data
        ExpY[i]    <- mu[i]
        VarY[i]    <- mu[i]
        PRes[i]    <- (Y[i]    - ExpY[i]) / sqrt(VarY[i])
        PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
        D[i]       <- pow(PRes[i], 2)
        DNew[i]    <- pow(PResNew[i], 2)
    }
    beta65 <- beta[6] - beta[5]
    Fit         <- sum(D[1:N])
    FitNew      <- sum(DNew[1:N])
}
",fill = TRUE)
sink()


#Initial values for each chain, for each parameter
inits1 <- function () {
   list(beta     = rnorm(K, 0, 0.01),
        a        = rnorm(Nre, 0, 1),
        num      = rnorm(1, 0, 25), 
        denom    = rnorm(1, 0, 1)
        )}


#Parameters to store
params1 <- c("beta", "a", "sigma.re", 
             "PRes", "Fit", "FitNew", "beta65")


######################################################
#Figure 5.2
par(mar = c(5,5,2,2))
dcauchy0_25 <- function(x) dcauchy(x, location=0, scale=25)
curve(dcauchy0_25, from=0, to=200,
xlab = "sigma_hive",
ylab ="Half-Cauchy(25) distribution",
cex.lab = 1.5)
######################################################




J0 <- jags(data = win.data1,
             inits = inits1,
             parameters = params1,
             model.file = "GLMM.txt",
             n.thin   = 10,
             n.chains = 3,
             n.burnin = 25000,
             n.iter   = 35000)
             
out <- J0$BUGSoutput
print(out)


OUT1 <- MyBUGSOutput(out, c(uNames("beta",6), 
                          "sigma.re")) 
print(OUT1, digits =3)



MyBUGSChains(out, c(uNames("beta",6), "sigma.re"))

#Not in book:
summary(M1)
vars <- c(uNames("beta",6), 
                          "sigma.re")
MyBUGSACF(out, vars)
MyBUGSHist(out, vars)
MyBUGSChains(out, vars)

     
E <- out$mean$PRes
sum(E^2) / (nrow(KW) - p)
######################################################## 






########################################################
#Section 5.7
library("glmmADMB")
M2 <- glmmadmb(Dandelion ~ Time * Treatment, 
             random =~ 1|fHive, 
             family = "nbinom", data=KW)
summary(M2)


E2 <- resid(M2, type = "pearson")

#Overdisp from glmm adnb
p <- 6 + 1 + 1
Overdispersion2 <-sum(E2^2) / (N - p)
Overdispersion2
######################################################






######################################################
#Section 5.8
X <- model.matrix(~ Time * factor(Treatment) , data = KW)
K <- ncol(X)
head(X)
Nre <- length(unique(KW$Hive))

win.data1 <- list(Y       = KW$Dandelion,
                  X       = X,
                  N       = nrow(KW),
                  re      = KW$Hive,
                  b0      = rep(0,K),
                  B0      = diag(0.0001, K),
                  a0      = rep(0,Nre),
                  A0      = diag(Nre))
win.data1


#Modelling code
sink("GLMM_NB.txt")
cat("
model {
    #1. Priors for regression parameters
    beta ~ dmnorm(b0[], B0[,]) 

    #Priors for random effect hive
    a ~ dmnorm(a0,  tau.re * A0[,])
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma.re <- abs(num / denom) 
    tau.re <- 1 / (sigma.re * sigma.re)
        
    #Prior for size
#    size ~ dgamma(0.001, 0.001)
    numS ~ dnorm(0, 0.0016) 
    denomS ~ dnorm(0, 1)
    size <- abs(numS / denomS)

    #Likelihood
    for (i in 1:N) {
        Y[i]  ~  dnegbin(p[i], size)
        p[i] <- size / (size + mu[i])
        log(mu[i]) <- eta[i]  #max(-20, min(20, eta[i]))
        eta[i]  <- inprod(beta[], X[i,]) + a[re[i]]

        #3. Discrepancy measures (used for checking overdispersion)
        YNew[i]   ~ dnegbin(p[i], size)
        ExpY[i]    <- mu[i]
        VarY[i]    <- mu[i] + pow(mu[i],2) / size
        PRes[i]    <- (Y[i]    - ExpY[i]) / sqrt(VarY[i])
        PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
        D[i]       <- pow(PRes[i], 2)
        DNew[i]    <- pow(PResNew[i], 2)
    }
    Fit         <- sum(D[1:N])
    FitNew      <- sum(DNew[1:N])
    beta65 <- beta[4] - beta[3]

}
",fill = TRUE)
sink()



#Initial values for each chain, for each parameter
inits2 <- function () {
   list(beta     = rnorm(K, 0, 0.1),
        a        = rnorm(Nre, 0, 1),
        num      = rnorm(1,0, 25), 
        denom    = rnorm(1,0, 1),
        numS     = rnorm(1, 0, 25) ,
        denomS   = rnorm(1, 0, 1)
        )}


#Parameters to sore
params2 <- c("beta", "a", "sigma.re", 
             "PRes", "size",
             "Fit", "FitNew",
             "beta65")




######################################################

N0 <- jags(data = win.data1,
             inits = inits2,
             parameters = params2,
             model.file = "GLMM_NB.txt",
             n.thin   = 10,
             n.chains = 3,
             n.burnin = 25000,
             n.iter   = 30000)
             
N1 <- update(N0, n.iter = 20000)
N2 <- update(N1, n.iter = 50000)

out <- N2$BUGSoutput
print(out)

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), 
                          "sigma.re", "size","beta65"))
print(OUT1, digits =3)

MyBUGSChains(out, c(uNames("beta",K), 
                          "sigma.re", "size"))
                          
vars <- c(uNames("beta",K), 
                          "sigma.re", "size","beta65")
MyBUGSACF(out, vars)
MyBUGSHist(out, vars)
#Could do with more MCMC iterations!      
      
      
E <- out$mean$PRes
p <- 6 + 1
sum(E^2) / (nrow(KW) - p)
mean(out$sims.list$FitNew > out$sims.list$Fit)



#Model validation
Betas <- OUT1[1:6,1]
a     <- out$mean$a
eta   <- win.data1$X %*% Betas + as.vector(a[win.data1$re])
mu    <- exp(eta)

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = mu, y = E,
     xlab = "Fitted values",
     ylab = "Pearson resiuals",
     cex.lab = 1.5)
abline(h=0, lty = 2)
     
plot(x=KW$Time, y = E,
     xlab = "Time (days)",
     ylab = "Pearson resiuals",
     cex.lab = 1.5)
abline(h=0, lty = 2)

boxplot(E ~ Treatment, data = KW,
     xlab = "Treatment",
     ylab = "Pearson resiuals",
     cex.lab = 1.5)
abline(h=0, lty = 2)

boxplot(E ~ Hive, data = KW,
     xlab = "Hive",
     ylab = "Pearson resiuals",
     cex.lab = 1.5)
abline(h=0, lty = 2)



#######################################
#Section 5.8.8
MyData <- expand.grid(Time = seq(1,4,length = 10),
                      Treatment = levels(KW$Treatment)) 
Z <- model.matrix(~Time + Treatment, data = MyData)
MyData$eta <- Z %*% Betas
MyData$mu  <- exp(MyData$eta)

#And for the random effects:
Betas <- OUT1[1:6,1]
a <- out$mean$a
eta <- win.data1$X %*% Betas 
RE  <- a[win.data1$re]
eta <- as.vector(eta) + RE
mu  <- exp(eta)


xyplot(Dandelion ~ Time | Treatment, 
       xlab = list("Time (days)", cex = 1.5),
       ylab = list("Number of dandelion pollen grains", cex = 1.5),
       data = KW, layout = c(3,1),
       type = "p", col = 1,
       strip = strip.custom(bg = 'white',
                            par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")),
                     
       panel = function(x,y, subscripts,...){
       	ID <- KW$Treatment[subscripts][1]
       	x1 <- MyData[MyData$Treatment == ID,"Time"]
       	y1 <- MyData[MyData$Treatment == ID,"mu"]
       	panel.lines(x1,y1, lwd = 5, col =1)
       	panel.points(x,y, pch = 16, col = 1)
       	
       	xx1 <- KW$Time[subscripts]
       	yy1 <- mu[subscripts]
       	Hives <- KW$Hive[subscripts]
       	NH <- unique(Hives)
       	for (i in 1:4){
       	panel.lines(xx1[Hives==NH[i]],
       	            yy1[Hives==NH[i]], 
       	            lwd = 1, col = 1)
       }}             
)
################################################

 



 
################################################
#Section 5.9
#Simulation of AR1 residual correlation in a Poisson GLM
#noise process:

#eps_i = phi * eps_i-1 + xsi_i
#xsi_i ~ N(0, sigma)
#eta_i = b1 + b2 * X + eps_i
#mu = exp(eta_)
#Y_i ~ Poisson(mu_i) 

#Choose some values
set.seed(123456)
N     <- 100    #Sample size
beta1 <- 1      #intercept
beta2 <- 3      #slope
X     <- seq(0, 1, length = N)  #Covariate
X     <- X - mean(X)
sigma <- 0.5
phi   <- 0.8



#Ceate AR1 noise
eps <- vector(length = N)
eps[1] <- rnorm(1, 0, sigma)
for (i in 2:N){
	eps[i] <- phi * eps[i-1] + rnorm(1,0,sigma) 
}
eps


par(mfrow = c(2,2), mar = c(5,5,2,2))
acf(eps,xlab = "Time lag",
    ylab = "ACF eps",
    cex.lab = 1.5)  #auto-correlation in eps => auto-correlation in Y
)  #This thing is auto-correlated


#Create Y data
eta <- beta1 + beta2* X + eps
mu <- exp(eta)
D <- rpois(N,lambda = mu)

plot(x=X, y = D, 
     xlab = "Covariate Z",
     ylab = "Response variable D",
     cex.lab = 1.5)  #auto-correlation in eps => auto-correlation in Y



#Apply Poisson GLM. Should be overdispersed
M1 <- glm(D ~ X, family = poisson)
summary(M1)

E1 <- resid(M1, type = "pearson")
sum(E1^2) / (N - 2)

library(MASS)
M2 <- glm.nb(D ~X)
summary(M2)
#Overdispersed and biased parameters!

acf(E1, xlab = "Time lag",
    ylab = "ACF Pearson residuals",
    cex.lab = 1.5)  #auto-correlation in eps => auto-correlation in Y

plot(exp(eps), type = "h",
     xlab = "Observation",
     ylab = "exp(eps)",
     cex.lab = 1.5)
abline(h = 1, lty = 2)


####################################################
#Section 5.9.2
#Now apply the Poisson GLM with AR1 auto-correlation in JAGS.

Xm <- model.matrix(~ X)
K <- ncol(Xm)
head(Xm)

win.data1 <- list(Y       = D,
                  X       = Xm,
                  N       = N,
                  b0      = rep(0,K),
                  B0      = diag(0.0001, K)
                 )


#Modelling code
sink("GLMAR1Test.txt")
cat("
model {
    #1. Weak priors for regression parameters
    beta ~ dmnorm(b0[], B0[,]) 

    taueps ~ dgamma(0.01, 0.01)
    rho ~ dunif(-0.99, 0.99)

    #2. Likelihood: i = 1 
    eta[1] <- inprod(beta[], X[1,])
    eta1[1] ~ dnorm(eta[1], taueps) 
    log(mu[1]) <- eta1[1]
    Y[1] ~ dpois(mu[1])

    #2. Likelihood: i > 1
    for (i in 2:N) {
        eta[i]  <- inprod(beta[], X[i,]) 
        temp[i] <- eta[i] + rho * (log(mu[i-1]) - eta[i-1])
        eta1[i] ~ dnorm(temp[i], taueps)          
        log(mu[i]) <- eta1[i] 
        Y[i]  ~  dpois(mu[i])       
    }
    
    #3. Discrepancy measures (used for checking overdispersion) 
    for (i in 1:N) {   
        YNew[i]   ~ dpois(mu[i])   #New data
        ExpY[i]    <- mu[i]
        VarY[i]    <- mu[i]
        PRes[i]    <- (Y[i]    - ExpY[i]) / sqrt(VarY[i])
        PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
        D[i]       <- pow(PRes[i], 2)
        DNew[i]    <- pow(PResNew[i], 2)
     }
    Fit         <- sum(D[1:N])
    FitNew      <- sum(DNew[1:N])
}
",fill = TRUE)
sink()



#Initial values for each chain, for each parameter
inits1 <- function () {
   list(beta     = rnorm(K, 0, 0.01),
        taueps   = runif(1, 0.1, 5),
        rho      = runif(1,-0.99,0.99)
        )}

#Parameters to sore
params1 <- c("beta",  "rho","taueps", #"a", "sigma.re", 
             "PRes", "Fit", "FitNew")


######################################################

J0 <- jags(data = win.data1,
             inits = inits1,
             parameters = params1,
             model.file = "GLMAR1Test.txt",
             n.thin   = 10,
             n.chains = 3,
             n.burnin = 25000,
             n.iter   = 30000)
             
#J1 <- update(J0, n.iter = 20000)
#J2 <- update(J1, n.iter = 50000)

out <- J0$BUGSoutput
print(out)

mean(out$sims.list$FitNew >  
     out$sims.list$Fit) 

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), 
                          "rho", "taueps"))
print(OUT1, digits =3)

#1/sqrt(out$mean$taueps)
E <- out$mean$PRes
acf(E) 

vars <- c(uNames("beta",K), 
                          "rho")
MyBUGSACF(out, vars)
MyBUGSHist(out, vars)
MyBUGSChains(out, vars)
#####################################################
      






#####################################################
#Simulate multivariate time series
#Not in the book (Section 5.9.3)
M <- 15  #Number of time series
N <- 4 #Length of each time series


sigma <- 0.5
phi <- 0.8
b1  <- 1      #intercept
b2  <- 4      #slope
Z <- 1:N 
 


#Ceate AR1 noise
eps <- matrix(nrow = N, ncol = M)
eps[1,1:M] <- rnorm(M, 0, sigma)

head(eps)

for (k in 1:M){
  for (i in 2:N){
	 eps[i,k] <- phi * eps[i-1,k] + rnorm(1,0,sigma) 
  }}
head(eps)  


acf(eps[,2])



#Create Y data
eta <- matrix(nrow = N, ncol = M)
mu  <- matrix(nrow = N, ncol = M)
Y   <- matrix(nrow = N, ncol = M)
for (k in 1:M){
  eta[,k] <- b1 + b2* Z + eps[,k]
  mu[,k]  <- exp(eta[,k])
  Y[,k]   <- rpois(N, lambda = mu[,k])
}


plot(x=Z, y = Y[,1])
par(mfrow=c(2,2))
acf(Y[,1])
acf(Y[,2])
acf(Y[,3])
acf(Y[,4])


Yk <- as.vector(Y)
Xk <- rep(X, M)
IDk <- rep(1:M, each = N)

M1 <- glm(Yk ~ Xk + factor(IDk), family = poisson)
drop1(M1, test = "Chi")

summary(M1)

E1 <- resid(M1, type = "pearson")
sum(E1^2) / (length(Yk) - length(coef(M1)))

#Overdispersed and biased parameters!
library(lattice)
xyplot(Yk ~ Xk | factor(IDk))





######################################################
#Now apply JAGS
Xmatrix <- model.matrix(~1+Z)
win.data1 <- list(Y       = Y,
                  X       = as.matrix(Xmatrix),
                  N       = N,
                  M       = M,
                  b0      = rep(0,2),
                  B0      = diag(0.0001, 2)
                 )


#Modelling code
sink("GLMTest.txt")
cat("
model {
    #Weak priors for regression parameters
    beta ~ dmnorm(b0[], B0[,]) 

    taueps ~ dgamma(0.01, 0.01)
    rho ~ dunif(-0.99, 0.99)

    for (k in 1:M) { 
      #i = 1 
      eta[1,k] <- inprod(beta[], X[1,])
      eta1[1,k] ~ dnorm(eta[1,k], taueps) 
      log(mu[1,k]) <- eta1[1,k]
      Y[1,k] ~ dpois(mu[1,k])

      #i > 1
      for (i in 2:N) {
        eta[i,k]  <- inprod(beta[], X[i,]) 
        temp[i,k] <- eta[i,k] + rho * (log(mu[i-1,k]) - eta[i-1,k])
        eta1[i,k] ~ dnorm(temp[i,k], taueps)          
        log(mu[i,k]) <- eta1[i,k] 
        Y[i,k]  ~  dpois(mu[i,k])       
    }}
    
    ##Discrepancy measures (used for checking overdispersion) 
    #for (i in 1:N) {   
    #    YNew[i]   ~ dpois(mu[i])   #New data
    #    ExpY[i]    <- mu[i]
    #    VarY[i]    <- mu[i]
    #    PRes[i]    <- (Y[i]    - ExpY[i]) / sqrt(VarY[i])
    #    PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
    #    D[i]       <- pow(PRes[i], 2)
    #    DNew[i]    <- pow(PResNew[i], 2)
    # }
    #Fit         <- sum(D[1:N])
    #FitNew      <- sum(DNew[1:N])
}
",fill = TRUE)
sink()



#Initial values for each chain, for each parameter
inits1 <- function () {
   list(beta     = rnorm(2, 0, 0.01),
        taueps   = runif(1, 0.1, 5),
        rho      = runif(1,-0.99,0.99)
        )}

#Parameters to sore
params1 <- c("beta",  "rho","taueps")#, #"a", "sigma.re", 
             #"PRes", "Fit", "FitNew")


######################################################
J0 <- jags(data = win.data1,
             inits = inits1,
             parameters = params1,
             model.file = "GLMTest.txt",
             n.thin   = 10,
             n.chains = 3,
             n.burnin = 25000,
             n.iter   = 30000)
             
#J1 <- update(J0, n.iter = 20000)
#J2 <- update(J1, n.iter = 50000)

out <- J0$BUGSoutput
print(out)

mean(out$sims.list$FitNew >  
     out$sims.list$Fit) 

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), 
                          "rho"))
print(OUT1, digits =3)

MyBUGSChains(out, uNames("beta",K))

vars <- c(uNames("beta",K), 
                          "rho")
MyBUGSACF(out, vars)
MyBUGSHist(out, vars)
MyBUGSChains(out, vars)
      
###########################################################






##########################################################
#Section 5.9.4
Xm <- model.matrix(~ Time + factor(Treatment) , data = KW)
Xm <- as.matrix(Xm)

K <- ncol(Xm)
head(Xm)
win.data2 <- list(Y     = matrix(KW$Dandelion, nrow = 15, byrow=TRUE),
                  Time  = matrix(Xm[,2], nrow = 15, byrow=TRUE),
                  Tr2   = matrix(Xm[,3], nrow = 15, byrow=TRUE),
                  Tr3   = matrix(Xm[,4], nrow = 15, byrow=TRUE),
                  b0    = rep(0,K),
                  B0    = diag(0.0001, K)
                 )


#Modelling code
sink("GLMM3.txt")
cat("
model {
    #Weak priors for regression parameters
    beta ~ dmnorm(b0[], B0[,]) 

    taueps ~ dgamma(0.01, 0.01)
    rho ~ dunif(-0.99, 0.99)

    for (i in 1:15) {
      #j = 1 
      eta[i,1] <- beta[1] + beta[2] * Time[i,1] + 
                            beta[3] * Tr2[i,1]  +
                            beta[4] * Tr3[i,1]  
                            
      eta1[i,1] ~ dnorm(eta[i,1], taueps) 
      log(mu[i,1]) <- eta1[i,1]
      Y[i,1] ~ dpois(mu[i,1])

      #j > 1
      for (j in 2:4) {
        eta[i,j] <- beta[1] + beta[2] * Time[i,j] + 
                              beta[3] * Tr2[i,j]  +
                              beta[4] * Tr3[i,j]    
 
        temp[i,j] <- eta[i,j] + rho * (log(mu[i,j-1]) - eta[i,j-1])
        eta1[i,j] ~ dnorm(temp[i,j], taueps)          
        log(mu[i,j]) <- eta1[i,j] 
        Y[i,j]  ~  dpois(mu[i,j])       
    }}
    
#    #Discrepancy measures (used for checking overdispersion) 
    for (i in 1:15) {
    	   for (j in 1:4) {
        YNew[i,j]   ~ dpois(mu[i,j])   #New data
        ExpY[i,j]    <- mu[i,j]
        VarY[i,j]    <- mu[i,j]
        PRes[i,j]    <- (Y[i,j]    - ExpY[i,j]) / sqrt(VarY[i,j])
        PResNew[i,j] <- (YNew[i,j] - ExpY[i,j]) / sqrt(VarY[i,j])
        D[i,j]       <- pow(PRes[i,j], 2)
        DNew[i,j]    <- pow(PResNew[i,j], 2)
       }
       Fiti[i]         <- sum(D[i,1:4])
       FitNewi[i]      <- sum(DNew[i,1:4])
       }
       Fit         <- sum(Fiti[1:15])
       FitNew      <- sum(FitNewi[1:15])
}
",fill = TRUE)
sink()



#Initial values for each chain, for each parameter
inits1 <- function () {
   list(beta     = rnorm(K, 0, 0.01),
        taueps   = runif(1, 0.1, 5),
        rho      = runif(1,-0.99,0.99)
        )}

#Parameters to sore
params1 <- c("beta",  "rho","taueps",
             "PRes", "Fit", "FitNew",
             "mu")


######################################################

J0 <- jags(data = win.data2,
             inits = inits1,
             parameters = params1,
             model.file = "GLMM3.txt",
             n.thin   = 10,
             n.chains = 3,
             n.burnin = 25000,
             n.iter   = 30000)
             
J1 <- update(J0, n.iter = 20000)
J2 <- update(J1, n.iter = 50000)

out <- J2$BUGSoutput
print(out)

mean(out$sims.list$FitNew >  
     out$sims.list$Fit) 

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), 
                          "rho", "taueps"))
print(OUT1, digits =3)



vars <- c(uNames("beta",K), 
                          "rho")
MyBUGSACF(out, vars)
MyBUGSHist(out, vars)
MyBUGSChains(out, vars)
      

