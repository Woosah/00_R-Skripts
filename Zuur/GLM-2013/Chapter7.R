#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Cheetahs")
ZooData <- read.table(file = 'ZooData.txt', header = TRUE)
names(ZooData)
str(ZooData)
###################################################################




###################################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
library(R2jags)
library(lme4)
source(file = "/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises/HighstatLibV6.R")  
source(file = "/Users/Highstat/applicat/HighlandStatistics/MCMC/R/MCMCSupportHighstat.R")
##################################################################





##################################################################
#House keeping
ZooData$fRaised  	<- factor(ZooData$Raised)
ZooData$fFeeding 	<- factor(ZooData$Feeding)
ZooData$fOc 		<- factor(ZooData$Oc)
ZooData$fOther 		<- factor(ZooData$Other)
ZooData$fEnrichment <- factor(ZooData$Enrichment)
ZooData$fGroup 		<- factor(ZooData$Group)
ZooData$fSex 		<- factor(ZooData$Sex)
ZooData$fZoo 		<- factor(ZooData$Zoo) 
##################################################################
 




###################################################################
#Data exploration
#Outliers
MyVar <- c("Scans", "Number", "Proportion", "Size", "Visual", "Visitors", "Enclosure", "Vehicle", "Diet", "Age")
Mydotplot(ZooData[,MyVar])
ZooData$LSize <- log(ZooData$Size)



#Collinearity
#Not in book:
MyVar <- c("LSize", "Visual", "Visitors", "Enclosure", "Vehicle", "Diet", "Age")
pairs(ZooData[,MyVar], lower.panel = panel.cor)


corvif(ZooData[ ,c("LSize",  "Visual", "Visitors", 
                   "Age", "Enclosure", "Vehicle", 
                   "Diet" , "fRaised", 
                   "fFeeding", "fOc",  "fOther", 	
                   "fEnrichment","fGroup", 	"fSex")])



corvif(ZooData[ ,c("LSize",  "Visual", "Visitors", 
                   "Age", "Enclosure", "Vehicle", 
                   "fRaised", 
                   "fFeeding", "fOc",  "fOther",   
                   "fEnrichment","fGroup", 	"fSex")])

corvif(ZooData[ ,c("LSize",  "Visitors", 
                   "Age", "Enclosure", "Vehicle", 
                   "fRaised", 
                   "fFeeding", "fOc",  "fOther",   
                   "fEnrichment","fGroup",   "fSex")])
#Remove diet and visual


#Number of observations per zoo
table(ZooData$Zoo)
ZD <- ZooData[ZooData$Zoo != 1, ]  #Remove the first zoo
ZD$fZoo <- factor(ZD$Zoo)
dim(ZD)
#######################################################





#######################################################
#Section 7.4

#Section 7.4.1
#Standardise all continuous covariates
MyNorm <- function(x) { (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}
ZD$cLSize        <- MyNorm(ZD$LSize)
ZD$cVisitors     <- MyNorm(ZD$Visitors)
ZD$cAge          <- MyNorm(ZD$Age)
ZD$cEnclosure    <- MyNorm(ZD$Enclosure)
ZD$cVehicle      <- MyNorm(ZD$Vehicle)


#Section 7.4.3
ZD$Neg <- ZD$Scans - ZD$Number
M1 <- glmer(cbind(Number, Neg) ~ cLSize + cVisitors+ fFeeding+ 
            fOc + fOther + fEnrichment + fGroup + fSex + 
            cEnclosure + cVehicle+ cAge + (1 | fZoo), 
            family = binomial, data = ZD)
summary(M1)

#Section 7.4.4
E1 <- residuals(M1)
p1 <- length(fixef(M1)) + 1
Overdisp1 <- sum(E1^2) / (nrow(ZD) - p1)
Overdisp1


#Figure 7.3
ZD$E1 <- E1
vars <- c("cLSize", "cVisitors",  "cEnclosure", "cVehicle", "cAge")
Myxyplot(ZD, vars,"E1")

table(ZD$Number)

#Figure 7.4
F1 <- fitted(M1, type = "response")
par(mar = c(5, 5, 2, 2))
plot(F1,E1, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)


#Section 7.4.5
ZD$Eps <- 1:nrow(ZD)
M2 <- glmer(cbind(Number, Neg) ~ cLSize + cVisitors+ fFeeding+ 
            fOc + fOther + fEnrichment + fGroup + fSex + 
            cEnclosure + cVehicle+ cAge + (1 | fZoo) + (1 | Eps), 
            family = binomial, data = ZD)
summary(M2)

#Figure 7.5
par(mar = c(5,5,2,2))
plot(resid(M2) ~ fitted(M2, type = "response"), cex.lab = 1.5)
abline(h = 0, lty = 2)


X <- model.matrix( ~ cLSize + cVisitors+ fFeeding+ 
                     fOc + fOther + fEnrichment + fGroup + fSex + 
                     cEnclosure + cVehicle+ cAge, data = ZD)
Betas <- fixef(M2)
  
z   <- ranef(M2)$fZoo$'(Intercept)'
Eps <- ranef(M2)$Eps$'(Intercept)'
re <- as.numeric(ZD$fZoo)

eta <- X %*% Betas + z[re] + Eps
pi <- exp(eta) / (1+ exp(eta))
fitted(M2) - pi


#Section 7.4.6
NewData <- expand.grid(cLSize = seq(-2.2, 2.54, length = 50), 
                       cVisitors = 0,
                       fFeeding    = c("1","2"),                       
                       fOther      = c("1","2"),
                       fOc         = c("1","2"),
                       fEnrichment = c("1","2"),
                       fGroup      = c("1","2"),
                       fSex        = c("1","2"),
                       cEnclosure  = 0,
                       cVehicle    = 0,
                       cAge        = 0)

head(NewData)                       


X <- model.matrix(~ cLSize + cVisitors+ fFeeding+ 
                    fOc + fOther + fEnrichment + fGroup + fSex + 
                    cEnclosure + cVehicle+ cAge,
                    data = NewData)

NewData$eta         <- X %*% fixef(M2)
NewData$SuccessPred <- exp(NewData$eta) / (1 + exp(NewData$eta))
head(NewData,15)

#Get the standard errors
NewData$VarPred <- diag(X %*% vcov(M2) %*% t(X))
NewData$sePred  <- sqrt(NewData$VarPred)


NewData$seLow  <- exp(NewData$eta - 1.96 * NewData$sePred) / (1 + exp(NewData$eta - 1.96 * NewData$sePred))
NewData$seHigh <- exp(NewData$eta + 1.96 * NewData$sePred) / (1 + exp(NewData$eta + 1.96 * NewData$sePred))
head(NewData, 10)


#Change Feeding
Res1a <- NewData[NewData$fFeeding == "1" &
                  NewData$fOther == "1" & 
                  NewData$fOc == "1" &
                  NewData$fGroup == "1" &
                  NewData$fEnrichment == "1" & 
                  NewData$fSex   == "1",]

Res1b <- NewData[NewData$fFeeding == "2" &
  NewData$fOther == "1" & 
  NewData$fOc == "1" & 
  NewData$fGroup == "1" &
  NewData$fEnrichment == "1" & 
  NewData$fSex   == "1",]




#Change Oc
Res2a <- NewData[NewData$fFeeding == "1" &
  NewData$fOther == "1" & 
  NewData$fOc == "1" & 
  NewData$fGroup == "1" &
  NewData$fEnrichment == "1" & 
  NewData$fSex   == "1",]

Res2b <- NewData[NewData$fFeeding == "1" &
  NewData$fOther == "1" & 
  NewData$fOc == "2" &  
  NewData$fGroup == "1" &
  NewData$fEnrichment == "1" & 
  NewData$fSex   == "1",]



#Change Group
Res3a <- NewData[NewData$fFeeding == "1" &
  NewData$fOther == "1" & 
  NewData$fOc == "1" &  
  NewData$fGroup == "1" &
  NewData$fEnrichment == "1" & 
  NewData$fSex   == "1",]

Res3b <- NewData[NewData$fFeeding == "1" &
  NewData$fOther == "1" & 
  NewData$fOc == "1" &  
  NewData$fGroup == "2" &
  NewData$fEnrichment == "1" & 
  NewData$fSex   == "1",]




#Figure 7.6
Ally <- c(Res1a$SuccessPred, Res1b$SuccessPred, Res2a$SuccessPred, Res2b$SuccessPred, Res3a$SuccessPred, Res3b$SuccessPred)
Allx <- c(Res1a$cLSize, Res1b$cLSize, Res2a$cLSize, Res2b$cLSize, Res3a$cLSize, Res3b$cLSize) 
N <- length(Res1a$SuccessPred)

Names <- c("Feeding 1","Feeding 2","Oc 1","Oc 2","Group 1","Group 2")
AllID <- rep(Names , each = N)

AllID <- factor(AllID, levels = c("Feeding 1", "Feeding 2", "Oc 1", "Oc 2","Group 1","Group 2"),
                       labels = c("Predictable feeding time",  "No predictable feeding time",
                                  "Can't see other cheetahs", "See other cheetahs",
                                  "solitary", "group"))

SELOW <- c(Res1a$seLow, Res1b$seLow, Res2a$seLow, Res2b$seLow, Res3a$seLow, Res3b$seLow)
SEHI  <- c(Res1a$seHigh, Res1b$seHigh, Res2a$seHigh, Res2b$seHigh, Res3a$seHigh, Res3b$seHigh)



xyplot(Ally ~ Allx | AllID, type = "l",
       layout = c(2,3),
       xlab = list(label="Standardized log transformed size", cex=1.5),
       ylab = list(label="Probability of stereotype behaviour", cex=1.5),
       strip = function(bg='white',cex.lab = 1.5,...)
                strip.default(bg='white', ...),  
       panel=function(x,y,subscripts,...){
         ID<-AllID[subscripts][1]
         x1   <- Allx[AllID == ID]
         y1   <- Ally[AllID == ID]
         SEUP <- SEHI[AllID == ID]
         SELO <- SELOW[AllID== ID]
         panel.grid(h = -1, v = 2)
         panel.polygon(c(x1,rev(x1)),c(SELO,rev(SEUP)), col="grey65",border=NULL)
         panel.lines(x1,y1,lwd=3,lty=1,col=1)
         
       }
)
#########################################################




#########################################################
#Section 7.5
X <- model.matrix(~cLSize + cVisitors + fFeeding + fOc + 
                   fOther + fEnrichment + fGroup + fSex +
                   cEnclosure + cVehicle + cAge, data = ZD)
K <- ncol(X)

re <- as.numeric(ZD$fZoo)
NumZoos <- length(unique(ZD$Zoo))

win.data <- list(Y        = ZD$Number,
                 Scans    = ZD$Scans,
                 N        = nrow(ZD),
                 X        = X,
                 b0       = rep(0, K),
                 B0       = diag(0.0001, K),
                 re       = re, 
                 a0       = rep(0, NumZoos),
                 A0       = diag(1, NumZoos)
                 )

sink("GLMM1.txt")
cat("
model{
    #1. Priors
    beta  ~ dmnorm(b0[], B0[,])  
    a     ~ dmnorm(a0[], tau * A0[,]) 
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
 
    #2. Likelihood 
    for (i in 1:N){  
      Y[i] ~ dbin(p[i], Scans[i])
      logit(p[i]) <- eta[i]
      eta[i]      <- inprod(beta[], X[i,]) + a[re[i]]
  } 
}
",fill = TRUE)
sink()


#Set the initial values for the betas and sigma
inits <- function () {
  list(
    beta  = rnorm(K, 0, 0.1),
    a     = rnorm(NumZoos, 0, 0.1),
    num   = rnorm(1, 0, 26), 
    denom = rnorm(1, 0, 1)
    )  }

#Parameters to estimate
params <- c("beta", "a", "sigma")

J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLMM1.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

out <- J0$BUGSoutput
J0$BUGSoutput

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), "sigma"))
print(OUT1, digits = 3)


#Section 7.5.4
sink("GLMM1.txt")
cat("
model{
    #1. Priors
    beta  ~ dmnorm(b0[], B0[,])  
    a     ~ dmnorm(a0[], tau * A0[,]) 
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
 
    #2. Likelihood 
    for (i in 1:N){  
      Y[i] ~ dbin(p[i], Scans[i])
      logit(p[i]) <- eta[i]
      eta[i]      <- inprod(beta[], X[i,]) + a[re[i]]

      #3. Measures of discrepancy      
      ExpY[i] <- p[i] * Scans[i]
      VarY[i] <- p[i] * Scans[i] * (1 - p[i])
      PRes[i] <- (Y[i] - ExpY[i])/sqrt(VarY[i])

      YNew[i]    ~ dbin(p[i], Scans[i]) #New data 
      PResNew[i] <- (YNew[i] - ExpY[i]) /sqrt(VarY[i])
      D[i]       <- pow(PRes[i], 2)
      DNew[i]    <- pow(PResNew[i], 2)
  }    
     Fit         <- sum(D[1:N])
     FitNew      <- sum(DNew[1:N])

}
",fill = TRUE)
sink()

params <- c("beta", "a", "sigma", "PRes", "Fit", "FitNew")

J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLMM1.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

out <- J0$BUGSoutput
out

E <- out$mean$PRessum(E^2) / (nrow(ZD) - 12 - 1)


beta <- out$mean$betaeta <- win.data$X %*% betapi <- exp(eta) / (1+ exp(eta))ExpY <- win.data$Scans * piVarY <- win.data$Scans * pi * (1- pi)E <- (win.data$Y - ExpY) / sqrt(VarY)
sum(E^2) / (nrow(ZD) - 12 - 1)

mean(out$sims.list$FitNew < out$sims.list$Fit)
###########################################################





###########################################################
#Section 7.6

#Figure 7.7
x <- seq(0,1, length = 50)
y <- dbeta(x, shape1 = 1, shape2 = 1 )

par(mar = c(5,5,2,2))
plot(x,y, type = "l", ylim = c(0,5),
     xlab = "Values for y",
     ylab = "Probability density",
     cex.lab = 1.5)
lines(x, dbeta(x, shape1 = 0.5, shape2 = 0.5))
lines(x, dbeta(x, shape1 = 3, shape2 = 3))
lines(x, dbeta(x, shape1 = 1, shape2 = 4))




X <- model.matrix(~cLSize + cVisitors + fFeeding + fOc + 
                   fOther + fEnrichment + fGroup + fSex +
                   cEnclosure + cVehicle + cAge, data = ZD)
K <- ncol(X)
re <- as.numeric(ZD$fZoo)
NumZoos <- length(unique(ZD$Zoo))

win.data <- list(Y        = ZD$Number,
                 Scans    = ZD$Scans,
                 N        = nrow(ZD),
                 X        = X,
                 b0       = rep(0, K),
                 B0       = diag(0.0001, K),
                 re       = re, 
                 a0       = rep(0, NumZoos),
                 A0       = diag(1, NumZoos)
                 )

sink("GLMM2.txt")
cat("
model{
    #1. Priors beta and random effects
    beta  ~ dmnorm(b0[], B0[,])  
    a     ~ dmnorm(a0[], tau * A0[,]) 
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
    numtheta   ~ dnorm(0, 0.0016) 
    denomtheta ~ dnorm(0, 1)
    theta <- abs(numtheta / denomtheta) 
 
    #2. Likelihood 
    for (i in 1:N){  
      Y[i] ~ dbin(p[i], Scans[i])
      p[i] ~ dbeta(shape1[i], shape2[i])
      shape1[i] <- theta * pi[i]        #a
      shape2[i] <- theta * (1 - pi[i])  #b
      
      logit(pi[i]) <- eta[i]
      eta[i]      <- inprod(beta[], X[i,]) + a[re[i]]

      #Discrepancy stuff (not in the book)
      ExpY[i] <- p[i] * Scans[i]
      VarY[i] <- p[i] * Scans[i] * (1 - p[i]) * (1 + (Scans[i] / (theta + 1)))
      PRes[i] <- (Y[i] - ExpY[i]) / sqrt(VarY[i])
      YNew[i]   ~ dbin(p[i], Scans[i])   #New data
      PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
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
    beta  = rnorm(K, 0, 0.1),
    a     = rnorm(NumZoos, 0, 0.1),
    num   = rnorm(1, 0, 25), 
    denom = rnorm(1, 0, 1),
    numtheta   = rnorm(1, 0, 25), 
    denomtheta = rnorm(1, 0, 1)    
    )  }

#Parameters to estimate
params <- c("beta", "a", "sigma", "theta", "PRes","Fit", "FitNew")

J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLMM2.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)

J1.upd <- update(J0, n.iter=50000, n.thin = 10)  
out <- J1.upd$BUGSoutput

mean(out$sims.list$FitNew > out$sims.list$Fit)

J0$BUGSoutput

OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), "sigma", "theta"))
print(OUT1, digits = 3)
OUTBBGLMM <- OUT1  #Used for later

vars <- c(uNames("beta",K), "sigma", "theta")
MyBUGSACF(out, vars)
MyBUGSHist(out, vars)
MyBUGSChains(out, vars)


#Section 7.6.5
eta <- win.data$X %*% OUT1[1:12,1]
pi <- exp(eta) / (1+exp(eta))
ExpY <- win.data$Scans * pi

par(mar = c(5,5,2,2))
plot(x = ExpY, 
     y = out$mean$PRes,
     xlab = "Fitted values",
     ylab = "Pearson residuals", cex.lab = 1.5)
abline(h = 0)
#############################################################




#############################################################
#Section 7.7
X <- model.matrix(~cLSize + cVisitors + fFeeding + fOc + 
                   fOther + fEnrichment + fGroup + fSex +
                   cEnclosure + cVehicle + cAge, data = ZD)
K <- ncol(X)
re <- as.numeric(ZD$fZoo)
NumZoos <- length(unique(ZD$Zoo))
names(ZD)

N <- nrow(ZD)
ZD$PropBeta <- (ZD$Proportion * (N - 1) + 0.5) / N

win.data <- list(Y        = ZD$PropBeta ,
                 N        = nrow(ZD),
                 X        = X,
                 b0       = rep(0, K),
                 B0       = diag(0.0001, K),
                 re       = re, 
                 a0       = rep(0, NumZoos),
                 A0       = diag(1, NumZoos)
                 )

sink("GLMM2.txt")
cat("
model{
    #1. Priors
    beta  ~ dmnorm(b0[], B0[,])  
    a     ~ dmnorm(a0[], tau * A0[,]) 
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
 
    numtheta   ~ dnorm(0, 0.0016) 
    denomtheta ~ dnorm(0, 1)
    theta <- abs(numtheta / denomtheta) 
 
    #2. Likelihood 
    for (i in 1:N){       
      Y[i] ~ dbeta(shape1[i], shape2[i])
      shape1[i] <- theta * pi[i]        #a
      shape2[i] <- theta * (1 - pi[i])  #b
      
      logit(pi[i]) <- eta[i]
      eta[i]       <- inprod(beta[], X[i,]) + a[re[i]]

      ExpY[i] <- pi[i] 
      VarY[i] <- pi[i] * (1 - pi[i])  / (theta + 1)
      PRes[i] <- (Y[i] - ExpY[i]) / sqrt(VarY[i])

      #Discrepancy measures (used for checking overdispersion)
      YNew[i]   ~ dbeta(shape1[i], shape2[i])   #New data
      PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
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
    beta  = rnorm(K, 0, 0.1),
    a     = rnorm(NumZoos, 0, 0.1),
    num   = rnorm(1, 0, 25), 
    denom = rnorm(1, 0, 1),
    numtheta   = rnorm(1, 0, 25), 
    denomtheta = rnorm(1, 0, 1)    
    )  }

#Parameters to estimate
params <- c("beta", "a", "sigma", "theta", "PRes","Fit", "FitNew")

J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLMM2.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 40000,
           n.iter   = 50000)


out <- J0$BUGSoutput
mean(out$sims.list$FitNew > out$sims.list$Fit)


J0$BUGSoutput
OUT1 <- MyBUGSOutput(out, c(uNames("beta",K), "sigma", "theta"))
print(OUT1, digits = 3)
OUTBGLMM <- OUT1



AllBeta <- out$sims.list$beta

MyData <- expand.grid(cLSize      = 0,
                      cVisitors   = 0,
                      fFeeding    = c("1","2"),
                      fOc         = c("1","2"),
                      fOther      = c("1","2"),
                      fEnrichment = c("1","2"),
                      fGroup      = c("1","2"), 
                      fSex        = c("1","2"),
                      cEnclosure  = 0, 
                      cVehicle    = 0, 
                      cAge        = 0)

X <- model.matrix(~cLSize + cVisitors + fFeeding + fOc + 
                   fOther + fEnrichment + fGroup + fSex +
                   cEnclosure + cVehicle + cAge, data = MyData)

eta <- X %*% t(AllBeta)
pi <- exp(eta) / (1 + exp(eta))




##############################################
#Section 7.8 

library(coefplot2)

M1 <- glm(cbind(Number, Neg) ~ cLSize + cVisitors+ fFeeding+ 
  fOc + fOther + fEnrichment + fGroup + fSex + 
  cEnclosure + cVehicle+ cAge, 
            family = binomial, data = ZD)

M2 <- glmer(cbind(Number, Neg) ~ cLSize + cVisitors+ fFeeding+ 
  fOc + fOther + fEnrichment + fGroup + fSex + 
  cEnclosure + cVehicle+ cAge + (1 | fZoo), 
            family = binomial, data = ZD)


M3 <- glmer(cbind(Number, Neg) ~ cLSize + cVisitors+ fFeeding+ 
  fOc + fOther + fEnrichment + fGroup + fSex + 
  cEnclosure + cVehicle+ cAge + (1 | fZoo) + (1 | Eps), 
            family = binomial, data = ZD)


#1: Binomial GLM
beta1 <- coef(M1)
se1   <- sqrt(diag(vcov(M1)))

#2: Binomial GLMM with random effect zoo
beta2 <- fixef(M2)
se2   <- sqrt(diag(vcov(M2)))

#3: Binomial GLMM with random effect zoo and observation level random intercept
beta3 <- fixef(M3)
se3   <- sqrt(diag(vcov(M3)))

#Beta-binomial GLMM
beta4 <- OUTBBGLMM[1:12,1]
se4   <- OUTBBGLMM[1:12,2]

#Beta-binomial GLMM
beta5 <- OUTBGLMM[1:12,1]
se5   <- OUTBGLMM[1:12,2]




coefplot2(beta1, se1, offset = 0, col =1, 
          varnames = names(beta1), xlim = c(-4,2),
          cex.lab = 1.5)

coefplot2(beta2, se2, offset = 0.15, col =1, 
          varnames = names(beta1), add = TRUE)

coefplot2(beta3, se3, offset = 0.3, col = 1, 
          varnames = names(beta1), add = TRUE)

coefplot2(beta4, se4, offset = 0.45, col = 1, 
          varnames = names(beta1), add = TRUE)

coefplot2(beta5, se5, offset = 0.6, col = 1, 
          varnames = names(beta1), add = TRUE)



###########################################################################
#Section 7.9 Model selection

f1 <- formula(cbind(Number, Neg) ~ cLSize+ fRaised+ cVisitors+ fFeeding+ fOc+ fOther+ fEnrichment+ fGroup+ fSex+ cEnclosure+ cVehicle+ cAge+  (1 | fZoo) + (1 | Eps))
f2 <- formula(cbind(Number, Neg) ~ cLSize+ fRaised+ cVisitors+ fFeeding+ fOc+ fOther+ fEnrichment+ fGroup+ fSex+ cEnclosure+ cVehicle+ cAge+ cLSize:fOc+ cLSize:fOther+ fGroup:fEnrichment+ cLSize:fSex+ fSex:cAge +(1 | fZoo) + (1 | Eps))
f3 <- formula(cbind(Number, Neg) ~ fSex+ cAge+ fSex:cAge +(1 | fZoo) + (1 | Eps))
f4 <- formula(cbind(Number, Neg) ~ cVisitors+ fSex+ cAge+ cVehicle+ (1 | fZoo) + (1 | Eps))
f5 <- formula(cbind(Number, Neg) ~ cVisitors+ fSex+ cAge+ cVehicle+ cVisitors:cVehicle+ fSex:cAge+ fSex:cVisitors+ cAge:cVisitors + (1 | fZoo) + (1 | Eps))
f6 <- formula(cbind(Number, Neg) ~ cLSize+ fOther+ fOc+ fFeeding+ fGroup+ cEnclosure+ fEnrichment + fRaised+ cLSize:fOc+ cLSize:fOther+ cLSize:fGroup+ fGroup:fFeeding+ fGroup:fEnrichment+ (1 | fZoo) + (1 | Eps))
f7 <- formula(cbind(Number, Neg) ~ cLSize+ fOther+ fOc+ fFeeding+ fGroup+ cEnclosure+ fEnrichment+ fRaised+(1 | fZoo) + (1 | Eps))
f8 <- formula(cbind(Number, Neg) ~ cLSize+ fOc+ fOther+ fFeeding+  fGroup+ fSex+ cAge+ cEnclosure +(1 | fZoo) + (1 | Eps))
f9 <- formula(cbind(Number, Neg) ~ cLSize+ fOc+ fOther+ fFeeding+  fGroup+ fSex+ cAge+ cEnclosure+ cLSize:fSex+ cLSize:cAge+ fOc:fOther+ cLSize:fFeeding+ fGroup:fSex +(1 | fZoo) + (1 | Eps))
f10<- formula(cbind(Number, Neg) ~ cVehicle+ cVisitors+ fOc+ cEnclosure+ cVehicle:cVisitors+ cVehicle:fOc+ fOc:cVisitors+(1 | fZoo) + (1 | Eps))
f11<- formula(cbind(Number, Neg) ~ fEnrichment+ fRaised+ fGroup+ fSex+ cAge+ fEnrichment:fRaised+ fEnrichment:fSex+ fEnrichment:cAge+ fEnrichment:fGroup +(1 | fZoo) + (1 | Eps))


#And fit them
M1 <- glmer(f1, family = binomial, data = ZD)
M2 <- glmer(f2, family = binomial, data = ZD)
M3 <- glmer(f3, family = binomial, data = ZD)
M4 <- glmer(f4, family = binomial, data = ZD)
M5 <- glmer(f5, family = binomial, data = ZD)
M6 <- glmer(f6, family = binomial, data = ZD)
M7 <- glmer(f7, family = binomial, data = ZD)
M8 <- glmer(f8, family = binomial, data = ZD)
M9 <- glmer(f9, family = binomial, data = ZD)
M10 <- glmer(f10, family = binomial, data = ZD)
M11 <- glmer(f11, family = binomial, data = ZD)

#Calculate AIC values, differences in AIC, and AIC weights
AICs <- AIC(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11)

MyDf <- AICs[,1]
AICsNum <- AICs[,2]
minAW <- min(AICsNum)
Delta <- AICsNum-minAW
RL <- exp(-0.5 * Delta)
wi <- RL / sum(RL)
Z <- data.frame(MyDf, AICsNum, Delta, wi)
Z <- round(Z, digits = 3)
colnames(Z)<- c("Df", "AIC", "AIC differences", "Akaike weights")
Z
####################################################



####################################################
#Section 7.10.1

Beta  <- out$sims.list$beta
a     <- t(out$sims.list$a) #This is called z in the book?
theta <- out$sims.list$theta

eta <- win.data$X %*% t(Beta) + a[win.data$re,]
pi <- exp(eta) / (1+exp(eta))


Y <- win.data$Y
Lik <- vector(length = 3000)
for (k in 1:3000){
	Shape1 <- theta[k] * pi[,k]
	Shape2 <- theta[k] * (1 - pi[,k])
    l1 <- lgamma(Shape1 + Shape2)
    l2 <- lgamma(Shape1)
    l3 <- lgamma(Shape2)
	l1 <- lgamma(Shape1 + Shape2)
    l2 <- lgamma(Shape1)
    l3 <- lgamma(Shape2)
	f <-  l1 - l2 - l3  + (Shape1-1)*log(Y) + (Shape2-1) * log(1-Y)
    Lik[k] <- -2 * sum(f)
}
MeanDeviance <- mean(Lik)  #deviance average over all MCMC iterations
#Is correct!!! Compare to out$mean$Deviance
out$mean$deviance



#Calculate posterior mean and use this to calculate deviance
beta  <- out$mean$beta
z     <- out$mean$a
theta <- out$mean$theta

REZoo <- as.vector(z[win.data$re])
eta  <- as.vector(win.data$X %*% beta) + REZoo

pi <- exp(eta) / (1+exp(eta))
pi <- pi
Shape1 <- theta * pi
Shape2 <- theta * (1-pi)

l1 <- lgamma(Shape1 + Shape2)
l2 <- lgamma(Shape1)
l3 <- lgamma(Shape2)
f <-  l1 - l2 - l3  + (Shape1-1)*log(Y) + (Shape2-1) * log(1-Y)
DevianceMean <- -2 * sum(f)


pd <- MeanDeviance - DevianceMean
pd
DIC <- MeanDeviance + pd
DIC

var(Lik)/2

p <- 12 + 1 + 1
AIC <- min(out$sims.list$deviance) + 2*p
AIC

p <- 12 + 1 + 1
AIC <- min(Lik) + 2*p
AIC


p <- 12 + 1 + 1
AIC <- DevianceMean + 2* p
AIC
########################################################








########################################################
#Section 7.10.2

Zoo <- as.numeric(ZD$fZoo)
NumZoos <- length(unique(Zoo))

X <- model.matrix(~cLSize + cVisitors + fFeeding + fOc + 
                   fOther + fEnrichment + fGroup + fSex +
                   cEnclosure + cVehicle + cAge, data = ZD)
              
                           
win.data <- list(Number   = ZD$Number,
                 Scans    = ZD$Scans,
                 N        = nrow(ZD),
                 X        = X,                                
                 Zoo      = Zoo, 
                 NumZoos  = NumZoos
                 )

sink("GLMM1.txt")
cat("
model{
    
    #Priors for regression coefficients 
    #beta ~ dmnorm(b0[1:4], B0[,])  
    for (i in 1:NumZoos){ a[i] ~ dnorm(0,tau.zoo) }
    tau.zoo <- 1 / (sigma.zoo * sigma.zoo)
    sigma.zoo ~ dunif(0.001,10)
    
    PInd <- 0.5
    for (i in 1:12) {
      Ind[i] ~ dbern(PInd)
  	  betaT[i] ~ dnorm(0,0.0001)
  	  beta[i] <- Ind[i] * betaT[i]
      #beta[i] ~ dnorm(0,0.0001)
} 

    #######################
    #Likelihood 
    for (i in 1:N){  
       Number[i] ~ dbin(p[i], Scans[i])  
       logit(p[i]) <- max(-10, min(10, eta[i]))
       eta[i]      <- inprod(beta[], X[i,]) + a[Zoo[i]] 
    
       ExpY[i] <- p[i] * Scans[i]
       VarY[i] <- p[i] * Scans[i] * (1 - p[i])
       PRes[i] <- (Number[i] - ExpY[i])/sqrt(VarY[i])

          
       #Discrepancy measures
       YNew[i]     ~ dbin(p[i], Scans[i])   #New data
       PResNew[i] <- (YNew[i]   - ExpY[i]) / sqrt(VarY[i])
       D[i]       <- pow(PRes[i], 2)
       DNew[i]    <- pow(PResNew[i], 2)
    } 
    Fit    <- sum(D[1:N])
    FitNew <- sum(DNew[1:N])

}
    ",fill = TRUE)
sink()




#Set the initial values for the betas and sigma
inits <- function () {
  list(
    betaT  = rnorm(12, 0, 0.1),
    #beta  = rnorm(12, 0, 0.1),
    Ind    = rep(1,12),
    a      = rnorm(8, 0, 0.1),
    sigma.zoo = rlnorm(1, meanlog = 0, sdlog = 0.1)
    )  }

#Parameters to estimate
params <- c("beta", "a", "sigma.zoo", "PRes", "Ind")
#params <- c("beta", "a", "sigma.zoo", "PRes", "Fit", "FitNew")

######################################################

J0 <- jags(data = win.data,
           inits = inits,
           parameters = params,
           model.file = "GLMM1.txt",
           n.thin = 10,
           n.chains = 3,
           n.burnin = 10000,
           n.iter   = 50000)

J1.upd <- update(J0, n.iter=100000, n.thin = 100)  
out <- J1.upd$BUGSoutput

OUT1 <- MyBUGSOutput(out, uNames("Ind",12))
print(OUT1, digits = 3)

#MyBUGSChains(out, uNames("beta", 12))






