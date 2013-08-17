#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data

setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Spiders")
Spiders2 <- read.table(file = "Spiders.txt", 
                       header = TRUE, 
                       dec = ".")
names(Spiders2)
str(Spiders2)
###################################################################






###################################################################
#Load packages and library files
library(lattice)
library(lme4)
library(R2jags)
source(file = "/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises/HighstatLibV6.R")  
source(file = "/Users/Highstat/applicat/HighlandStatistics/MCMC/R/MCMCSupportHighstat.R")
##################################################################



##################################################################
#House keeping
Spiders2$fPlot <- factor(Spiders2$Plot)
Spiders <- Spiders2[
                    Spiders2$fPlot != "4" &
                    Spiders2$fPlot != "9" &
                    Spiders2$fPlot != "11" &
                    Spiders2$fPlot != "14" &
                    Spiders2$fPlot != "23",]
#Some plots were dropped from the analysis in the book
Spiders$fPlot <- as.factor(as.numeric(Spiders$fPlot))
##################################################################




##################################################################
#Figure 4.2
plot(x = Spiders$HerbLayer, 
     y = Spiders$Hlog10,
     xlab = "Percentage of herb layer",
     ylab = "Shannon index",
     cex.lab = 1.5, pch = 16)

M0 <- lm(Hlog10 ~ HerbLayer, 
            data = Spiders)
summary(M0)
abline(M0, lwd = 3)


#Figure 4.3
E0 <- resid(M0)
par(mar = c(5,5,2,2))
boxplot(E0 ~ fPlot, 
        data = Spiders,
        xlab = "Plot",
        ylab = "Residuals",
        cex.lab = 1.5)
abline(h = 0, lty = 2)

M1 <- lm(Hlog10 ~ HerbLayer + fPlot, 
            data = Spiders)
summary(M1)
drop1(M1, test = "F")



M1 <- lm(Hlog10 ~ HerbLayer + fPlot, 
            data = Spiders)

#Figure 4.4
par(mar = c(5,5,2,2))
plot(x = Spiders$HerbLayer, 
     y = Spiders$Hlog10,
     xlab = "Percentage of herb layer",
     ylab = "Shannon index",
     cex.lab = 1.5, pch = 16)

PlotLevels <- unique(levels(Spiders$fPlot))
for (i in PlotLevels){
   MinMaxi <- range(Spiders$HerbLayer[Spiders$fPlot==i])
   MyDatai <- data.frame(HerbLayer=
                  seq(from = MinMaxi[1],
                      to   = MinMaxi[2],
                      length=10),
                  fPlot = i)
   Pi <- predict(M1, newdata = MyDatai)
   lines(MyDatai$HerbLayer, Pi, lty=1,col=1,lwd=1)
}


MinMaxi <- range(Spiders$HerbLayer)
MyDatai <- data.frame(HerbLayer=
                  seq(from = MinMaxi[1],
                      to   = MinMaxi[2],
                      length=10),
                  fPlot = i)

P <- predict(M1, newdata = MyDatai)
lines(MyDatai$HerbLayer, P, lty=1,col=1,lwd=5)
#####################################################




#####################################################
#Section4.3
M2 <- lmer(Hlog10 ~ HerbLayer + (1 | fPlot), 
            data = Spiders)
summary(M2)

Betas  <- fixef(M2)                  #Get the betas
SE     <-  sqrt(diag(vcov(M2)))      #Get the SEs
pval   <- 2*pnorm(-abs(Betas  / SE)) #Z distribution
Output <- cbind(Betas,SE, pval)
print(Output, digits = 3)


#Figure 4.6
MinMax <- range(Spiders$HerbLayer)
MyData <- data.frame(HerbLayer=
                      seq(from = MinMax[1],
                          to   = MinMax[2],
                          length=10))

X   <- model.matrix(~ HerbLayer, data = MyData)
Fit <- X %*% fixef(M2)

par(mfrow = c(1, 2), mar = c(5,5,2,2))
plot(x = Spiders$HerbLayer, 
     y = Spiders$Hlog10,
     xlab = "Percentage of herb layer",
     ylab = "Shannon index",
     cex.lab = 1.5, pch = 1)
lines(MyData$HerbLayer, Fit, lwd = 5)


plot(x = Spiders$HerbLayer, 
     y = Spiders$Hlog10,
     xlab = "Percentage of herb layer",
     ylab = "Shannon index",
     cex.lab = 1.5, pch = 1)

lines(MyData$HerbLayer, Fit, lwd = 5)

j<-1
a <- ranef(M2)$fPlot$'(Intercept)'

PlotLevels <- unique(Spiders$fPlot)
for (i in PlotLevels){
   MinMaxi <- range(Spiders$HerbLayer[Spiders$fPlot==i])
   MyDatai <- data.frame(HerbLayer=
                  seq(from = MinMaxi[1],
                      to   = MinMaxi[2],
                      length=10),
                  fPlot = i)
   X <- model.matrix(~ HerbLayer, data = MyDatai)
               
   Pi <- X %*% fixef(M2) + a[j]
   j <- j + 1
   lines(MyDatai$HerbLayer, Pi, lty=1,col=1,lwd=1)
}
#######################################################






#######################################################
#Section 4.3.3
#Center covariates
MyNorm <- function(x){ (x-mean(x))/sd(x)}
#Add na.rm = TRUE to deal with NAs

Spiders$HerbLayerc <- MyNorm(Spiders$HerbLayer)
Spiders$GroundVegc <- MyNorm(Spiders$GroundVeg)
Spiders$Litterc    <- MyNorm(Spiders$Litter)


M3 <- lmer(Hlog10 ~ HerbLayerc + GroundVegc + Litterc + (1 | fPlot), 
            data = Spiders)
E3 <- resid(M3)
E3 <- resid(M3, type = "n")
F3 <- fitted(M3)


Betas     <- fixef(M3)
X         <- model.matrix(M3)
FitManual <- X %*% Betas

RE <- ranef(M3)$fPlot$'(Intercept)'
AllRE <- RE[as.numeric(Spiders$fPlot)]

FitManual + AllRE - fitted(M3)

ResRaw <- Spiders$Hlog10 - FitManual - AllRE
resid(M3) - ResRaw

Sigmas <- as.numeric(summary(M3)@REmat[,4])
Sigma_Plot <- Sigmas[1]
Sigma_Eps  <- Sigmas[2]

summary(M3)

M4 <- lmer(Hlog10 ~ HerbLayerc + GroundVegc + Litterc + (1 | fPlot), 
            data = Spiders, REML = FALSE)

M4A <- update(M4, .~. - HerbLayerc)
M4B <- update(M4, .~. - GroundVegc)
M4C <- update(M4, .~. - Litterc)

anova(M4, M4A)
anova(M4, M4B)
anova(M4, M4C)

drop1(M4, test = "Chi")
AIC(M4, M4A, M4B, M4C)


M5 <- lmer(Hlog10 ~ HerbLayerc + GroundVegc + Litterc + (1 | fPlot), 
            data = Spiders, REML = TRUE)

summary(M5)


range(Spiders$HerbLayerc)
MyData <- data.frame(HerbLayerc = seq(-1.3, 2, length = 10),
                     GroundVegc = 0,
                     Litterc = 0)
X <- model.matrix(~HerbLayerc + GroundVegc + Litterc, data = MyData)


Betas     <- fixef(M5)
FitManual <- X %*% Betas
SE        <- sqrt(diag(X %*% vcov(M5) %*% t(X)))
####################################################







####################################################
#Section 4.4
X <- model.matrix(~ HerbLayerc + GroundVegc + Litterc, 
                    data = Spiders)
K   <- ncol(X)
Nre <- length(unique(Spiders$fPlot))

win.data1 <- list(Y       = Spiders$Hlog10,
                  X       = X,
                  N       = nrow(Spiders),
                  re      = as.numeric(Spiders$fPlot),
                  b0      = rep(0,K),
                  B0      = diag(0.0001, K),
                  a0      = rep(0,Nre),
                  A0      = diag(Nre))
win.data1

#Modelling code
sink("lmm.txt")
cat("
model {
    #1. Weak priors for regression parameters
    beta ~ dmnorm(b0[], B0[,]) 

    #Priors for random effect plot
    a ~ dmnorm(a0,  tau.plot * A0[,])

    #Priors for the two sigmas
    tau.plot <- 1 / (sigma.plot * sigma.plot)
    tau.eps  <- 1 / (sigma.eps * sigma.eps)
    sigma.plot ~ dunif(0.001, 10)
    sigma.eps  ~ dunif(0.001, 10)
    
    #2. Likelihood of the data
    for (i in 1:N) {
        Y[i]  ~  dnorm(mu[i], tau.eps)
        mu[i]  <- eta[i] 
        eta[i] <- inprod(beta[], X[i,]) + a[re[i]]
      }
    }
",fill = TRUE)
sink()



#Initial values for each chain, for each parameter
inits1 <- function () {
   list(beta       = rnorm(K, 0, 0.01),
        a          = rnorm(Nre, 0, 1),
        sigma.eps  = runif(1, 0.001, 10),
        sigma.plot = runif(1, 0.001, 10)
        )}

#Parameters to store
params1 <- c("beta", "a", "sigma.plot", 
             "sigma.eps")

J0 <- jags(data = win.data1,
             inits = inits1,
             parameters = params1,
             model.file = "lmm.txt",
             n.thin   = 10,
             n.chains = 3,
             n.burnin = 25000,
             n.iter   = 35000)
             
J1 <- update(J0, n.iter = 20000)

out <- J1$BUGSoutput
print(out)

OUT1 <- MyBUGSOutput(out, c(uNames("beta",4), 
                          "sigma.plot", "sigma.eps"))
print(OUT1, digits =3)

#Not in book:
MyBUGSChains(out, c(uNames("beta",4), "sigma.eps"))
vars <- c(uNames("beta",4), 
                          "sigma.eps", "sigma.plot")
MyBUGSACF(out, vars)
MyBUGSHist(out, vars)
#####################################################

