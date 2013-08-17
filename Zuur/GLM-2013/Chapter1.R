#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/CD/Data/")
Fish <- read.table(file = "Baileyetal2008.txt",
                   header = TRUE)

#Inspect the results
str(Fish)
names(Fish)
##############################################################



##############################################################
#House keeping: load packages and support functions
library(lattice)
library(MASS)

#Source our library file Highstatlib.R
#Mac OS:
source("/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/RSolutions/HighstatLibV6.R")
#You need to adjust the path!
#Windows OS:
#source("C:/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/RSolutions/HighstatLibV6.R")
##############################################################



##############################################################
# Underlying question: 
# Has the Density - Depth relationship changed over time?
##############################################################



##############################################################
#Not shown in book chapter:
#Remove 2 observations (one spatial outlier and one with no spatial coordinates)
#Express depth in km

Fish2 <- na.exclude(Fish)
#Remove the spatial outlier
Fish3 <- Fish2[c(-135), ]
Fish  <- Fish3
#Express depth in km
Fish$MeanDepth <- Fish$MeanDepth / 1000
##############################################################


##############################################################
#Results in Chapter 1


#Figure 1.1
xyplot(TotAbund ~ MeanDepth | factor(Period), 
       xlab = list("Mean depth (km)", cex = 1.5),
       ylab = list("Number of fish", cex = 1.5),
       data = Fish, layout = c(2,1),
       type = "p", col = 1, pch = 16,
       strip = strip.custom(bg = 'white',
                            par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same"))
)


#Linear regression
M0 <- lm(TotAbund ~ MeanDepth, 
          data = Fish)
summary(M0)


#####################################################
#Figure 1.2
#Get residuals and fitted values
E0 <- resid(M0)
F0 <- fitted(M0)

par(mfrow = c(1,2), mar = c(5,5,3,2))
plot(x = F0, 
     y = E0,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x = Fish$MeanDepth, 
     y = Fish$TotAbund,
     xlab = "Mean Depth (km)",
     ylab = "Total abundance",
     cex.lab = 1.5,
     pch = 16)
abline(M0, lwd = 5)


#Not in book:
par(mfrow = c(2,2), mar = c(5,5,3,2))
plot(x = Fish$MeanDepth, 
     y = E0,
     xlab = "Mean Depth (km)",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

boxplot(E0 ~ Period, 
        data = Fish, 
        cex.lab = 1.5, 
        xlab = "Period",
        ylab = "Residuals")

hist(E0, 
     main = "", 
     nbreaks = 20, 
     cex.lab = 1.5, 
     xlab = "Residuals")
#################################################




#################################################
#Figure 1.3
par(mfrow = c(1,1), mar = c(5,5,3,2))
plot(x = Fish$MeanDepth, 
     y = Fish$TotAbund,
     xlab = "Mean Depth (km)",
     ylab = "Total abundance",
     cex.lab = 1.5,
     pch = 1,
     ylim = c(-300, 1200))
abline(M0, lwd = 5)
abline(h = 0, lty = 2)

range(Fish$MeanDepth)
md <- seq(0.804, 4.865, length = 10)

Beta <- coef(M0)
for (i in 1:10){
	mu <- Beta[1] + Beta[2] * md[i]
	yi <- rnorm(100, mean = mu, sd = summary(M0)$sigma)
	points(jitter(rep(md[i], 100)), jitter(yi), col = grey(0.5), pch = 16, cex = 1)
}
#################################################

 

#################################################
#Figure 1.4
par(mfrow = c(2,2), mar = c(5,5,3,2))
x1 <- 0:10
y1 <- dpois(x1,lambda = 2)
plot(x = x1, 
     y = y1, 
     type = "h", 
     xlab = "Total abundance values",
     ylab = "Probability",
     cex.lab = 1.5,
     main = "mean = 2",
     cex.main = 1.5)
     
x1 <- 0:15
y1 <- dpois(x1, lambda = 5)
plot(x = x1, 
     y = y1, 
     type = "h", 
     xlab = "Total abundance values",
     ylab = "Probability",
     cex.lab = 1.5,
     main = "mean = 5",
     cex.main = 1.5)
    
x1 <- 0:20
y1 <- dpois(x1,lambda = 10)
plot(x = x1, 
     y = y1, 
     type = "h", 
     xlab = "Total abundance values",
     ylab = "Probability",
     cex.lab = 1.5,
     main = "mean = 10",
     cex.main = 1.5)
     
x1 <- 0:200
y1 <- dpois(x1,lambda = 100)
plot(x = x1, 
     y = y1, 
     type = "h", 
     xlab = "Total abundance values",
     ylab = "Probability",
     cex.lab = 1.5,
     main = "mean = 100",
     cex.main = 1.5)
########################################################     




########################################################
#Section 1.2.5
M1 <- glm(TotAbund ~ MeanDepth, 
          data = Fish, 
          family = poisson(link = "log"))
summary(M1)

logLik(M1)

beta1 <- coef(M1)[1]
beta2 <- coef(M1)[2]
eta <- beta1 + beta2 * Fish$MeanDepth
mu  <- exp(eta)
y <- Fish$TotAbun
LogL <- sum(Fish$TotAbun * eta-mu - lgamma(Fish$TotAbun+1))
LogL

2 * sum(y*log(y/mu) - (y-mu))



#########################################################
#Figure 1.5
par(mar = c(5,5,2,2))
MyData <- data.frame(MeanDepth = seq(0.804, 4.865, length = 25))
P1 <- predict(M1, newdata = MyData, type = "response")
plot(x = Fish$MeanDepth,
     y = Fish$TotAbund,
     ylim = c(0,1300),
     xlab = "Mean depth (km)",
     ylab = "Total abundance values", cex.lab = 1.5)
     
lines(MyData$MeanDepth, P1, lwd = 3)


###########################################################
#Figure 1.6
range(Fish$MeanDepth)
par(mar = c(5,5,2,2))
MyData <- data.frame(MeanDepth = seq(0.804, 4.865, length = 25))
P1 <- predict(M1, newdata = MyData, type = "response")
plot(x = Fish$MeanDepth,
     y = Fish$TotAbund,
     ylim = c(0,1300),
     xlab = "Mean depth (km)",
     ylab = "Total abundance values", cex.lab = 1.5)
     
lines(MyData$MeanDepth, P1, lwd = 3)

HL <- seq(.804, 4.865, length = 25)
Beta <- coef(M1)
for (i in 1:25){
	mu <- exp(Beta[1] + Beta[2] * HL[i])
	yi <- rpois(150, lambda= mu)
	points(jitter(rep(HL[i], 150)), 
	       jitter(yi), col = grey(0.5), 
	       pch = 16, cex = 0.5)
}
########################################################



########################################################
#Figure 1.7

library(scatterplot3d)

x <- seq(0.804, 4.865, length = 25)
y <- exp(coef(M1)[1]+coef(M1)[2]*x)
y
z <- 0*x

ymeas=rpois(length(y),lambda=y)
#plot(x,ymeas,type="p",xlab="Covariate",ylab="Observed values")
#lines(x,y)

rr=scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="black",
      col.grid="black", pch=20,zlim=c(0,0.05),type="l",lwd=3,
      #ylim = c(9,1200),
      cex.lab = 1.5,
      xlab="Mean depth (km)",ylab="Possible values",zlab="Probability")


MyX=c(1.000,2.000,3.000,4.000,5.000)
for (i in 1:5){
  xi=MyX[i]
  yi=exp(coef(M1)[1]+coef(M1)[2]*xi)
  yseq=round(seq(0,500,by=10))
  zi=dpois(yseq, lambda=yi)
  rb=cbind(xi,yseq,zi)
  rr$points3d(rb, col = 1,type="h",pch=30)
  rdat <- cbind(Fish$MeanDepth,Fish$TotAbund, rep(0,nrow(Fish)))
  #rr$points3d(rdat, col = 1,type="p")
  }
#########################################################





##########################################################
#Model validation
E1 <- resid(M1, type = "pearson")
F1 <- fitted(M1)
eta <- predict(M1, type = "link")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)

plot(x = eta, 
     y = E1,
     xlab = "Eta",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h=0, v = 0, lty = 2)

plot(x = Fish$MeanDepth, 
     y = E1,
     xlab = "Mean Depth (km)",
     ylab = "Pearson residuals",
     cex.lab = 1.5,
     pch = 16)
abline(h=0, v = 0, lty = 2)

boxplot(E1 ~ Period, 
        ylab = "Pearson residuals",
        data = Fish,
        cex.lab = 1.5, 
        xlab = "Period")
abline(h = 0, v = 0, lty = 2)
######################################################





######################################################
#Section 1.2.6
#Checking for overdispersion
N <- nrow(Fish)
p <- length(coef(M1))
Dispersion <- sum(E1^2) / (N - p)
Dispersion


par(mar = c(5,5,2,2))
plot(cooks.distance(M1), 
     type = "h", 
     ylim = c(0,20), 
     cex.lab = 1.5)
abline(h=0, lty = 2, lwd = 2)
I <-1:nrow(Fish)
I[cooks.distance(M1) > 1]
##########################################################




##########################################################
#Figure 1.12
Mypch <- Fish$Period
Mypch[Fish$Period == 2] <- 16
xyplot(Ykm ~ Xkm, 
       aspect = "iso",
       data = Fish,
       pch = Mypch,
       col = 1,
       xlab = "X-coordinate",
       ylab = "Y-coordinate")
###################################################




####################################################
#Section 1.2.7
Fish$fPeriod <- factor(Fish$Period)
M2 <- glm(TotAbund ~ MeanDepth * fPeriod, 
          data = Fish, 
          family = poisson)#,
          #subset = -23)

summary(M2)

#Check overdispersion
E2 <- resid(M2, type = "pearson")
N <- nrow(Fish)
p <- length(coef(M2))
Dispersion <- sum(E2^2) / (N - p)
Dispersion


#Using the offset
Fish$LogSA <- log(Fish$SweptArea)
M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), 
          data = Fish, 
          family = poisson)

summary(M3)

E3 <- resid(M3, type = "pearson")
N <- nrow(Fish)
p <- length(coef(M3))
Dispersion <- sum(E3^2) / (N - p)
Dispersion
####################################################







########################################################
#Section 1.3
dnbinom(0,mu=3,size=1)


#Figure 8.3 
library(stats)

mu1B=3 ; k1B=0.1
mu1C=3 ; k1C=1
mu1D=3 ; k1D=1000

mu2B=15 ; k2B=0.1
mu2C=15 ; k2C=1
mu2D=15 ; k2D=1000

mu3B=50 ; k3B=0.1
mu3C=50 ; k3C=1 
mu3D=50 ; k3D=1000

x1B<-0:10; Y12<-dnbinom(x1B,mu=mu1B,size=k1B)
x1C<-0:10; Y13<-dnbinom(x1C,mu=mu1C,size=k1C)
x1D<-0:10; Y14<-dnbinom(x1D,mu=mu1D,size=k1D)

x2B<-0:25; Y22<-dnbinom(x2B,mu=mu2B,size=k2B)
x2C<-0:25; Y23<-dnbinom(x2C,mu=mu2C,size=k2C)
x2D<-0:25; Y24<-dnbinom(x2D,mu=mu2D,size=k2D)

x3B<-0:100; Y32<-dnbinom(x3B,mu=mu3B,size=k3B)
x3C<-0:100; Y33<-dnbinom(x3C,mu=mu3C,size=k3C)
x3D<-0:100; Y34<-dnbinom(x3D,mu=mu3D,size=k3D)

par(mfrow = c(3,3), cex.lab = 1.5, cex.main = 1.5, mar = c(5,5,2,2))
Xlab = "Y values"
Ylab = "Probabilities"
plot(x1B,Y12,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu1B,", k =",k1B,")"))
plot(x1C,Y13,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu1C,", k =",k1C,")"))
plot(x1D,Y14,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu1D,", k =",k1D,")"))

plot(x2B,Y22,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu2B,", k =",k2B,")"))
plot(x2C,Y23,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu2C,", k =",k2C,")"))
plot(x2D,Y24,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu2D,", k =",k2D,")"))

plot(x3B,Y32,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu3B,", k =",k3B,")"))
plot(x3C,Y33,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu3C,", k =",k3C,")"))
plot(x3D,Y34,type="h",xlab=Xlab,ylab=Ylab,main=paste("NB(mu =",mu3D,", k =",k3D,")"))
###############################################





########################################################
#Section 1.3.2
Fish$LogSA <- log(Fish$SweptArea)
M4 <- glm.nb(TotAbund ~ MeanDepth * fPeriod + offset(LogSA), 
          data = Fish)

summary(M4)

E4 <- resid(M4, type = "pearson")
N <- nrow(Fish)
p <- length(coef(M4)) + 1  #"+1' is due to k
Dispersion <- sum(E4^2) / (N - p)
Dispersion


drop1(M4, test = "Chi")
M5 <- glm.nb(TotAbund ~ MeanDepth + fPeriod + offset(LogSA), 
          data = Fish)

drop1(M5, test = "Chi")
summary(M5)


#Figure 1.14
par(mfrow = c(2, 2), mar = c(5,5,2,2))
E5 <- resid(M5, type = "pearson")
F5 <- fitted(M5, type = "response")
plot(x = F5, 
     y = E5,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0, 0, lty = 2)

plot(cooks.distance(M5),
     type = "h",
     ylim=c(0,1),cex.lab = 1.5)
abline(h=1)

#Normality
#hist(E5, breaks = 15,cex.lab = 1.5,
#     ylab ="",
#     xlab ="Pearson residuals")


#Independence:
plot(x = Fish3$MeanDepth, 
     y = E5,
     xlab = "Mean depth (km)",
     ylab = "Pearson residuals", cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(E5 ~ Period, 
        data = Fish3, 
        xlab = "Period",
        ylab = "Pearson residuals",
        cex.lab = 1.5)


#Figure 1.15
xyplot(E5 ~ MeanDepth | factor(Period), 
       data = Fish3,
       xlab = list(label = "Mean depth (km)", cex = 1.5),
       ylab = list(label = "Pearson residuals", cex = 1.5),
       panel = function(x,y){
         panel.points(x,y, col = 1, pch = 16, cex = 0.7)
         panel.loess(x,y, col = 1, lwd = 2)
         panel.abline(h=0)
       })

 
######################################################
#Or visualize the lines with points
M5 <- glm.nb(TotAbund ~ MeanDepth + fPeriod + offset(LogSA), 
          data = Fish))


MyData1 <- data.frame(MeanDepth=seq(from= 0.804,
                                    to=4.865,
                                    length= 25),
                      fPeriod = "1",
                      LogSA = mean(log(Fish$SweptArea)))

MyData2 <- data.frame(MeanDepth=seq(from= 0.804,
                                    to=4.865,
                                    length=25),
                      fPeriod = "2",
                      LogSA = mean(log(Fish$SweptArea)))

P1 <- predict(M5, newdata = MyData1, type="response")
P2 <- predict(M5, newdata = MyData2, type="response")

NicePch <- Fish$Period
NicePch[Fish$Period==1] <- 1
NicePch[Fish$Period==2] <- 16


par(mar = c(5,5,2,2))#, mfrow=c(1,2))
plot(x = Fish$MeanDepth, y = Fish$TotAbund,
     pch=NicePch, cex.lab = 1.5, xlab = "Mean depth (km)",
     ylab = "Total abundance")
#With CI
P1 <- predict(M5, newdata = MyData1, type="link", se = TRUE)
P2 <- predict(M5, newdata = MyData2, type="link", se = TRUE)
 
 
 
polygon(c(MyData2$MeanDepth, rev(MyData2$MeanDepth)),
        c(exp(P1$fit-2*P1$se), rev(exp(P1$fit+2*P1$se))),
        col =gray(0.5), border=NULL,
        density =50   )
        
polygon(c(MyData2$MeanDepth, rev(MyData2$MeanDepth)),
        c(exp(P2$fit-2*P2$se), rev(exp(P2$fit+2*P2$se))),
        col =1, border=NULL,
        density =50   )
############################################################






############################################################
#Section 1.3.3
library(msme)nb2 <- nbinomial(TotAbund ~ MeanDepth + fPeriod + offset(LogSA),
                 data = Fish)
                 
nbH <- nbinomial(TotAbund ~ MeanDepth + fPeriod,
                 formula2 = ~ MeanDepth + fPeriod,
                 offset = Fish$LogSA,
                 family = "negBinomial"
                 mean.link = "log",
                 scale.link = "log_s",
                 data = Fish)
summary.msme(nbH)
###########################################################






#Section 1.4
#####################################################################
#Set the working directory and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/bees")
Bees <- read.table(file = 'workerbees.txt', 
                   header = TRUE)
names(Bees)



##################################################################
#House keeping
#Convert the data to absence/presence data
Bees$Parasites[Bees$Parasites>0] <-1
##################################################################



##################################################################
table(Bees$Parasites)

#Plot the data and add a linear regression line
#Figure 1.17
plot(x = Bees$CellSize,
     y = Bees$Parasites,
     xlab = "Cell size",
     ylab = "Absence or presence of parasites")

M0 <- lm(Parasites ~ CellSize, 
         data = Bees)
abline(M0, lwd = 3)


#Boxplot of cell size conditional on absence/presence of parasites
boxplot(CellSize ~ Parasites,
        data = Bees)
##################################################################






##################################################################
M1 <- glm(Parasites ~ CellSize, data = Bees, family = binomial)
summary(M1)


#Figure 1.18
plot(x=Bees$CellSize, y = Bees$Parasites)
range(Bees$CellSize)
MyData <- data.frame(CellSize = seq(0.35,0.66, length = 50) )
MyData$P <- predict(M1, newdata=MyData, type = "response")
lines(MyData$CellSize, MyData$P)


#For Figure 1.19: just change the link in the family argument,
#and rerun



#############################################################
M2 <- glm(Parasites ~ CellSize, data = Bees, family = quasibinomial)
summary(M2)
#See Hilbe (2009)
#############################################################






#########################################################
#Section 1.5 Binomial GLM


#Figure 1.20
Xlab="Number of dead mites"
Ylab="Probabilities"

n11<-4; x11<-1:n11; p11<-0.2
n12<-4; x12<-1:n12; p12<-0.5
n13<-4; x13<-1:n13; p13<-0.7

n21<-10; x21<-1:n21; p21<-0.2
n22<-10; x22<-1:n22; p22<-0.5
n23<-10; x23<-1:n23; p23<-0.7

n31<-50; x31<-1:n31; p31<-0.2
n32<-50; x32<-1:n32; p32<-0.5
n33<-50; x33<-1:n33; p33<-0.7

prop11<-dbinom(x11, size=n11, prob=p11)
prop12<-dbinom(x12, size=n12, prob=p12)
prop13<-dbinom(x13, size=n13, prob=p13)

prop21<-dbinom(x21, size=n21, prob=p21)
prop22<-dbinom(x22, size=n22, prob=p22)
prop23<-dbinom(x23, size=n23, prob=p23)


prop31<-dbinom(x31, size=n31, prob=p31)
prop32<-dbinom(x32, size=n32, prob=p32)
prop33<-dbinom(x33, size=n33, prob=p33)


par(mfrow=c(3,3), mar = c(5,5,2,2), cex.lab = 1.5, cex.main = 1.5)
plot(x21,prop21,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p21,",",n21,")"))
plot(x22,prop22,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p22,",",n22,")"))
plot(x23,prop23,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p23,",",n23,")"))

plot(x11,prop11,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p11,",",n11,")"))
plot(x12,prop12,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p12,",",n12,")"))
plot(x13,prop13,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p13,",",n13,")"))

plot(x31,prop31,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p31,",",n31,")"))
plot(x32,prop32,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p32,",",n32,")"))
plot(x33,prop33,type="h",xlab=Xlab,ylab=Ylab,main=paste("B(",p33,",",n33,")"))


#####################################################
#Load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/DrugMites")
Mites <- read.table(file = "DrugsMites.txt", 
                    header = TRUE, 
                    dec=".")

names(Mites)
str(Mites)
####################################################




####################################################
#House keeping
Mites$Pesticides <- factor(Mites$Acaricide)
####################################################


Mites$Neg <- Mites$Total - Mites$Dead_mites


M1 <-glm(cbind(Dead_mites, Neg )~
          Concentration * Pesticides,
          family = binomial, data = Mites)
summary(M1)

#or
Mites$PosProp <- Mites$Dead_mites / Mites$Total
M2 <- glm(PosProp ~ Concentration * Pesticides,
          family = binomial,
          weights = Total,
          data = Mites)

#Check for overdispersion
E1 <- resid(M1, type = "pearson")
sum(E1^2) / (M1$df.res)
## 1.25.... not much


#Look at the numerical output
summary(M1)
drop1(M1, test = "Chi")



############################################################
#Sketch model fit
MyData1 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Pesticides = "1")
MyData2 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Pesticides = "2")
MyData3 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Pesticides = "3")
MyData4 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Pesticides = "4")

P1 <- predict(M1, newdata=MyData1, type="response")
P2 <- predict(M1, newdata=MyData2, type="response")
P3 <- predict(M1, newdata=MyData3, type="response")
P4 <- predict(M1, newdata=MyData4, type="response")

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Mites$Concentration, 
     y = Mites$PosProp, pch=16,
     ylab = "Probability of dead mites",
     xlab = expression(paste(Concentration~~(mg.l^{-1}))))
      
lines(MyData1$Concentration, P1, col=1, lty=1)
lines(MyData2$Concentration, P2, col=1, lty=3)
lines(MyData3$Concentration, P3, col=1, lty=5)
lines(MyData4$Concentration, P4, col=1, lty=7)

legend("bottomright", legend=c("1","2","3","4"),
       lty=c(1,3,5,7), col=c(1,1,1,1), cex = 1)

