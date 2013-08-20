#########################################################
## Intro to Simulation -  R/Stats Intro Series
## Designed by: Corey Chivers, 2013
## Department of Biology, McGill University
#########################################################


##@  0.1  @##
rm(list=ls())					 # Housekeeping
#setwd('<my working directory>') # set working dir
install.packages("RCurl")
library(RCurl)
###  --  ###



##@  1.1 @##
sample(letters,1)   #Draw a random letter

EQ<-c('heads','tails')  #Define the ecologist's quarter
sample(EQ,20,replace=TRUE,p=c(0.4,0.6))

###  --  ###



##@  1.2 @##
### Download Heights of workshop participants ###
url<-"http://madere.biol.mcgill.ca/cchivers/heights.csv"
web_text<-getURL(url)
heights<-read.csv(textConnection(web_text)) ##Read in the data

head(heights)
plot(heights$X)

hist(heights$X,xlab='Heights (cm)',ylab='frequency',main='Cool Kids\' heights')

mu_h<-mean(heights$X) #get the mean
sd_h<-sd(heights$X)   #and standard deviation of the heights

###  --  ###



##@  1.3 @##
### Simulate a virtual room of participants *like* this one.
sim_heights<-rnorm(30,mean=mu_h,sd=sd_h)

sim_heights #Print the simulated heights
plot(sim_heights) #Plot the values

## Look at the histogram for comparison
hist(sim_heights,xlab='Heights (cm)',ylab='frequency',main='Virtual Cool Kids\' heights')

###  --  ###





##@  1.4 @##

## Generating Random values from a variety of distributions
## R is GREAT at this.

n=1000  ## Number of samples to draw (you can change this if you want)
par(mfrow=c(1,2),pty='s') ## Graphical parameters

# Normal
x<-rnorm(n,mean=0,sd=1)
plot(x,main='random draws from \n a standard normal') ## Here are all the randomly sampled points 
hist(x,probability=TRUE)                              ## A histogram of the samples
curve(dnorm(x,mean=0,sd=1),add=TRUE)                  ## Overlay the pdf to convince ourselves that the points were 
                                                      ## infact drawn from that distribution.
# Beta
x<-rbeta(n,7,2)
plot(x,main='random draws \n from a beta distribution')
hist(x,probability=TRUE)
curve(dbeta(x,7,2),add=TRUE)

# Log-Normal
x<-rlnorm(n,0,1)
plot(x,main='random draws \n from a log-Normal distribution')
hist(x,probability=TRUE)
curve(dlnorm(x,0,1),add=TRUE)

# Exponential
x<-rexp(n,0.1)
plot(x,main='random draws \n from an exponential distribution')
hist(x,probability=TRUE)
curve(dexp(x,0.1),add=TRUE)

# Poisson
x<-rpois(n,3)
plot(x,main='random draws \n from a Poisson distribution')
hist(x,probability=TRUE,breaks=seq(-0.5,max(x)+0.5,1))
lines(0:10,dpois(0:10,3))

# Chi-squared
x<-rchisq(n,3)
plot(x,main='random draws \n from a chi-squared distribution')
hist(x,probability=TRUE)
curve(dchisq(x,3),add=TRUE)

# Binomial
x<-rbinom(n,size=10,p=0.7)
plot(x,main='random draws \n from a binomial distribution')
hist(x,probability=TRUE,breaks=seq(-0.5,10.5))
lines(1:10,dbinom(1:10,size=10,p=0.7))

## R has samplers for many more distributions: So, in general, you just use r<dist>(n,pars).

###  --  ###



##@ 2.1 @##

#@ 2.1.1 @#
##Simulate from a simple linear model.

#Model parameters
intercept<-10    #B_0
slope<-1        #B_1
error_sd<-2     #sigma

n<-30   #number of sample points

x<-runif(n,10,20) #Simulate x values
 ##--##



#@ 2.1.2 @#
#Simulate from the model
y<- intercept + slope*x #Deterministic
y<-y + rnorm(n,0,error_sd) #Stochastic

#Plot the simulated data
plot(x,y,xlab='Length (cm)',ylab='Swimming speed (cm/s)')
 ##--##

abline(intercept,slope) #Plot the true (generating) relationship
fit<-lm(y~x)        #Run a linear regression on x and y
summary(fit)        #Results of the model
abline(fit,lty=2)   #Plot the best fit line

###  --  ###



##@ 2.2 @##

##Simulate tadpole predation according to a Holling type II function response model.
##

#Model parameters
a<-0.8
h<-0.04 
n<-300
N<-sample(1:100,n,replace=TRUE) #Simulate initial tadpole densities


#Simulate from the model
predprob<- a/(1+a*h*N) #Deterministic part
killed<- rbinom(n,prob=predprob,size=N) #Stocastic part

plot(N,killed,xlab='Initial population',ylab='Number killed')

curve(a/(1+a*h*x)*x,add=TRUE) #Add the response curve (the deterministic part)
###  --  ###
