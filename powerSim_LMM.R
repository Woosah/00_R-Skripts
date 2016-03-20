# define the parameters
muTher <- c(20,14,12)
muCont <- c(22,18,17)
Sigma  <- 10
rho    <- 0.5
nsubs  <- 10 # Should be an even number!
nsims  <- 1
TRT    <- c("Cont", "Ther")
Time   <- c("pre", "post", "FU")


# create 2 factors representing the 2 independent variables

dm <- rbind(data.frame(expand.grid(Time = Time, ID = paste0("ID", 1:(nsubs/2)), TRT = TRT[1])),
            data.frame(expand.grid(Time = Time, ID = paste0("ID", ((nsubs/2) + 1):nsubs)), TRT = TRT[2]))


# create k x k matrix populated with sigma
sd2 <- Sigma^2
S2 <- matrix(c(sd2, sd2*rho, sd2*rho*rho,0,0,0,
              sd2*rho, sd2, sd2*rho,0,0,0,
              sd2*rho*rho, sd2*rho, sd2,0,0,0,
              0,0,0,sd2, sd2*rho, sd2*rho*rho,
              0,0,0,sd2*rho, sd2, sd2*rho,
              0,0,0,sd2*rho*rho, sd2*rho, sd2), ncol = length(Time)*2, byrow = TRUE) 

S <- matrix(c(sd2, sd2*rho, sd2*rho*rho,
              sd2*rho, sd2, sd2*rho,
              sd2*rho*rho, sd2*rho, sd2), ncol = length(Time), byrow = TRUE) 

# stack 'nsims' individual data frames into one large data frame
df <- dm[rep(seq_len(nrow(dm)), nsims), ]

# add an index column to track the simulation run
df$simID <- sort(rep(seq_len(nsims), nrow(dm)))

# sample the observed data from a multivariate normal distribution
# using MASS::mvrnorm with the parameters mu and Sigma created earlier
# and bind to the existing df

library(MASS)
df$y <- NA
make.y1 <- expression(as.vector(t(mvrnorm(nsubs/2, muTher, S))))
df$y[df$TRT == "Ther"] <- as.vector(replicate(nsims, eval(make.y1)))
make.y2 <- expression(as.vector(t(mvrnorm(nsubs/2, muCont, S))))
df$y[df$TRT == "Cont"] <- as.vector(replicate(nsims, eval(make.y2)))

# use do(), the general purpose complement to the specialized data 
# manipulation functions available in dplyr, to run the ANOVA on
# each section of the grouped data frame created by group_by

library(dplyr)
library(car)
library(broom)

mods <- df %>% 
    group_by(simID) %>% 
    do(model = aov(y ~ X1 * X2 + Error(subject / (X1*X2)), qr=FALSE, data = .)) 

# extract p-values for each effect and store in a data frame
p <- data.frame(
    mods %>% do(as.data.frame(tidy(.$model[[3]])$p.value[1])),
    mods %>% do(as.data.frame(tidy(.$model[[4]])$p.value[1])),
    mods %>% do(as.data.frame(tidy(.$model[[5]])$p.value[1])))
colnames(p) <- c('X1','X2','Interaction')

power <- apply(as.matrix(p), 2, 
               function(x) round(mean(ifelse(x < .05, 1, 0) * 100),0))

# plot the known effects
library(ggplot2)
library(gridExtra)

means <- data.frame(cond[1:4, ], mu, SE = sigma / sqrt(nsubs))
plt1 <- ggplot(means, aes(y = mu, x = X1, fill=X2)) +
    geom_bar(position = position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin = mu-SE, ymax = mu+SE), 
                  position = position_dodge(width=0.9), size=.6, width=.3) +
    coord_cartesian(ylim=c((.7*min(mu)), 1.2*max(mu))) +
    theme_bw()

# melt the data into a ggplot friendly 'long' format
library(reshape2)
plotData <- melt(p, value.name = 'p')

# plot each of the p-value distributions on a log scale
options(scipen = 999) # 'turn off' scientific notation
plt2 <- ggplot(plotData, aes(x = p)) +
    scale_x_log10(breaks=c(1, 0.05, 0.001), 
                  labels=c(1, 0.05, 0.001)) +
    geom_histogram(colour = "darkblue", fill = "white") +
    geom_vline(xintercept = 0.05, colour='red') +
    facet_grid(variable ~ .) +
    labs(x = expression(Log[10]~P)) +
    theme(axis.text.x = element_text(color='black', size=7))

# arrange plots side by side and print
grid.arrange(plt1, plt2, nrow=1)








nPerGroup <- 100
nTime     <- 3
muTher   <- c(20, 14, 12)
muCont    <- c(21, 18, 17)
stdevs    <- c(10, 9, 8)
stdiff    <- 9
nSim      <- 1

# set up the indep var data
Subject <- factor(1:(nPerGroup*2))
Time <- factor(1:nTime, labels = c("Pre", "Post", "FU"))

theData <- expand.grid(Time, Subject)
names(theData) <- c("Time", "Subject")

tmp <- rep(c("Ther", "Cont"), each = nPerGroup * nTime)
theData$TRT <- factor(tmp)

# to set up variance-covariance matrix
ones <- rep(1, nTime)
A <- stdevs^2 %o% ones
B <- (A + t(A) + (stdiff^2)*(diag(nTime) - ones %o% ones))/2


# can run it through once to check that it works
library(MASS)
tmp1 <- mvrnorm(nPerGroup, mu = muTher, Sigma = B)
tmp2 <- mvrnorm(nPerGroup, mu = muCont, Sigma = B)
theData$Y <- c(as.vector(t(tmp1)), as.vector(t(tmp2)))
library(nlme)
summary(lme(Y ~ Time * TRT, random = ~ 1 | Subject, data = theData))
aovComp <- aov(Y ~ Time*TRT + Error(Subject/Time), theData)
summary(aovComp)

# some descriptive statistics and graphs
print(model.tables(aovComp, "means"), digits = 3) 
boxplot(Y ~ Time*TRT, data = theData)       
with(theData, interaction.plot(Time, TRT, Y))



