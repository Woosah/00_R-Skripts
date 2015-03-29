library(lme4)
library(mvtnorm)
library(multcomp)
library(lattice)

########################################################################
# setting up fake data, for details on sampling design see:            #
# cichini et al. (2011)                                                #
# http://www.springerlink.com/content/t4u305045t214882                 #
########################################################################

# 3 successional stages with 44 units and 1 succ. stage wit 10 units:
N = 44*3+10

cen <- per <- cbind(n = rep(NA, N), X = rep(NA, N))

# i simulate an effect between factor-levels cen and per
# all other fixed and random factors and interactions will have
# no effect.
# X = incidents, n = no. of observations:
for(i in 1:N){cen[i,"X"]<-sum(rbinom(5, 1, 0.35)); cen[i, "n"] <- 5}
for(i in 1:N){per[i,"X"]<-sum(rbinom(8, 1, 0.65)); per[i, "n"] <- 8}

dat <- data.frame(rbind(cen, per)) 

# probabilities:
dat$p <- dat$X/dat$n
stage <- as.factor(rep(c(rep("A",44), rep("B",44), rep("C",44), rep("D",10)),2))
plotA <- paste("A",rep(1:11,c(4,4,4,4,4,4,4,4,4,4,4)),sep="")
plotB <- paste("B",rep(1:13,c(3,4,2,4,4,4,4,4,3,4,4,2,2)),sep="")
plotC <- paste("C",rep(1:12,c(3,4,4,4,4,4,4,4,3,4,4,2)),sep="")
plotD <- paste("D",rep(1:3,c(2,4,4)),sep="")
plot <- as.factor(rep(c(plotA, plotB, plotC, plotD),2))
subplot <-  as.factor(rep(1:142,2))
pos <- as.factor(c(rep("Cen", N),rep("Per", N)))

df <- data.frame(cbind(dat, pos = pos, stage = stage, plot = plot, subplot = subplot))
str(df)

### visualize data:
stripplot(jitter(p,10) ~ pos | plot, data = df, groups = subplot,
          strip = strip.custom(strip.names = c(F,T)),
          ylab="p",
          layout=c(12,4),
          type = c("p", "a"),col=1,cex=0.5,
          scales = list(y = list(cex=0.5),
                        x = list(rot=90,cex=0.6,labels=c("Centers","Periphery"))),
          par.strip.text=list(cex=0.7),
          page = function(...) {
              ltext(x = 0.39,
                    y = 0.835,
                    lab = c("CENTERS vs. PERIPHERY\npanels = plots;\nrows = successional stages:\nA: pioneer, B: early, C: late, D: alpine gr.land; \nlines = paired samples (= subplots) "),
                    cex = 1,adj=0)
          })

gmod <- glmer(cbind(dat$X, dat$n - dat$X) ~ stage * pos + (1|plot/subplot), family = binomial)
print(summary(gmod), corr = F)
# comparisons -
# i want to know whether there are differences between center vs. periphery
# within each stage, and whether this effect differs across
# stages (interactions).
# as i simulated an effect only for centre vs. periphery and this
# effect was simulated
# to be the same across all stages, there should not be any sign.
# interactions:

c1 <- rbind("A: Center vs. Peri. Effect" = c(0,0,0,0,1,0,0,0),
            "B: Center vs. Peri. Effect" = c(0,0,0,0,1,1,0,0),
            "C: Center vs. Peri. Effect" = c(0,0,0,0,1,0,1,0),
            "D: Center vs. Peri. Effect" = c(0,0,0,0,1,0,0,1),
            "Center vs. Peri. Effect, A vs. B" = c(0,0,0,0,0,1,0,0),
            "Center vs. Peri. Effect, A vs. C" = c(0,0,0,0,0,0,1,0),
            "Center vs. Peri. Effect, A vs. D" = c(0,0,0,0,0,0,0,1),
            "Center vs. Peri. Effect, B vs. C" = c(0,0,0,0,0,-1,1,0),
            "Center vs. Peri. Effect, B vs. D" = c(0,0,0,0,0,-1,0,1),
            "Center vs. Peri. Effect, C vs. D" = c(0,0,0,0,0,0,-1,1))

summary(glht(gmod, c1))
###########################################################################

# EDIT:
# for investigation of contrasts you can use this functions, i.e.:
stage <- relevel(stage, "B")
model.matrix(~ stage * pos, contrasts = list(stage = "contr.treatment", pos = "contr.treatment"))