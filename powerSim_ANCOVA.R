# Paket, was fuer den ANOVA-Table gebraucht wird:
install.packages("car")


# Hilfsfunktion fuer die Abbildung:
rectText <- function(xl, yb, xr, yt, text, rectArgs = NULL, textArgs = NULL) {
    center <- c(mean(c(xl, xr)), mean(c(yb, yt)))
    do.call('rect', c(list(xleft = xl, ybottom = yb, xright = xr, ytop = yt), rectArgs))
    do.call('text', c(list(x = center[1], y = center[2], labels = text), textArgs))
}

# Simulation eines Datensatzes und rausziehen des p-Wertes:
simFunc <- function(n, probs = probs, sigma = sigma, a = a, b = b, c = c) {
    n <- n
    probs <- probs
    sigma <- sigma
    betas <- c(a, b, c)
    gruppe <- sample(c(0,1), size = n, replace = TRUE, prob = probs)
    if (all(gruppe == 0)) gruppe[sample(n, size = 1)] <- 1
    if (all(gruppe == 1)) gruppe[sample(n, size = 1)] <- 0
    gruppe <- factor(gruppe, levels = c(0,1), labels = c("Control", "Deprexis"))
    BDI <- runif(n, 0, 63) # Alle werte in der Range des BDI sind moeglich
    dat1 <- data.frame(BDI = BDI, gruppe = gruppe)
    modFrame <- model.matrix(~ BDI + gruppe, data = dat1)
    y <- modFrame %*% betas + rnorm(n, mean = 0, sd = sigma)
    dat <- data.frame(y = y, BDI = BDI, gruppe = gruppe)
    # contrasts(dat$gruppe) <- contr.sum(n = 2)
    # colnames(contrasts(dat$gruppe)) <- "Deprexis"
    # summary(lm(y ~ BDI + gruppe, data = dat))
    pValue <- car::Anova(lm(y ~ BDI + gruppe, data = dat))[2,4]
    pValue
}
# Simulation vektorisieren:
vecRep <- function(n, groupSize, probs, sigma, a, b, c, ANCOVA = TRUE, ...) replicate(n = n, simFunc(n = groupSize,
                                                                                                     probs = probs,
                                                                                                     sigma = sigma,
                                                                                                     a = a, b = b, c = c))


### Ab hier Parameter fuer die Simulation angeben:

# Fuer welche Gruppengroessen soll simuliert werden (von 20 bis 200 in Vierer-Schritten):
groupSize <- seq(20, 200, 4)

# Wahrscheinlichkeit der Gruppenzugehoerigkeit (muss zusammen 1 ergeben), hier 50/50:
probs <- c(0.5, 0.5)

# Standardabweichung des BDI zur Baseline:
sigma <- 10

# Mittelwert des BDI zur Baseline:
a <- 20

# Slope des BDI (unabhaengig von der Gruppe)
b <- 1

# BDI-Gruppenunterschied zwischen Kontrolle und Treatment (hier -6 heisst 6 Punkte weniger in der Treatment-Gruppe):
c <- -6

# Anzahl Replikationen pro Gruppengroesse:
rep <- 1000


### Eigentliche Simulation:

# ANCOVA:
out <- matrix(NA, ncol = length(groupSize), nrow = rep)
out <- sapply(groupSize, FUN = vecRep, probs = probs, n = rep, sigma = sigma, a = a, b = b, c = c)
colnames(out) <- as.character(groupSize)


### Auswertung:

# Power: Wieviele p-Werte von den Replikationen sind pro Stichprobengroesse im Mittel unter 0.05?
power <- apply(out, 2, function(x) mean(x <= 0.05))

# Hilfslinien: Wann hat man das erste Mal bei welcher Stichprobengroesse die 0.8 an Power ueberschritten?
pLine <- power[which(power > 0.8)][1]
sLine <- as.numeric(names(power[which(power > 0.8)][1]))


### Abbildung:

# png(filename = "powerSim.png", units = "in", width = 11, height = 8, type = "cairo-png", res = 600, bg = "transparent")
par(mar = c(5.5, 5, 5, 0) + 0.1,
    mgp = c(3,1,0))
plot(groupSize, power, type = "n", ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
title(paste0("Power-simulation for finding a significant overall group effect for the BDI\nwith BDI baseline = ", a, ", SD = ", sigma,
             " and a post group difference = ", c, "\nwith a normal ANCOVA"),
      ylab = expression(Power*phantom(x)*(1-beta)),
      xlab = paste("Overall sample size, probability for group dependencies", probs[1], "and", probs[2]), font.lab = 2, cex.lab = 1.25, cex.main = 1.5)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), font = 2, cex.axis = 1.2)
axis(1, at = groupSize, labels = FALSE)
text(groupSize, par("usr")[3] - 0.04,
     srt = 45, adj = 1,
     labels = groupSize, xpd = TRUE, font = 2)
abline(h = seq(0,1,0.1), col = "lightgray", lty = "dotted")
abline(v = groupSize, col = "lightgray", lty = "dotted")
abline(h = pLine, col = "red", lwd = 2)
abline(v = sLine, col = "red", lwd = 2)
lines(groupSize, power, lwd = 4)
rectText(120, 0.1, 170, 0.3, paste("Based on", rep, "replications\nfor each sample size."),
         rectArgs = list(col = "white"),
         textArgs = list(cex = 1.25, adj = c(0.5, 0.5)))
# dev.off()




mus <- c(10,15,20,25)
sds <- c(2,3,4,5)

# Data generation
n.groups <- 3
n.sample <- 10	
(n <- n.groups*n.sample) # Total number of data points

# Generate the factor
rep(n.sample, n.groups)
(f1 <- rep(1:n.groups, rep(n.sample, n.groups))) # Indicator for groups
(groups <- factor(f1, labels = c("A", "B", "C")))

# Generate the continuous covariate
x <- runif(n, 45, 70)
round(x, 2)


# Generate the linear predictor (i.e., the deterministic part of the model)
Xmat <- model.matrix(~groups*x)
print(Xmat, dig=2) 
beta.vec <- c(-250, 150, 200, 6, -3, -4)

lin.pred <- Xmat %*% beta.vec # lin.predictor
data.frame(f1=f1, x=round(x, 2), E.y=round(lin.pred, 2))

# Add the stochastic part
sigma <- 10
normal.error <- rnorm(n=n, mean=0, sd=sigma) # residuals 
y <- lin.pred+normal.error # response=lin.pred+residual

data.frame(f1=f1, x=round(x, 2), E.y=round(lin.pred, 2), y=round(y, 2))





