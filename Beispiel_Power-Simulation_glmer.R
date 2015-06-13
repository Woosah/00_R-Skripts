
# Beispiel Power-Simulation glmer von jan.vanhove@unifr.ch ----------------


###
# Fit model
mod <- glmer(CorrectVowel ~ LearningCondition + Category + LearningCondition:Category + (1 + Category | Subject) + (1 + LearningCondition | Item), data = dat_post_critical, family = binomial, control = glmerControl(optimizer="bobyqa"))

# Get RE estimates ('relative Cholesky factors of each RE term', so I learn. Not that that rings a bell.)
thetas <- getME(mod,"theta")

# Get FE estimates
betas <- fixef(mod)
# and change the 1.92 for the interaction to 1.27
betas[4] <- 1.9292 - 2*0.3292

# Function for simulating new data
power.simulation2 <- function(n) {
    
    # Generate new data
    dat_post_critical$New <- factor(unlist(simulate(mod, newparams = list(theta = thetas, beta = betas))))
    
    # And randomly reduce this dataset (as before)
    dat1 <- subset(dat_post_critical,
                   Subject %in% sample(unique(dat_post_critical[dat_post_critical$LearningCondition == "<ij> participants",]$Subject), size = n/2))
    dat2 <- subset(dat_post_critical,
                   Subject %in% sample(unique(dat_post_critical[dat_post_critical$LearningCondition == "<oe> participants",]$Subject), size = n/2))
    dat <- droplevels(rbind(dat1, dat2))
    
    # Run new model comparison
    mod0 <- glmer(New ~ LearningCondition + Category + (1 + Category | Subject) + (1 + LearningCondition | Item), data = dat, family = binomial, control = glmerControl(optimizer="bobyqa"))
    mod1 <- update(mod0, . ~ . + LearningCondition:Category)
    
    # Save and return p-value
    pvalue <- anova(mod0, mod1)[2, 8]
    return(pvalue)
}

# Run 100 times for 40 participants
ps.n40b <- replicate(100, power.simulation2(n = 40))

table(ps.n40b < 0.05)
# 82% TRUE
###
