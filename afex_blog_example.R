
library(afex)
data("fhch2010") # load data (comes with afex) 
mean(!fhch2010$correct) # error rate
# [1] 0.01981546
fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
str(fhch2010) # structure of the data
# 'data.frame':	13222 obs. of  10 variables:
#  $ id       : Factor w/ 45 levels "N1","N12","N13",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ task     : Factor w/ 2 levels "naming","lexdec": 1 1 1 1 1 1 1 1 1 1 ...
#  $ stimulus : Factor w/ 2 levels "word","nonword": 1 1 1 2 2 1 2 2 1 2 ...
#  $ density  : Factor w/ 2 levels "low","high": 2 1 1 2 1 2 1 1 1 1 ...
#  $ frequency: Factor w/ 2 levels "low","high": 1 2 2 2 2 2 1 2 1 2 ...
#  $ length   : Factor w/ 3 levels "4","5","6": 3 3 2 2 1 1 3 2 1 3 ...
#  $ item     : Factor w/ 600 levels "abide","acts",..: 363 121 202 525 580 135 42 368 227 141 ...
#  $ rt       : num  1.091 0.876 0.71 1.21 0.843 ...
#  $ log_rt   : num  0.0871 -0.1324 -0.3425 0.1906 -0.1708 ...
#  $ correct  : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...

(a1 <- aov_car(log_rt ~ task*length*stimulus + Error(id/(length*stimulus)), fhch))
# Contrasts set to contr.sum for the following variables: task
# Anova Table (Type 3 tests)
# 
# Response: log_rt
#                 Effect          df  MSE          F   ges p.value
# 1                 task       1, 43 0.23  13.38 ***   .22   .0007
# 2               length 1.83, 78.64 0.00  18.55 ***  .008  <.0001
# 3          task:length 1.83, 78.64 0.00       1.02 .0004     .36
# 4             stimulus       1, 43 0.01 173.25 ***   .17  <.0001
# 5        task:stimulus       1, 43 0.01  87.56 ***   .10  <.0001
# 6      length:stimulus 1.70, 72.97 0.00       1.91 .0007     .16
# 7 task:length:stimulus 1.70, 72.97 0.00       1.21 .0005     .30
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1
# 
# Sphericity correction method: GG 
# Warning message:
# More than one observation per cell, aggregating the data using mean (i.e, fun_aggregate = mean)!

aov_car(log_rt ~ task + Error(id/length*stimulus), fhch)
aov_ez(id = "id", dv = "log_rt", fhch, between = "task", within = c("length", "stimulus"))
aov_4(log_rt ~ task + (length*stimulus|id), fhch)
aov_4(log_rt ~ task*length*stimulus + (length*stimulus|id), fhch)


lsmeans(a1, c("stimulus","task"))
# NOTE: Results may be misleading due to involvement in interactions
#  stimulus task        lsmean         SE    df    lower.CL    upper.CL
#  word     naming -0.34111656 0.04250050 48.46 -0.42654877 -0.25568435
#  nonword  naming -0.02687619 0.04250050 48.46 -0.11230839  0.05855602
#  word     lexdec  0.00331642 0.04224522 47.37 -0.08165241  0.08828525
#  nonword  lexdec  0.05640801 0.04224522 47.37 -0.02856083  0.14137684
# 
# Results are averaged over the levels of: length 
# Confidence level used: 0.95 

# set up conditional marginal means:
(ls1 <- lsmeans(a1, c("stimulus"), by="task"))
# task = naming:
#  stimulus      lsmean         SE    df    lower.CL    upper.CL
#  word     -0.34111656 0.04250050 48.46 -0.42654877 -0.25568435
#  nonword  -0.02687619 0.04250050 48.46 -0.11230839  0.05855602
# 
# task = lexdec:
#  stimulus      lsmean         SE    df    lower.CL    upper.CL
#  word      0.00331642 0.04224522 47.37 -0.08165241  0.08828525
#  nonword   0.05640801 0.04224522 47.37 -0.02856083  0.14137684
# 
# Results are averaged over the levels of: length 
# Confidence level used: 0.95 
update(pairs(ls1), by=NULL, adjust = "holm")
#  contrast       task      estimate         SE df t.ratio p.value
#  word - nonword naming -0.31424037 0.02080113 43 -15.107  <.0001
#  word - nonword lexdec -0.05309159 0.01860509 43  -2.854  0.0066
# 
# Results are averaged over the levels of: length 
# P value adjustment: holm method for 2 tests 

pairs(update(pairs(ls1), by=NULL))
# contrast                                        estimate         SE df t.ratio p.value
# word - nonword,naming - word - nonword,lexdec -0.2611488 0.02790764 43  -9.358  <.0001

test(pairs(update(pairs(ls1), by=NULL)), joint=TRUE)
 # df1 df2      F p.value
 #   1  43 87.565  <.0001


png("lmsip_example1.png", 10, 7, units = "cm", res = 300)
lsmip(a1, task ~ stimulus)
dev.off()


lsm1 <- summary(lsmeans(a1, c("stimulus","task")))
lsm1$lsmean <- exp(lsm1$lsmean)
require(lattice)
xyplot(lsmean ~ stimulus, lsm1, group = task, type = "b", 
       auto.key = list(space = "right"))
