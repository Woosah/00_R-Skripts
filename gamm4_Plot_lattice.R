dat <-  dget("./gamm4_Plot_lattice_example-data.txt")
# str(dat)
# summary(dat)
dat$SITE_ID <- factor(dat$SITE_ID)

library(gamm4)

m1 <- gamm4(LIFE.OE_spring ~ super.end.group + s(Q95, by = super.end.group) +
                Year + Hms_Rsctned + Hms_Poaching + X.broadleaved_woodland + 
                X.urban.suburban + X.CapWks,
            data = dat, random = ~(1|WATERBODY_ID/SITE_ID))
plot(m1$gam, pages = 1)
summary(m1$gam)

newDat <- expand.grid(super.end.group = levels(dat$super.end.group),
                      Q95 = seq(from = min(dat$Q95, na.rm = TRUE),
                                to = max(dat$Q95, na.rm = TRUE),
                                length = 200),
                      Year = 2002,
                      Hms_Rsctned = 0,
                      Hms_Poaching = 0,
                      X.broadleaved_woodland = 0,
                      X.urban.suburban = 0,
                      X.CapWks = 0,
                      WATERBODY_ID = "GB102021072830",
                      SITE_ID = "157166")

datM <- predict(m1$gam, type = "response", se.fit = TRUE, newdata = newDat)
# datM2 <- predict(m1$gam, type = "link", se.fit = TRUE, newdata = newDat)
# all.equal(datM$fit, datM2$fit)

newDat$fit <- datM$fit
newDat$upr <- datM$fit + (1.96 * datM$se.fit)
newDat$lwr <- datM$fit - (1.96 * datM$se.fit)


library(ggplot2)

ggplot(newDat, aes(x = Q95, y = fit, group = super.end.group)) + theme_bw() +
    geom_rug(data = dat, aes(x = Q95, y = 0.85), sides = "b") +
    ylim(0.85, NA) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), col = NA, fill = "grey", alpha = 0.3) +
    geom_line(size = 1.2) + 
    facet_wrap(~ super.end.group)
    

library(lattice)
library(latticeExtra)

xyplot(fit ~ Q95 | super.end.group,
       data = newDat, type = "l"),  
       prepanel = function (x, y,...) list(ylim = c(min(upr), max(lwr))), 
       panel = function(x,y, ...) {
           panel.lines(x, y)
           panel.lines(upr, lty = 2, col = "red")
           panel.lines(lwr, lty = 2, col = "red")
           #panel.loess(x,y,...)
           #panel.rug(x = x[is.na(y)], y = y[is.na(x)])
           })

y.at <- pretty(range(c(0.85, 1.1)), 10)
x.at <- pretty(newDat$Q95, 10)

xyplot(fit ~ Q95 |super.end.group, type = "l",
       xlab = "Q95", ylab = "LIFE OE Spring", 
       data = newDat, ylim = c(0.85, 1.1),
       scales = list(x = list(at = x.at),
                     y = list(at = y.at)),
       par.settings = list(strip.background=list(col="lightgrey")),
       panel = function(x, y, subscripts, ...){
           panel.abline(v = x.at,
                        h = y.at, col="lightgrey")
           panel.xyplot(newDat$Q95[subscripts], newDat$upr[subscripts],
                        type = "l", col = "black", lwd = 2, lty = 2)
           panel.xyplot(newDat$Q95[subscripts], newDat$lwr[subscripts],
                        type = "l", col = "black", lwd = 2, lty = 2)
           # panel.polygon(c(newDat$Q95[subscripts], rev(newDat$Q95[subscripts])),
           #               c(newDat$upr[subscripts], rev(newDat$lwr[subscripts])),
           #               col = "grey", border = NA, ...)
           panel.xyplot(x, y, col = "black", lwd = 2, ...)
           panel.rug(x = dat$Q95[subscripts], col = 1, end = ...)
})

