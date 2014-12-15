### Plotting random effects from an lme4-model!

## re = object of class ranef.mer
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE, LatexOptions = TRUE, coordflip = TRUE) {
    require(ggplot2)
    f <- function(x) {
        pv   <- attr(x, "postVar")
        cols <- 1:(dim(pv)[1])
        se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
        ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
        pDf  <- data.frame(y=unlist(x)[ord],
                           ci=1.96*se[ord],
                           nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                           ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                           ind=gl(ncol(x), nrow(x), labels=names(x)))
        
        if(QQ) {  ## normal QQ-plot
            p <- ggplot(pDf, aes(nQQ, y))
            p <- p + facet_wrap(~ ind, scales="free")
            p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
        } else {  ## caterpillar dotplot
            p <- ggplot(pDf, aes(ID, y)) 
            if (coordflip) p <- p + coord_flip()
            if (likeDotplot) {  ## imitate dotplot() -> same scales for random effects
                p <- p + facet_wrap(~ ind)
            } else {           ## different scales for random effects
                p <- p + facet_grid(ind ~ ., scales="free_y")
            }
            p <- p + xlab("Levels") + ylab("Random effects")
        }
        p <- p + theme(legend.position="none")
        p <- p + geom_hline(yintercept=0)
        if (!LatexOptions) {
            p <- p + geom_errorbar(aes(ymin=y-ci, ymax = y + ci), width = 0, colour="black")
            p <- p + geom_point(aes(size=1.2), colour="blue")
            p <- p + theme(legend.position="none")
        } else if (LatexOptions) {
            y_axis_size <- ifelse(nrow(pDf) <= 60, 10, 8)
            
            p <- p + geom_errorbar(aes(ymin=y-ci, ymax = y + ci), width = 0, colour="black", cex = 1.05)
            p <- p + geom_point(aes(size=1.2), colour="black")
            p <- p + theme_bw()
            p <- p + xlab("") + ylab(expression(paste("Varianz der ", italic("random effects"), sep = "")))
            p <- p + theme(plot.title = element_text(size = 20, face = "bold",
                                                     family = "Times", vjust = 2),
                           axis.title.y = element_text(family = "Times", size = 15,
                                                       vjust = 1.5, face = "bold"),
                           axis.title.x = element_text(family = "Times", size = 15,
                                                       face = "bold", vjust = -0.25),
                           axis.text.x = element_text(family = "Times", size = 14,
                                                      face = "bold"),
                           axis.text.y = element_text(family = "Times", size = y_axis_size),
                           strip.text = element_text(family = "Times", colour = "black",
                                                     face = "bold", size = 14),
                           plot.background = element_blank())
            p <- p + theme(legend.position="none")
        }
        if (!coordflip) p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 1),
                                       axis.text.y = element_text(face = "bold", size = 12),
                                       axis.title.y = element_text(face = "bold", size = 16))
        return(p)
    }
    
    lapply(re, f)
}
