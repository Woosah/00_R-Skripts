plot.probabilities <- function(grid,
                               model,
                               leg,
                               draw.these = NULL,
                               main = "",
                               xlab = "",
                               legend.pos = "topleft",
                               ylim = NULL,
                               col = NULL,
                               lty = NULL) {
    
    co <- model$coefficients[1:length(model$y.levels) - 1]
    pre.mat <- pred(eta = rowSums(grid), theta = co)
    n.total <- length(pre.mat)
    n.rows <- length(pre.mat[1, ])
    n <- n.total / n.rows
    ylim <- if(is.null(ylim)) c(0, 1) else ylim
    draw.these <- if(is.null(draw.these)) 1:n else draw.these
    
    plot(model$y.levels,
         pre.mat[draw.these[1], ], lty = 1,
         type = "l", ylim = ylim, xlab = xlab, axes = FALSE,
         ylab = "Probability", las = 1, main = main)
    axis(1)
    axis(2)
    i <- 1
    for(k in draw.these) {
        draw_color <- if(is.null(col)) "black"
        else col[i]
        curr_lty <- if(is.null(lty)) i
        else lty[i]
        lines(model$y.levels, pre.mat[k, ], lty = curr_lty, col = draw_color)
        i <- i + 1 
    }
    if(is.null(lty)) { 
        legend(legend.pos, leg, lty = 1:i, bty = "n")
    }
    else {
        legend(legend.pos, leg, lty = lty, bty = "n")
    }
}