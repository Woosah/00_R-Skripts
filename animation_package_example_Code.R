## Source John Fox circle function and load animation

source("http://dl.dropboxusercontent.com/u/61803503/wordpress/circle_fun.txt")
library(animation)

## Function to draw a stickman and a telephone (telephone is not fixed)

FUN <- function(y = 0.8) {
    opar <- par()$mar
    on.exit(par(mar = opar))
    par(mar = rep(0, 4))
    plot.new()
    circle(0.5, 0.6, 1, "cm", , 4)
    segments(0.5, 0.2, 0.5, 0.54, lwd = 4)
    segments(0.4, 0, 0.5, 0.2, lwd = 4)
    segments(0.6, 0, 0.5, 0.2, lwd = 4)
    segments(0.5, 0.4, 0.3, 0.5, lwd = 4)
    segments(0.5, 0.4, 0.7, 0.5, lwd = 4)
    points(0.5, y, pch = -9742L, cex = 4, col = "firebrick3")
}

## Try it! 

FUN()

oopt <- animation::ani.options(interval = 0.1)
 
## Now run FUN multiple times with different values for the 
## location of the phone

FUN2 <- function() {
    lapply(seq(1.01, 0.69, by = -0.02), function(i) {
        FUN(i)
        animation::ani.pause()
    })
}
 
FUN2()

## Save the files in desired format

saveGIF(FUN2(), interval = 0.1, outdir = "images/animate")
 
saveVideo(FUN2(), interval = 0.1, outdir = "images/animate", 
    ffmpeg = "C:/Program Files (x86)/ffmpeg-latest-win32-static/ffmpeg-20130306-git-28adecf-win32-static/bin/ffmpeg.exe")
 
saveLatex(FUN2(), autoplay = TRUE, loop = FALSE, latex.filename = "tester.tex",
    caption = "animated dialogue", outdir = "images/animate", ani.type = "pdf",
    ani.dev = "pdf", ani.width = 5, ani.height = 5.5, interval = 0.1)
 
saveHTML(FUN2(), autoplay = FALSE, loop = FALSE, verbose = FALSE, outdir = "images/animate/new",
    single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
