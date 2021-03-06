# This is the code for the animated video on the blog
# http://www.mikemeredith.net/blog/1309_Normal_pdf_animation.htm

# Updated 16 Sept 2013

# See Kruschke 2011 "Doing Bayesian Data Analysis" p32 for the basic
#  idea of using spinners to explain
#  probability density for continuous variables.

# I used ImageMagick (www.imagemagick.org) to combine the individual images
#   into a .mp4 video file.

library(MASS)  # We use the equiscale plot function (eqscplot)

# Function to generate data to draw a circle 
# (Pass the output to lines or plot(..., type='l') to do the plot.)
circle <- function(centre=c(0,0), radius=1, npt=500) {
  circ <- seq(0, 2*pi, length=npt)
  x <- sin(circ) * radius + centre[1]
  y <- cos(circ) * radius + centre[2]
  cbind(x=x, y=y)
}


# Set up main parameters for the animation
# ----------------------------------------
# Values to label on the spinner and final plot:
labs <- c(600, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1400)
# Starting locations as distance along the circumference
tickStart <- (pnorm(labs - 1000, 0, 145) - 0.5) * 2*pi
# Final locations will be uniform scale from -pi to pi:
tickEnd <- (labs - 1000) / 400 * pi

# Values used draw the curves
nGrid <- 201  # Number of points to use
grid <- seq(-400, 400, length=nGrid)  # Values, centered on 1000g
gridStart <- (pnorm(grid, 0, 145) - 0.5) * 2*pi # Starting locations on the circle
gridEnd <- seq(-pi, pi, length=nGrid) # Final locations on the x-axis

# Colours to use: make the 1150-1200 range red
isRed <- gridStart > tickStart[2] & gridStart < tickStart[3]
segCol <- ifelse(isRed, 'red', 'pink')
# The red/pink combination works for colorblinds too: they see it
#   as light/dark.

area <- diff(gridStart) / 2 # Area of initial triangles (height = 1)

dir.create("gfx")  # sub-directory to store individual plot files
setwd("gfx")

# ##############################################################
# Every plot between bmp(...) and dev.off() creates a .BMP file!
# You can play with the individual plots if you like, but once you
#   do bmp(...) run ALL the code down to dev.off(), ie, everything
#   between the ####### lines.
bmp(file = "norm%03d.bmp", width=700, height=350)
par(mar=c(0,2,0,2))

  # Initial plot with labels
  # ------------------------
  diam <- 1   # Diameter of the circle
  gridNow <- gridStart
  # Calculate coordinates of points around the circle
  x1 <- sin(gridNow + pi) * diam
  y1 <- cos(gridNow + pi) * diam + diam
  polyx <- c(x1, x1[1])
  polyy <- c(y1, y1[1])
  # Set up axes, display nothing:
  eqscplot(circle(0:1), type='l', bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
    xlim=range(gridEnd), col='white' )
  # Draw the pink polygon; make the 1150 - 1200 sector red
  polygon(polyx, polyy, col='pink', border=NA)
  redx <- c(0, x1[isRed], 0)
  redy <- c(diam, y1[isRed], diam)
  polygon(redx, redy, col='red', border=NA)
  lines(x1, y1) # Add a black line around the circle
  # Add ticks (actually blobs), white radial lines, and labels
  xt <- sin(tickStart + pi) * diam
  yt <- cos(tickStart + pi) * diam + diam
  segments(xt, yt, 0, diam, col='white', lwd=2)
  points(xt, yt, pch=19)
  text(xt[1], yt[1], " 600  1400", pos=3, xpd=TRUE)
  pos <- c(4,4,4,4, 1, 2,2,2,2)
  text(xt[2:10], yt[2:10], labs[10:2], pos=pos, xpd=TRUE)
  # Add the spinner arrow
  points(0,diam, cex=3, pch=20)
  arr <- c(4, 4+pi)
  diam.arr <- 0.8
  xta <- sin(arr) * diam.arr
  yta <- cos(arr) * diam.arr + diam
  arrows(xta[1], yta[1],xta[2], yta[2], lwd=3)
  # 
  title(xlab="Weight of squirrel (g)", line=-3.5)
  title(sub="Probability that spinner lands between 1150 and 1200 = area of red segment", line=-2)


  # Sequence of plots where (1) arc gets flatter (diameter increases) and
  #   (2) scale morphs into a regular scale
  # ----------------------------------------------------------------------
  # Set up diameters to use and increments to move grid points and ticks
  diams <- c(1, 1.01, 1.02, 1.05, (11:20)/10, 2.5, 3:10, 20, 30, 40, 50, 100, 1000, 1e4)
  nIncrem <- length(diams)
  incGrid <- (gridEnd - gridStart) / nIncrem
  incTick <- (tickEnd - tickStart) / nIncrem

  # Draw a plot for each diameter
  for(i in seq_along(diams)) {
    diam <- diams[i]
    # Calculate locations of the grid points along the arc
    gridNow <- gridStart + incGrid * i
    x2 <- sin(gridNow/diam + pi) * diam
    y2 <- cos(gridNow/diam + pi) * diam + diam
    # Calculate the top corners of the shaded area to give correct area
    triang2 <- diff(gridNow) * diam / 2 
      # Area of big triangle with apex at centre of arc
    ratio <- area / triang2
    xtl <- x2[-nGrid] * (1 - ratio) # x-coord of top-left
    xtr <- x2[-1] * (1 - ratio)
    ytl <- y2[-nGrid] + ratio * (diam - y2[-nGrid])
    ytr <- y2[-1] + ratio * (diam - y2[-1])
    # Set up axes, display nothing:
    eqscplot(circle(0:1), type='l', bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
      xlim=range(gridEnd), col='white' )
    # Plot a little quadrilateral between adjacent grid points, 
    #   red if between 1150 and 1200
    for(j in 1:(nGrid - 1)) {
      polyx <- c(x2[j], x2[j+1], xtr[j], xtl[j], x2[j])
      polyy <- c(y2[j], y2[j+1], ytr[j], ytl[j], y2[j])
      polygon(polyx, polyy, col=segCol[j], border=segCol[j])
    }
    # Draw the arc
    lines(x2, y2)
    # Add ticks and white lines
    tickNow <- tickStart + incTick * i
    xt2 <- sin(tickNow/diam + pi) * diam
    yt2 <- cos(tickNow/diam + pi) * diam + diam
    segments(xt2, yt2, 0, diam, col='white', lwd=2)
    points(xt2, yt2, pch=19)
    # Put a dashed line at the bottom, ie, where we're headed
    abline(h=0, lty=2)
  }

  # Final plot with smoother curve
  # ------------------------------
  # Calculate coordinates for the normal curve line
  sd <- 145 / 400 * pi
  xf <- seq(-4, 4, length=501)
  yf <- dnorm(xf, 0, sd) * 2*pi
  # Set up axes, display nothing:
  eqscplot(circle(0:1), type='l', bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
    xlim=range(gridEnd), col='white' )
  # Draw main pink polygon; add red polygon
  polygon(c(-4, xf, 4), c(0, yf, 0), col='pink', border='pink')
  lastRed <- xf > tickEnd[9] & xf < tickEnd[10]
  redx <- c(tickEnd[9], xf[lastRed], tickEnd[10])
  redy <- c(0, yf[lastRed], 0)
  polygon(redx, redy, col='red', border='red')
  # Add white lines, main curve, base line, ticks
  segments(xt2, yt2, 0, diam, col='white', lwd=2)
  lines(xf, yf)
  points(xt2, yt2, pch=19)
  abline(h=0)
  text(tickEnd, rep(0, length(tickEnd)), labs, pos=1, xpd=TRUE)
  # Add text
  title(xlab="Weight of squirrel (g)", line=-3.1)
  title(ylab="Probability density", line=0)
  arrows(-pi, 0.2, -pi, 2, length=0.1)
  title(sub="Probability that squirrel weight is between 1150 and 1200 = dark red area", line=-2)

dev.off()
# ######################################################

# Look in the gfx folder and you should see 32 .bmp files labelled 
#   "norm001.bmp" thro "norm032.bmp".

# Combine the individual graphics files into a .mp4 file
# ======================================================

# If you have ImageMagick installed and on the path, this
#   command should work:
system("convert -delay 8 *.bmp normal.mp4")

# Heyho! It didn't work for me: it thinks "8" is an invalid parameter.
# So I did it directly at the command prompt.

# Hint: In Windows, to open the command prompt window in the 'gfx' folder without
#   having to climb up and down the folder tree, copy the shortcut to
#   Command Prompt into your 'gfx' folder (you can copy/paste from the
#   Start > Programs > Accessories menu). Right-click on the shortcut, select
#   "Properties", and delete everything in the "Start in:" field. Now when you
#   double-click on the Command Prompt icon it will open in the 'gfx' folder.

# Open the command prompt window and you should see something like
#   C:\Documents and Settings\blah\blah\blah\gfx>
# Important bit is the 'gfx' at the end, just before the '>'.

# Copy the line below and paste into the Command Prompt window.
# To paste, right-click in the window and select Paste.

convert -delay 8 *.bmp normal.mp4

# If you get "'convert' is not recognized..." or a gobbledegook error 
#  (maybe from another program called 'convert' on your system), go find where
#  ImageMagick is installed, and use the full path. On my system that is:

"C:\Program Files\ImageMagick-6.8.6-Q16\convert" -delay 8 *.bmp normal.mp4

# Note the quotes in the line above: you need those if there's a space in
#  the name.


