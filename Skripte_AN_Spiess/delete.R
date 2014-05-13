delete <- function(x, pos, fill = FALSE) {
  xout <- x[-pos]
  if (fill) xout <- c(xout, rep(NA, length(pos)))
  return(xout)
}