peakarea <- function(x, y)
{
  plot(x, y, type = "b")
  ID <- identify(x, y, n = 2, plot = FALSE)
  x <- x[ID[1]:ID[2]]
  y <- y[ID[1]:ID[2]]
  X <- c(head(x, 1), tail(x, 1))
  Y <- c(head(y, 1), tail(y, 1))
  LM <- lm(Y ~ X)
  y.pred <- predict(LM, newdata = data.frame(X = x))
  y <- y - y.pred

  simpson <- function(x, y) {
    n <- length(y)
    odd <- n %% 2
    if (odd) area <- 1/3 * sum(y[1] + 2 * sum(y[seq(3, (n - 2), by = 2)]) + 4 * sum(y[seq(2, (n - 1), by = 2)]) + y[n])
    else area <- 1/3 * sum(y[1] + 2 * sum(y[seq(3, (n - 3), by = 2)]) + 4 * sum(y[seq(2, (n - 2), by = 2)]) + y[n - 1]) + 1/2 * (y[n - 1] + y[n])
    dx <- x[2] - x[1]
    return(area * dx)
  }
  
  polygon(x, y + y.pred, col = 2)

  simpson(x, y)
}
