feldt <- function(alpha1, n1, k1, alpha2, n2, k2) {
         W <- ((1 - alpha2) / (1 - alpha1))
         a  <- n1 - 1
         b  <- (n1 - 1) * (k1 - 1)
         c  <- (n2 - 1) * (k2 - 1)
         d  <- n2 - 1
         GA <- ((d / (d - 2)) * (b / (b - 2)))
         GB <- ((((a + 2) * (d^2)) / ((d - 2) * (d - 4) * a)) * (((c + 2) * (b^2)) / ((b - 2) * (b - 4) * c)))
         v2 <- ((2 * GA) / (GA - 1))
         v1 <- ((2 * (GA^2)) / ((2 * GB) - (GA * GB) - (GA^2)))
         upper.F <- qf(0.95, v1, v2)
         p.value <- pf(W, v1, v2, lower.tail = FALSE)
         list(round(v1), round(v2), upper.F, W, p.value)
}

feldt(0.79, 121, 8, 0.79, 86, 8)

#samples : 33, 133, 121, 86
#pos (20): 0.89, 0.89, 0.93, 0.94
#neg (14): 0.90, 0.89, 0.90, 0.89 
#dep  (8): 0.88, 0.80, 0.79, 0.79
