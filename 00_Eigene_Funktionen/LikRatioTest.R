LikRatioTest <- function(L1, L2, df) {
    #L1 ist das vollstaendige Modell, L2 das genestete = kleinere Modell Modell
    #df Differenz der genuzten Freiheitsgrade zwischen den Modellen
    L <- 2 * (L1 - L2)
    L <- abs(as.numeric(L))
    p <- 1 - pchisq(L, df)
    round(c(L, p), digits= 4) 
}
