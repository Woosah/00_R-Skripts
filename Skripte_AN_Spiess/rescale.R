rescale <- function (x, tomin, tomax) 
{
    if (missing(x) | missing(tomin) | missing(tomax)) {
        stop(paste("Usage: rescale(x, tomin, tomax)\n", 
            "\twhere x is a numeric object and tomin and tomax\n is the range to rescale into", 
            sep = "", collapse = ""))        
    }
    if (is.numeric(x) && is.numeric(tomin) && is.numeric(tomax)) {        
        xrange <- range(x, na.rm = TRUE)
        if (xrange[1] == xrange[2]) return(x)
        mfac <- (tomax - tomin)/(xrange[2] - xrange[1])
        return(tomin + (x - xrange[1]) * mfac)
    }
    else {
        warning("Only numeric objects can be rescaled")
        return(x)
    }
}