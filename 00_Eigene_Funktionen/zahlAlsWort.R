### Kleine Funktion, um in Latex-Texten die Zahlen von 1 bis 12 durch das 
### entsprechende deutsche Wort zu ersetzen.

zahlAlsWort <- function(zahl, gross = FALSE) {
    zahlen <- c("eins", "zwei", "drei", "vier", "fünf", "sechs",
                "sieben", "acht", "neun", "zehn", "elf", "zwölf")
    names(zahlen) <- c(1:12)
    if (abs(zahl) > 12) {
        message("Zahlen ueber zwoelf werden nicht als Wort ausgeschrieben...die Zahl wird unveraendert ausgegeben!")
        return(zahl)
    } else {
        wort <- unname(zahlen[names(zahlen) == abs(zahl)])
        if (gross) wort <- paste0(toupper(substring(wort, 1, 1)), substring(wort, 2))
        if (zahl < 0) wort <- paste("minus", wort, collapse = " ")
        return(wort)
    }
}
