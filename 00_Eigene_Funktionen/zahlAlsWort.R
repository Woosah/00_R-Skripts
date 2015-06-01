### Kleine Funktion, um in Latex-Texten die Zahlen von 1 bis 12 durch das 
### entsprechende deutsche Wort zu ersetzen.

zahlAlsWort <- function(zahl, gross = FALSE) {
    zahlen <- c("eins", "zwei", "drei", "vier", "fünf", "sechs",
                "sieben", "acht", "neun", "zehn", "elf", "zwölf")
    names(zahlen) <- c(1:12)
    wort <- unname(zahlen[names(zahlen) == zahl])
    if (gross) wort <- paste0(toupper(substring(wort, 1, 1)), substring(wort, 2))
    return(wort)
}
