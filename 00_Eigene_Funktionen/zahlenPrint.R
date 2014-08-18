zahlenPrint <- function(p, DIG = 3, Stern = FALSE) {
	# Diese Funktion ist fuer p-Werte oder Korrelationen etc. gedacht,
	# die ohne Null vor dem Dezimalzeichen gedruckt werden muessen laut
	# APA und die entsprechend in LaTeX dargestellt werden sollen.
	# Dabei kann ausgewaehlt werden, ob zusaetzlich Signifikanz-Sterne
	# gedruckt werden oder nicht, voreingestellt ist FALSE.
	
	# Voreingestellt sind des Weiteren die Anzahl der gerundeten Stellen auf 3.
	# Zusaetzlich ist zu beachten, dass entsprechend des Zahlenwertes auch das 
	# Gleichheitszeichen bzw. das Kleiner-Als-Zeichen mit gedruckt wird!
	
	# Pruefung auf die Anzahl der Rundungsstellen, denn bei weniger als 3 kann 
	# es zu Problemen kommen!
	
	if (DIG < 3) warning("Bei weniger als 3 Nachkommastellen kann es zu Problemen kommen!")
	
	# Sicher gehen, dass es sich um eine numerische Zahl handelt und die Grenzen pruefen:
	p.num <- as.numeric(p)
	if (abs(p.num) > 1) stop("Zahl muss in den Grenzen von Null und Eins liegen!")
	
	# Funktion zum Drucken von Werten ohne Null vorm "Komma" und ohne Sterne:
	zahlenPrintOhne <- function(p.num, DIGITS = DIG) {
		if (abs(p.num) < 0.001) {
			Wert <- "< .001" 
		} else {
			Wert <- paste("= ", gsub("0\\.", ".", round(p.num, dig = DIGITS)), sep = "")
		}
		return(Wert)
	}
	
	# Funktion zum Drucken von Werten ohne Null vorm "Komma" und mit Sternen:
	zahlenPrintMit <- function(p.num, DIGITS = DIG) {
		if (abs(p.num) <= 1 & abs(p.num) > 0.1) {
			Wert <- paste("= ", gsub("0\\.", ".", round(p.num, dig = DIGITS)), sep = "")
		} else if (abs(p.num) <= 0.1 & abs(p.num) > 0.05) {
			Wert <- paste("= ", gsub("0\\.", ".", round(p.num, dig = DIGITS)), "^{+}", sep = "")
		} else if (abs(p.num) <= 0.05 & abs(p.num) > 0.01) {
			Wert <- paste("= ", gsub("0\\.", ".", round(p.num, dig = DIGITS)), "^{*}", sep = "")
		} else if (abs(p.num) <= 0.01 & abs(p.num) > 0.001) {
			Wert <- paste("= ", gsub("0\\.", ".", round(p.num, dig = DIGITS)), "^{**}", sep = "")
		} else if (abs(p.num) < 0.001) {
			Wert <- "< .001^{***}" 
		}
		return(Wert)
	}
	
	# Eigentliche Ausgabe der gesamten Funktion:
	if (Stern == FALSE) {
		Wert <- zahlenPrintOhne(p.num, DIGITS = DIG)		
	} else if (Stern == TRUE) {
		Wert <- zahlenPrintMit(p.num, DIGITS = DIG)
	}
	return(Wert)
}