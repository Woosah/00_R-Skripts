##### Skript zum R-Workshop
### DGPs Dresden 2015
### 13. 5. 2015

### Christoph Breuninger
### Uni Freiburg
### christoph.breuninger@psychologie.uni-freiburg.de

### Dieses Skript und Beispiel-Dateien: 
# http://j.mp/Dresden15Rcb

# Buchempfehlung: Luhmann, M. (2010). R für Einsteiger. Einführung in die Statistiksoftware für die Sozialwissenschaften. Weinheim: Beltz.

##### Umgang mit R und Rstudio

### RStudio-Fenster: RStudio ist eine Oberfläche für R.
# Das R im engsten Sinn ist die Konsole, wo Befehle ausgeführt und Ergebnisse angezeigt werden
# Besser und üblich ist aber die Arbeit mit "Scripts" - ähnlich SPSS Syntax Files
# Im gleichen Fenster können auch größere Datenmengen angezeigt werden
# Ein weiteres Fenster zeigt alle Objekte im "Environment" (sehr nützlich, später mehr)
# Das letzte zeigt v. a. Grafiken und Hilfe

### Zitieren: Beim Start von R zeigt die Konsole Infos zur Version, Zitierbarkeit und wie Hilfe und Demos abgerufen werden können
citation()
citation("lme4")

### Hilfe
help("mean")
help("+")
help(":")



##### Ziele heute
# Grundprinzipien, Funktionsweise, Oberfläche RStudio verstehen
# in einige Anwendungen hineinschnuppern, dabei wichtige Funktionen kennen lernen
# Anknüpfungspunkte für eigenes Nachlesen, Recherchieren, Lernen und Vertiefen
# Auf Eure Vertiefungswünsche eingehen


##### Wozu R?

# Grafiken
# ungewöhnliche Berechnungen
# große Datenmengen (z. B. Physio)
# wiederkehrende Aufgaben
# ...

### Was mache ich damit?
# EMA- und Physio-Daten aufbereiten
# komplexe Grafiken (z. B. Physio-Verläufe bei Agoraphobie-Expos)
# Lehrevaluation
# seltener: Statistik, v. a. HLM


##### Vorteile von R
# schlank und schnell
# dokumentiert / zitierbar
# Fülle von Paketen
# modernste statistische Verfahren (z. B. hierchische lineare Modelle, multiple Imputation, ...)


##### Nachteile von R
# Welche Funktion / welches Paket wofür? => rseek.org
# Wie sehen meine Daten aus? => RStudio, Export nach csv/Excel


##### Grundprinzipien
### Objekte: Variablen & Funktionen => siehe Environment
# Grundprinzip von R ist die Zuweisung von Daten, eigenen Funktionen, Ergebnissen von Berechnungen, Output von Funktionen, ... zu Objekten: <- ("=" ist auch möglich, aber verwirrender)

zahl <- 1
nachricht <- "Hallo Welt!"

# Objekte können durch eingeben / ausführen des Namens abgefragt werden
zahl
nachricht
# => ein Blick auch ins Environment-Fenster!

# Funktionen immer durch Name() - mit den Klammern, meistens mit Parametern dort
# Parameter können entweder durch ihre Reihenfolge oder ihren Namen bestimmt werden
# (siehe jeweils die Hilfeseite)
citation()
help("help")
help("mean")
help.search("spss")
# Hilfe kürzer:
?"mean"
??"spss"

### Datentypen
# nach Form: vector, matrix, data.frame, list, ...

zahlen <- c(1,2,3,4,5,6) # kürzer: 1:6, vgl. seq() und c()
zahlen
mean(zahlen)

mat <- matrix(zahlen, nrow = 2, byrow = F)
# Anmerkung: funktionen wie "matrix", die mehrere Parameter brauchen, haben meistens benannte Parameter, die wie oben in der Form "Parameter = Wert" in beliebiger Reihenfolge eingegeben werden können. Und für die meisten Parameter gibt es Standardwerte bzw. bestimmen sie sich automatisch aus den anderen (wie hier ncol aus nrow)
mat
t(mat) # transponierte Matrix

daten <- data.frame(id = 100:105, av = zahlen)
daten

# nach Inhalt: numeric, logical, character, factor, ...

mix <- c(zahlen, nachricht)
mix
mix + 2
# => es gibt keine gemischten Vektoren
# R sucht den "kleinsten gemeinsamen Nenner", in dem alle Daten repräsentiert werden können
# evtl. müssen gemischte Vektoren "coerced" werden
as.numeric(mix)
as.numeric(mix) + 2
# der Typ lässt sich mit mode() bestimmen, bzw. mit is.character(), is... testen:
mode(mix)
is.character(mix)

# T, F, TRUE, FALSE als äquivalente Versionen für wahr und falsch
logi1 <- c(T, F, TRUE, FALSE)
logi1
logisch <- zahlen > 3
logisch

# character immer in Anführungszeichen " oder ', können auch jeweils ineinander vorkommen, dann ohne besondere Funktion
char1 <- c("Was gibt's?", 'Er sagte: "Stopp!"')
char1
gru.char <- c("EG", "EG", "EG", "KG", "KG", "KG") # kürzer: rep(c("EG", "KG"), each = 3)
gru.char

# Um Zahlen (oder andere Variablen) zu komplexen character-Vektoren zu verbinden, gibt es den sprintf-Befehl:
sprint <- sprintf("Gruppe: %s, Wert: %1.0f (exakt: %4.2f)", gru.char, zahlen, zahlen)
sprint

# Zum schönen Ausgeben von character-Strings (auch Schreiben in Textdateien) "cat"
cat(sprint, sep = "\n")

# Faktoren ähneln character, aber mit fest definierten Möglichkeiten
fact1 <- factor(rep(1:3, 3), levels = 1:3, labels = c("klein", "mittel", "groß"))
fact1[1] <- "riesig"
fact1

# und können aus Text automatisch generiert werden
gru.fact <- factor(gru.char)
gru.fact

daten["gru"] <- gru.fact
daten

# listen sind Ansammlungen von beliebigen Objekten, in beliebiger Verschachtelung
liste <- list(nachricht, list(zahlen, mix))
liste

# Die Namen sind immer frei wählbar (bis auf Systemnamen -> tückisch!)
# Ich empfehle: ein eigener Präfix für die Art/Form des Inhalts
# (z. B. DF für daten, V für Vectoren, F für eigene Funktionen)

Ferste.funktion <- function(Vin) {
	Vout <- Vin + Vin
	return(Vout)
} 
Ferste.funktion(zahlen)

# data.frame ist die Form für Daten, wie wir sie aus SPSS kennen
# es wird häufig eine Kombination aus numerischen Spalten und character/factor Spalten geben
# neue "variablen" im data.frame (= neue Spalten) können einfach durch Zuweisung zu einem noch nicht benutzen Namen erzeugt werden
daten["av2"] <- Ferste.funktion(zahlen)
daten

### auswählen von Teilmengen der Daten (logisch, index, mehrdimensional, mehrfach)
# geschieht durch den Namen des Objekts mit eckigen Klammern []
# entweder mit dem Index der gewünschten Elemente:
zahlen[1]
zahlen[3]
zahlen[3:5]
zahlen[c(2,4)]

# oder mit einem logischen Vektor gleicher Länge
# (häufigster und wichtigster Fall)
zahlen[zahlen > 3] # vgl. ?">" für mehr Vergleichsoperatoren

# Bei Matrizen sind entsprechend zwei Dimensionen nötig: [Zeilen, Spalten]
# leer lassen bedeutet: alle
mat
mat[1,]
mat[,2]
mat[1,3]

# ebenso data.frames, als Standard werden allerdings Spalten ausgewählt:
daten
daten[3]
# ACHTUNG: daten[3] ist immer noch ein data.frame!!!
# erst daten[[3]] oder daten[,3] ist der erwartete Vektor!
# das hat etwas mit der internen Verwandtschaft von data.frames und listen zu tun (s.u.)
daten[[3]]
daten[,3]

# eine Spalte aus einem data.frame lässt sich auch über den Namen auswählen:
# und $ ist die gängigste Abkürzung
daten["id"]
daten[["id"]]
daten$id

# häufig benutzen wir die Auswahlmöglichkeiten für Gruppen o. ä.:

daten
daten[daten$gru == "EG",] # ACHTUNG KOMMA NICHT VERGESSEN!!!
daten[daten$gru == "EG","av2"]

# weil sich logische Vektoren mit & (auch |) verknüpfen lassen, haben wir unbegrenzte Möglichkeiten für die Auswahl:
daten[daten$gru == "KG" & daten$id < 105,]

# und wir können aus einem gerade ausgewählten Objekt gleich wieder eine Auswahl treffen:

daten[["id"]][4:6]
daten[["id"]][4:6][1]

# listeninhalte werden über [[]] ausgewählt, [] liefert eine Teil-Liste
liste[[1]]
liste[1]
liste[[2]][[1]]




##### 1. Übungsblock:

# 1. erzeuge einen numerischen Vektor mit den Zahlen 1 bis 10 und einen zweiten mit dessen Quadrat
# 2. konvertiere diesen Vektor in einen character vom Format: "x Quadrat = y"
# 3. erzeuge einen Gruppenvektor (factor) mit 5 mal 4 Gruppen
# 4. Verbinde die drei Vektoren zu einem data.frame
# 5. wähle anschließend aus dem data.frame die Quadratzahlen aus von Zahlen >= 15












##### Daten einlesen
# für "ganz neue" Daten würde ich den Weg über Excel zur Eingabe wählen

### Text / CSV
# funktionen read.table / read.csv2
?read.table
### Excel
# ggf. als csv exportieren
# sonst: Pakete openxlsx oder xlsx (auch zum Schreiben nach Excel)
# SPSS: Paket foreign (auch zum Schreiben, aber Weg über csv oft besser)

# pakete einbinden:
library("foreign")
?read.spss

# Achtung: fehlende Werte bedürfen gewisser Aufmerksamkeit beim Einlesen (s.u.)

### Datensätze verbinden
# merge

##### elementare Statistik
# zur Illustration: apply wendet eine Funktion Zeilen- (1) oder Spaltenweise (2) auf Matrizen, data.frames etc. an
daten[c("av", "av2")]
apply(daten[c("av", "av2")], 1, sum)

# MW / SD
apply(daten[c("av", "av2")], 2, mean)
apply(daten[c("av", "av2")], 2, sd)
# median / IQR
apply(daten[c("av", "av2")], 2, median)
apply(daten[c("av", "av2")], 2, IQR)
?IQR
apply(daten[c("av", "av2")], 2, IQR, type = 8)

# ACHTUNG: bei allen diesen Funktionen genügt ein einzelner fehlender Wert, um das Ergebnis ungültig zu machen

### EXKURS: fehlender Werte
# sind in R als "NA" repräsentiert

daten$av3 <- daten$av
daten$av3[5] <- NA
daten$av3

# Die meisten Funktionen geben in Situationen mit NA zunächst auch NA zurück, können über Parameter aber dazu gebracht werden, die NAs zu ignorieren
# R ist hier also sehr konservativ

apply(daten[c("av", "av2", "av3")], 2, mean)
apply(daten[c("av", "av2", "av3")], 2, mean, na.rm = T)



### t.test

t.test(daten$av, daten$av2, paired = T)
# Auch hier lässt sich der Output in eine Objekt speichern und später nutzen
# das Format ist in der Hilfe jeweils im Abschnitt "Value" erklärt
?t.test
t1 <- t.test(daten$av, daten$av2, paired = T)
# z. B.: Konfidenzintervall
t1$conf.int
t2 <- t.test(av ~ gru, daten)
t2
t3 <- t.test(daten$av2[daten$gru == "EG"], daten$av2[daten$gru == "KG"])
t3
t4 <- t.test(daten$av2, daten$av3, paired = T)
t4



### p.adjust
# Korrekturen für wiederholte Vergleiche / "Bonferroni"-Korrektur
t.ps <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value)
t.ps
p.adjust(t.ps)
?p.adjust
p.adjust(t.ps, method = "bonferroni")

### cor.test
cor.test(daten$av, daten$av ^ 2)

### nichtparametrische Tests
# chisq.test, ks.test (Kolmogorov-Smirnov), shapiro.test, ...

### Regression (auch ANOVA, aber siehe unten)
# Daten mit "noise" versehen
daten$av4 <- daten$av2 + rnorm(6)
daten$av5 <- daten$av3 + runif(6, -2, 2)

# zwei Prädiktoren
LM1 <- lm(av ~ av4 + av5, data = daten)
summary(LM1)

# mit Interaktionsterm
LM2 <- lm(av ~ av4 * av5, data = daten)
# gleichwertig LM2 <- lm(av ~ av4 + av5 + av4:av5, data = daten)
# vgl. ?formula
summary(LM2)

# Vergleich der Modelle
anova(LM1, LM2)

# Diagnostik der Residuen
qqnorm(residuals(LM1))
qqline(residuals(LM1))


### ANOVA
# ist ein schwacher Punkt von R - anders konzeptualisiert als wir es von SPSS kennen, aber machbar :) 
# siehe: http://www.statmethods.net/stats/anova.html
# besser: HLM mit Paket lme4 (besonders für Messwiederholungsdesigns)

# Daten in "langes" Format bringen (Nur eine AV-Spalte, zweite Spalte zur Identifikation)
library("reshape2")
dat.lang <- melt(daten, is.vars = "id", measure.vars = c("av", "av2", "av3"))


library("lme4")
# Formeln für klassische MWH-Designs folgen dem Muster: AV ~ UVs (1 | VP-ID)
# Interaktion wäre ggf. als UV1*UV2
?formula
mwh0 <- lmer(value ~ 1 + (1 | id), data = dat.lang)
mwh1 <- lmer(value ~ variable + (1 | id), data = dat.lang)
anova(mwh0, mwh1)
summary(mwh1)



##### Grafiken erstellen
# Eine beliebte Erweiterung besonders zur Exploration ist lattice:
library("lattice")
# http://www.statmethods.net/advgraphs/trellis.html

### Histogramm
# Standard-R:
hist(dat.lang$value)

# lattice:
histogram(~value, dat.lang)
histogram(~value | variable, dat.lang)

### QQ-Plot
qqnorm(dat.lang$value)
qqline(dat.lang$value)


### Boxplot
boxplot(dat.lang$value)

bwplot(~value | variable, dat.lang)
bwplot(variable~value, dat.lang)


### Scatterplot
# mehrere abhängige Variablen im "wide" format
splom(daten[c("av", "av2", "av3")])
# nach Gruppen aufgelöst
xyplot(av ~ av2 | gru, daten)
xyplot(av ~ av2 , daten, groups = gru, auto.key = T)


### eigene Grafiken (für Veröffentlichung, etc.) Schritt für Schritt

?plot
?par

# Mittelwerte bestimmen
Vm.eg <- apply(daten[daten$gru == "EG", c("av", "av2", "av3")], 2, mean, na.rm = T)
Vm.kg <- apply(daten[daten$gru == "KG", c("av", "av2", "av3")], 2, mean, na.rm = T)

# Range der Daten bestimmen - könnte man sich natürlich auch selbst überlegen
Vrange <- c(min(c(Vm.eg, Vm.kg)), max(c(Vm.eg, Vm.kg)))
Vrange

# leeren Plot erstellen, ohne Achsen aber mit Beschriftung
plot(c(1,3), Vrange, type = "n", axes = F, xlab = "Zeit", ylab = "AV", main = "Mein erster Plot")

# Achse einfügen, x-Achse mit eigener Beschriftung
axis(1, at = 1:3, labels = c("Prä", "Post", "FU"))
axis(2)


# Linien zeichnen - mit Punkten, rot, etwas dicker
lines(1:3, Vm.eg, type = "b", col = "red", lwd = 2)
lines(1:3, Vm.kg, type = "b", col = "blue", lwd = 2)

# legende
legend("topright", legend = c("EG", "KG"), col = c("red", "blue"), lwd = 2, pch = 1)

# Standardabweichung berechnen
Vsd.eg <- apply(daten[daten$gru == "EG", c("av", "av2", "av3")], 2, sd, na.rm = T)
Vsd.kg <- apply(daten[daten$gru == "KG", c("av", "av2", "av3")], 2, sd, na.rm = T)

# Einzeichnen - als "Pfeile" mit 90°-Winkel für "Kopf"
# Paramter sind: x0, y0, x1, y1 (x und y von Startpunkt und Zielpunkt)
arrows(1:3, Vm.eg, 1:3, Vm.eg + Vsd.eg, angle = 90, length = .07)
arrows(1:3, Vm.kg, 1:3, Vm.kg + c(1,-1,1)*Vsd.kg, angle = 90, length = .07)
# kleiner Trick: ein "Kopf" zeigt nach unten

### Variante: Barplot
# Daten als Matrix
Mm <- matrix(c(Vm.eg, Vm.kg), byrow = T, nrow = 2)
# barplot zeichnen mit Anordnung nebeneinander
# dabei speichern der x-Koordinaten der Balken => vgl. Hilfe zu parplot / Value
Sbar <- barplot(Mm, beside = T, legend.text = c("EG", "KG"), main = "Gruppenvergleich über die Zeit")
# Säulen mit Gruppen beschriften - verwendet x-Koordinaten der Säulen als Vektor
mtext(side = 1, text = rep(c("EG", "KG"), times = 3), at = as.vector(Sbar), line = 0.3)
# Säulen mit MZP beschriften - verwendet Mittelwert der x-Koordinaten
mtext(side = 1, text = c("Prä", "Post", "FU"), at = apply(Sbar, 2, mean), line = 1.6)

# Standardabweichungen
arrows(Sbar[1,], Vm.eg, Sbar[1,], Vm.eg + Vsd.eg, angle = 90, length = .07)
arrows(Sbar[2,], Vm.kg, Sbar[2,], Vm.kg + c(1,-1,1)*Vsd.kg, angle = 90, length = .07)


### devices
# Zum Abspeichern stehen verschiedene Grafik-"devices" zur Verfügung:
# pdf, jpg, png, ...
# am schönsten ist pdf, weil Vektorgrafik (unendlich skalierbar)
?pdf

# Funktionsweise:
# 1) Device anlegen (=datei vorbereiten)
# pdf(file = "test.pdf", width = 7, heigth = 7)
# 2) Alle plotbefehle ausführen
# ...
# 3) device schließen (= datei speichern)
# dev.off()




##### elementare Programmierung, Schleifen und Bedingungen

### Funktionen
# können häufig benutzte eigene Berechnungen beinhalten
# oder wenn Funktionen nützlich sind (z. B. mit apply(), plot(), ...)

# Beispiel: aus ibi (inter-beat-intervall) die Herzrate bestimmen:
# Name <- function(vorgesehene, parameter = Standardwert) {geschweifte Klammern}
Fibi.hr <- function(Vibi) {
	Vhr <- 1000/Vibi*60
	return(Vhr) # bestimmt den Rückgabewert
}

# Funktionen können in der Regel nicht nur einzelne Werte, sondern auch Vektoren bearbeiten:
data.frame(ibi = seq(400,1200,100), hr = Fibi.hr(seq(400,1200,100)))

# Funktion plotten (kann plot auch, einfach X-grenzen angeben)
# vgl.: plot(sin, xlim = c(0,10), main = "Sinus - x in rad")
# plot(function(x) sin(x*pi/360), xlim = c(0,720), main = "Sinus, x in Grad")
# ?pi
plot(Fibi.hr,xlim = c(400,1200), xlab = "Inter-Beat-Intervall", ylab = "Herzrate", main = "Zusammenhang von IBI und HR")

# Ein Datenpunkt zur Illustration:
Vbsp <- 750
lines(c(Vbsp, Vbsp), c(0,Fibi.hr(Vbsp)), lty = 3)
lines(c(0, Vbsp), c(Fibi.hr(Vbsp),Fibi.hr(Vbsp)), lty = 3)
text(Vbsp, Fibi.hr(Vbsp) + 10, sprintf("%1.0f:\n%3.1f", Vbsp, Fibi.hr(Vbsp), 1))


### Schleifen mit for und Zählvariable

for(Cr in 1:nrow(daten)) {
	print(daten[Cr,])
}

# häufig gibt es effizientere (vektorbasierte) Lösungen für Aufgaben, die wir per Schleife lösen können
# aber Rechenzeit ist kein großes Problem, und for-Schleifen kommen unserem natürlichen Denken oft sehr nahe (s.u.)

### if-Bedingungen

for(Cr in 1:nrow(daten)) {
	# nur gerade ids ausgeben
	if(daten[Cr,"id"] %% 2 == 0) {
		print(daten[Cr,])
	} else {
		cat(sprintf("ausgelassen: %s\n", daten[Cr,"id"]))
	} # Ende if-else (else kann natürlich auch einfach weggelassen werden)
} # Ende for


###### Datenbeispiel / Übungen:
# Arbeitsverzeichnis setzen
getwd()
# Windows: setwd("c:/Verzeichnis/Unterverzeichnis/...")
setwd("~/Dropbox/Arbeit/Promotion/Kongresse/2015-05 DGPs Workshop Dresden/R-Workshop Dresden 15/public")

# EMA-Daten einlesen
DF <- read.csv2("15_01_19_EMA-Daten.csv", stringsAsFactors = F)

# kleiner Datenüberblick
names(DF)
head(DF)
DF$UserName
# am einfachsten: RStudios Tabellenansicht (über "Environment"-Fenster)

# Zeilen von test-Nutzern ausschließen - alle die nicht im Format 01EW
# vgl. ?regexp
Lok <- grepl("\\d{2}\\w{2}", DF$UserName)
data.frame(VP = DF$UserName, ok = Lok)
DF2 <- DF[Lok,]

# wie viele VPs?
unique(DF2$UserName)
length(unique(DF2$UserName))

# wie viele MZP je VP?
# aggregate ist sehr mächtig.
DFn <- aggregate(rep(1, nrow(DF2)), by = list(VP = DF2$UserName), FUN = sum)
DFn

# also drei "richtige" VPs
Vvps <- DFn$VP[DFn$x > 10]
Vvps

DF3 <- DF2[DF2$UserName %in% Vvps,]

# Nur id und positive panas-items auswählen
# auch subset ist sehr mächtig :)
DF3pp <- subset(DF3, subset = !is.na(DF3$panas_pos1), select = c(UserName, panas_pos1:panas_pos10))
DF3pp

# Mittelwert aller Items nach VP
DF3pp.m <- aggregate(DF3pp[-1], by = list(VP = DF3pp[[1]]), FUN = mean)
DF3pp.m


##### Selber üben und/oder Anwendungswünsche von Euch?



### Übungen:
# mit einer for-Schleife abfragen, wie viele Datensätze *mit negativer Stimmung* je VP vorhanden sind (Spalte: stimmung)

# mit einer for-Schleife den Mittelwert aller negativen Panas Items nach VP berechnen (Tipp: leeres data.frame für die Ausgabe anlegen)

# plot der Mittelwerte (je VP) aller negativen Panas-Items

