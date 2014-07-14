* Syntax zur Umwandlung einer Datumsvariable im Format von *.
* Unipark in eine richtige Datums-Angabe nach SPSS-Format zum Rechnen: *.

* Die urspuengliche Variable, die umgewandelt werden soll, heisst *.
* hier 'datetime', die endgueltige Variable zum Rechnen 'datum1'. *.

string dateTemp1 dateTemp2 (A25).
string m1 (a2).
string d1 (a2).
string y1 (a4).
execute.

* An dieser Stelle datetime gegen die jeweils umzuwandelende Variable ersetzen. *.
compute dateTemp1 = date_T3o.
execute.
compute dateTemp2 = substr(dateTemp1, 1, 10).
execute. 

compute m1 = substr(dateTemp2, 6, 2).
compute d1 = substr(dateTemp2, 9, 2).
compute y1 = substr(dateTemp2, 1, 4).

compute mn = numeric(m1, f4.0).
compute dn = numeric(d1, f4.0).
compute yn = numeric(y1, f4.0).

* Hier datum1 gegen die jeweils richtige Zielvariable austauschen. *.
compute datum_T3o = date.dmy(dn, mn, yn).
formats datum_T3o (adate11).
execute.
delete variables d1 m1 y1 dn mn yn dateTemp1 dateTemp2.
execute.

* Nach dieser Syntax kann man zwei verschiedene Datumsvariablen einfach voneinander abziehen, *.
* um einen Zeitabstand in Sekunden zu kriegen. Also dabei fuer Tage immer noch umrechnen, *.
* damit die Formatierung des Ergebnis stimmt. *.

* Beispiel: (Funktioniert hier natuerlich nicht, da es noch kein 'datum2' gibt) *.

numeric diff_T1telefon_T3online (F6.0).
execute.
compute diff_T1telefon_T3online = (datum_T3o - datum_T1t) / (60*60*24).
execute.







