* Neue Syntax fuer das Erzeugen der Graphiken mit Process *.


* Variablen verkuerzen, da PROCESS bei mehr als 8 Buchstaben meckert!.

compute Skep_2 = Skepsis.2.
compute Skep_1 = Skepsis.1.
compute Vert_2 = Vertrauen.2.
compute Vert_1 = Vertrauen.1.
compute Nach_2 = Nachteile.2.
compute Nach_1 = Nachteile.1.
compute Vort_2 = Vorteile.2.
compute Vort_1 = Vorteile.1.
execute.


* PROCESS fuer Skepsis.

process vars = Skep_2 Gruppe PHQ_dif Skep_1
/y = Skep_2
/x = Gruppe
/m = PHQ_dif
/model = 1
/quantile = 1
/jn = 1
/plot = 1.

* Daten fuer den Einzelplot werden aus der Regressionsgleichung aus dem PROCESS-Output per Hand berechnet.
* Dabei wird aber die Variable Skepsis.1 auf den Stichprobenmittelwert gesetzt, da sie in PROCESS als
* zentrierte Variable gehandhabt wird.

USE ALL.
COMPUTE filter_$=(NOT(MISSING(Skepsis.2))).
VARIABLE LABELS filter_$ 'NOT(MISSING(Skepsis.2)) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

aggregate
 /outfile=* mode=addvariables overwrite = yes
 /Skep_1_mean = mean(Skepsis.1).


* Zusaetzlich habe ich immer die beiden Endpunkte der Signifikanzregion als Variablen 'Left' und 'Right' 
* ergaenzt...die kommen auch aus dem PROCESS-Output.
* "Moderator value(s) defining Johnson-Neyman significance region(s):
     -,2685    
     3,4315    .

compute Skep_Y = 5.9367 + (-0.4486 * PHQ_dif) + (-0.3356 * Gruppe) + (0.5547 * Skep_1_mean) + (0.2024 * Gruppe * PHQ_dif).
execute.
do if (not(missing(Skep_Y))).
   compute Left_Skep = -0.2685.
   compute Right_Skep = 3.4315.
end if.
formats Skep_Y Left_Skep Right_Skep PHQ_dif (f8.0).
execute.

*  GUIDE: text.footnote(label("Grau hinterlegter Bereich ist nicht signifikant")).

* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PHQ_dif Skep_Y Gruppe Left_Skep Right_Skep MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: PHQ_dif=col(source(s), name("PHQ_dif"))
  DATA: Skep_Y=col(source(s), name("Skep_Y"))
  DATA: Gruppe=col(source(s), name("Gruppe"), unit.category())
  DATA: Left=col(source(s), name("Left_Skep"))
  DATA: Right=col(source(s), name("Right_Skep"))
  TRANS: Bottom = eval(0)
  TRANS: Top = eval(max(Skep_Y))
  GUIDE: axis(dim(1), label("Differenz prä - post total PHQ"))
  GUIDE: axis(dim(2), label("Vorhergesagte Werte (APOI-Skepsis)"))
  GUIDE: legend(aesthetic(aesthetic.shape), label("Gruppe"))
  SCALE: linear(dim(2), min(0), dataMaximum())
  SCALE: cat(aesthetic(aesthetic.shape), map(("1", shape.solid),("2", shape.dash)))
  ELEMENT: polygon(position(link.hull((Left + Right)*(Bottom + Top))),
                   color.interior(color.grey), transparency.interior(transparency."0.6"),
                   color.exterior(color.grey), transparency.exterior(transparency."0.6"))
  ELEMENT: line(position(PHQ_dif*Skep_Y), shape(Gruppe), missing.wings())
END GPL.


* PROCESS fuer Vertrauen.

process vars = Vert_2 Gruppe PHQ_dif Vert_1
/y = Vert_2
/x = Gruppe
/m = PHQ_dif
/model = 1
/jn = 1
/quantile = 1
/plot = 1.


USE ALL.
COMPUTE filter_$=(NOT(MISSING(Vert_2))).
VARIABLE LABELS filter_$ 'NOT(MISSING(Vert_2)) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

aggregate
 /outfile=* mode=addvariables overwrite = yes
 /Vert_1_mean = mean(Vert_1).

compute Vert_Y = 6.499 + (0.3209 * PHQ_dif) + (-0.2644 * Gruppe) + (0.5794 * Vert_1_mean) + (-0.1502 * Gruppe * PHQ_dif).
execute.
do if (not(missing(Vert_Y))).
   compute Left_Vert = -6.8305.
   compute Right_Vert = 0.5612.
end if.
formats Vert_Y Left_Vert Right_Vert PHQ_dif (f8.0).
execute.

* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PHQ_dif Vert_Y Gruppe Left_Vert Right_Vert MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: PHQ_dif=col(source(s), name("PHQ_dif"))
  DATA: Vert_Y=col(source(s), name("Vert_Y"))
  DATA: Gruppe=col(source(s), name("Gruppe"), unit.category())
  DATA: Left=col(source(s), name("Left_Vert"))
  DATA: Right=col(source(s), name("Right_Vert"))
  TRANS: Bottom = eval(0)
  TRANS: Top = eval(max(Vert_Y))
  GUIDE: axis(dim(1), label("Differenz prä - post total PHQ"))
  GUIDE: axis(dim(2), label("Vorhergesagte Werte (APOI-Vertrauen)"))
  GUIDE: legend(aesthetic(aesthetic.shape), label("Gruppe"))
  SCALE: linear(dim(2), min(0), dataMaximum())
  SCALE: cat(aesthetic(aesthetic.shape), map(("1", shape.solid),("2", shape.dash)))
  ELEMENT: polygon(position(link.hull((Left + Right)*(Bottom + Top))),
                   color.interior(color.grey), transparency.interior(transparency."0.6"),
                   color.exterior(color.grey), transparency.exterior(transparency."0.6"))
  ELEMENT: line(position(PHQ_dif*Vert_Y), shape(Gruppe), missing.wings())
END GPL.




* PROCESS fuer Nachteile / Defizite.

process vars = Nach_2 Gruppe PHQ_dif Nach_1
/y = Nach_2
/x = Gruppe
/m = PHQ_dif
/model = 1
/jn = 1
/quantile = 1
/plot = 1.

USE ALL.
COMPUTE filter_$=(NOT(MISSING(Nach_2))).
VARIABLE LABELS filter_$ 'NOT(MISSING(Nach_2)) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

aggregate
 /outfile=* mode=addvariables overwrite = yes
 /Nach_1_mean = mean(Nach_1)
 /Left_Nach = min(PHQ_dif).

compute Nach_Y = 5.9139 + (-0.1664 * PHQ_dif) + (-0.0706 * Gruppe) + (0.5886 * Nach_1_mean) + (0.0781 * Gruppe * PHQ_dif).
execute.
do if (not(missing(Nach_Y))).
   compute Right_Nach = 10.2164.
end if.
formats Nach_Y Left_Nach Right_Nach (f8.0).
execute.

* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PHQ_dif Nach_Y Gruppe Left_Nach Right_Nach MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: PHQ_dif=col(source(s), name("PHQ_dif"))
  DATA: Nach_Y=col(source(s), name("Nach_Y"))
  DATA: Gruppe=col(source(s), name("Gruppe"), unit.category())
  DATA: Left=col(source(s), name("Left_Nach"))
  DATA: Right=col(source(s), name("Right_Nach"))
  TRANS: Bottom = eval(0)
  TRANS: Top = eval(max(Nach_Y))
  GUIDE: axis(dim(1), label("Differenz prä - post total PHQ"))
  GUIDE: axis(dim(2), label("Vorhergesagte Werte (APOI-Defizite)"))
  GUIDE: legend(aesthetic(aesthetic.shape), label("Gruppe"))
  SCALE: linear(dim(2), min(0), dataMaximum())
  SCALE: cat(aesthetic(aesthetic.shape), map(("1", shape.solid),("2", shape.dash)))
  ELEMENT: polygon(position(link.hull((Left + Right)*(Bottom + Top))),
                   color.interior(color.grey), transparency.interior(transparency."0.6"),
                   color.exterior(color.grey), transparency.exterior(transparency."0.6"))
  ELEMENT: line(position(PHQ_dif*Nach_Y), shape(Gruppe), missing.wings())
END GPL.

* PROCESS fuer Vorteile.

process vars = Vort_2 Gruppe PHQ_dif Vort_1
/y = Vort_2
/x = Gruppe
/m = PHQ_dif
/model = 1
/jn = 1
/quantile = 1
/plot = 1.

USE ALL.
COMPUTE filter_$=(NOT(MISSING(Vort_2))).
VARIABLE LABELS filter_$ 'NOT(MISSING(Vort_2)) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

aggregate
 /outfile=* mode=addvariables overwrite = yes
 /Vort_1_mean = mean(Vort_1).

compute Vort_Y = 5.1982 + (0.2298 * PHQ_dif) + (-0.1245 * Gruppe) + (0.5691 * Vort_1_mean) + (-0.1387 * Gruppe * PHQ_dif).
execute.
do if (not(missing(Vort_Y))).
   compute Left_Vort = -7.1246.
   compute Right_Vort = 1.7214.
end if.
formats Vort_Y Left_Vort Right_Vort PHQ_dif (f8.0).
execute.


* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PHQ_dif Vort_Y Gruppe Left_Vort Right_Vort MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: PHQ_dif=col(source(s), name("PHQ_dif"))
  DATA: Vort_Y=col(source(s), name("Vort_Y"))
  DATA: Gruppe=col(source(s), name("Gruppe"), unit.category())
  DATA: Left=col(source(s), name("Left_Vort"))
  DATA: Right=col(source(s), name("Right_Vort"))
  TRANS: Bottom = eval(0)
  TRANS: Top = eval(max(Vort_Y))
  GUIDE: axis(dim(1), label("Differenz prä - post total PHQ"))
  GUIDE: axis(dim(2), label("Vorhergesagte Werte (APOI-Vorteile)"))
  GUIDE: legend(aesthetic(aesthetic.shape), label("Gruppe"))
  SCALE: linear(dim(2), min(0), dataMaximum())
  SCALE: cat(aesthetic(aesthetic.shape), map(("1", shape.solid),("2", shape.dash)))
  ELEMENT: polygon(position(link.hull((Left + Right)*(Bottom + Top))),
                   color.interior(color.grey), transparency.interior(transparency."0.6"),
                   color.exterior(color.grey), transparency.exterior(transparency."0.6"))
  ELEMENT: line(position(PHQ_dif*Vort_Y), shape(Gruppe), missing.wings())
END GPL.

SAVE OUTFILE='D:\Graphik_H3_Subskalen_Neu.sav'
  /KEEP Gruppe PHQ_dif Skep_Y Vert_Y Nach_Y Vort_Y
        Left_Skep Left_Vert Left_Nach Left_Vort
        Right_Skep Right_Vert Right_Nach Right_Vort
  /COMPRESSED.
get file 'D:\Graphik_H3_Subskalen_Neu.sav'.
dataset name Subskalen.


VARSTOCASES
  /ID=id
  /MAKE Y FROM Skep_Y Vert_Y Nach_Y Vort_Y
  /MAKE Left FROM Left_Skep Left_Vert Left_Nach Left_Vort
  /MAKE Right FROM Right_Skep Right_Vert Right_Nach Right_Vort
  /INDEX=AV(4) 
  /KEEP=Gruppe PHQ_dif 
  /NULL=KEEP.

value labels AV 1 'APOI-SKE' 2 'APOI-VER' 3 'APOI-DEF' 4 'APOI-VOR'.
formats Y (f8.0).
execute.

DATASET ACTIVATE Subskalen.
* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PHQ_dif Y Gruppe AV Left Right MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  PAGE: begin(scale(20cm, 25cm))
  SOURCE: s=userSource(id("graphdataset"))
  DATA: PHQ_dif=col(source(s), name("PHQ_dif"))
  DATA: Y=col(source(s), name("Y"))
  DATA: Gruppe=col(source(s), name("Gruppe"), unit.category())
  DATA: AV=col(source(s), name("AV"), unit.category())
  DATA: Left=col(source(s), name("Left"))
  DATA: Right=col(source(s), name("Right"))
  TRANS: Bottom = eval(0)
  TRANS: Top = eval(20)
  GUIDE: axis(dim(1), label("PHQ-9 Differenz prä - post"))
  GUIDE: axis(dim(2), label("Vorhergesagte Werte \n zum post-Zeitpunkt"))
  GUIDE: axis(dim(4), label(""), opposite())
  GUIDE: legend(aesthetic(aesthetic.shape), label("Gruppe"))
  SCALE: linear(dim(2), min(0), max(20))
  SCALE: cat(dim(4), include("1", "2", "3", "4"))
  SCALE: cat(aesthetic(aesthetic.shape), map(("1", shape.solid),("2", shape.dash)))
  ELEMENT: polygon(position(link.hull((Left + Right)*(Bottom + Top)*1*AV)),
                   color.interior(color.grey), transparency.interior(transparency."0.6"),
                   color.exterior(color.grey), transparency.exterior(transparency."0.6"))
  ELEMENT: line(position(PHQ_dif*Y*1*AV), shape(Gruppe), missing.wings())
  PAGE: end()
END GPL.
