*Some example corrgrams in base SPSS graphics using GPL.
*Any comments, updates, questions, extensions shoot me an email at apwheele@gmail.com

******************************************.


*making fake data.

dataset close ALL.
output close ALL.

*this uses the make data python extension.
*just generates random data for 10 variables and 30 cases, if you dont have the extension just
substitute any dataset you have handy and variables.

begin program.
import statsgeneratedata
numvar = 10
numcase = 30
parms = "0 1 "
dsname = "origcorr"
factor = "nofactor"
corrtype = "NONE"
corrs = ""
displaycorr="print"
distribution = "NORMAL"
displayinputpgm = "print"
statsgeneratedata.generate(dsname, numvar, numcase, distribution, parms, 
  factor, corrtype, corrs, displaycorr, displayinputpgm)
end program.

*I need to get the correlation matrix into a dataset to plot it.


*now to grab the correaltion matrix.
dataset declare corr_matrix.
OMS
/SELECT TABLES
/IF SUBTYPES = 'Correlations'
/DESTINATION FORMAT = SAV OUTFILE = 'corr_matrix'.


CORRELATIONS
  /VARIABLES=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

OMSEND.

*cleaning up the file.

dataset activate corr_matrix.

select if Var2 <> "Sig. (2-tailed)" and Var2 <> "N".

match files file = *
/drop Command_ Subtype_ Label_ Var2.
execute.


*now to reshape the data.

varstocases
/make Corr from V1 to V10
/INDEX Var2 (Corr).


******************************************************.
*Now to make my graphs.

*I make categories to map to colors.

if Corr < -.6 Corr_Dis = -2.
if Corr >= -.6 and Corr < -.2 Corr_Dis = -1.
if Corr >= -.2 and Corr < .2 Corr_Dis = 0.
if Corr >= .2 and Corr < .6 Corr_Dis = 1.
if Corr > .6 Corr_Dis = 2.
execute.

recode Corr (-1 thru -0.6 = -2)(-0.6 thru -0.2 = -1)(-0.2 thru 0.2 = 0)(0.2 thru 0.6 = 1)(0.6 thru 1 = 2) into Corr_Dis.
execute.

value labels Corr_Dis
-2 '-1 to -0.61'
-1 '-6 to -.21'
0 '-.2 to .19'
1 '.2 to .59'
2 '.6 to 1'
.

*Colors taken from ColorBrewer.

*Heat Map.

formats Corr (F3.2).


DATASET ACTIVATE corr_matrix.
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Var2 Var1 Corr_Dis Corr MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  PAGE: begin(scale(700px,500px))
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Var2=col(source(s), name("Var2"), unit.category())
  DATA: Var1=col(source(s), name("Var1"), unit.category())
  DATA: Corr_Dis=col(source(s), name("Corr_Dis"), unit.category())
  DATA: Corr=col(source(s), name("Corr"))
  GUIDE: axis(dim(1), label(" "))
  GUIDE: axis(dim(2), label(" "))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Correlation"))
  SCALE: cat(aesthetic(aesthetic.color.interior), map(("-2", color."0571B0"), ("-1", color."92C5DE"),("0", color."F7F7F7"),("1", color."F4A582"), ("2", color."CA0020")))
  SCALE: cat(dim(1), sort.natural(), reverse())
  SCALE: cat(dim(2), sort.natural())
  ELEMENT: polygon(position(Var2*Var1)), color.exterior(color.white), color.interior(Corr_Dis), label(Corr))
  PAGE: end()
END GPL.

compute corr_abs = ABS(corr).
execute.

GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Var2 Var1 Corr_Dis Corr corr_abs MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  PAGE: begin(scale(700px,500px))
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Var2=col(source(s), name("Var2"), unit.category())
  DATA: Var1=col(source(s), name("Var1"), unit.category())
  DATA: Corr_Dis=col(source(s), name("Corr_Dis"), unit.category())
  DATA: Corr=col(source(s), name("Corr"))
  DATA: corr_abs=col(source(s), name("corr_abs"))
  GUIDE: axis(dim(1), label(" "))
  GUIDE: axis(dim(2), label(" "))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Correlation"))
  SCALE: cat(aesthetic(aesthetic.color.interior), map(("-2", color."0571B0"), ("-1", color."92C5DE"),("0", color."F7F7F7"),("1", color."F4A582"), ("2", color."CA0020")))
  SCALE: cat(dim(1), sort.natural(), reverse())
  SCALE: cat(dim(2), sort.natural())
  SCALE: linear(aesthetic(aesthetic.size), aestheticMinimum(size."36px"), reverse())
  ELEMENT: point(position(Var2*Var1)), color.interior(Corr_Dis), size(corr_abs))
  PAGE: end()
END GPL.






  PAGE: end()
END GPL.

