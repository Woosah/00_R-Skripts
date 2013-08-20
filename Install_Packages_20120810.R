### Andy Field - Packages aus dem Buch

# Download & Installation (Achtung: WRS-Package benoetigt genaue Link-Angabe, s.u.!)

install.packages("boot")
install.packages("car")
install.packages("clinfun")
install.packages("compute.es")
install.packages("corpcor")
#install.packages("DSUR")
install.packages("effects")
install.packages("ez")
install.packages("foreign")
install.packages("ggm")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("GPArotation")
install.packages("Hmisc")
install.packages("MASS")
install.packages("mlogit")
install.packages("multcomp")
install.packages("mvnormtest")
install.packages("mvoutlier")
install.packages("nlme")
install.packages("pastecs")
install.packages("pgirmess")
install.packages("polycor")
install.packages("psych")
install.packages("QuantPsyc")
install.packages("Rcmdr")
install.packages("reshape")
install.packages("WRS", repos = "http://R-Forge.R-project.org", type = "source")


#Hier kann die aktuelle Development-Version von lme4 abgerufen werden
#(soll mehr können...!)
#
#Ist aber jetzt (10-08-2012) noch nicht für meine Zwecke nutzbar...buggy!
#
#install.packages("lme4",repos="http://r-forge.r-project.org")

#Sarkar (2008) -> Lattice - Multivariate Data Visualization with R
#
#R-Code für das Installieren aller notwendigen Packages und für das Schreiben der
#R-Code-Beispiele für alle Buchkapitel!

install.packages(c("latticeExtra", "copula", "ellipse", "gridBase",
                   "locfit", "logspline", "mapproj", "maps", "MEMSS",
                   "mlmRev", "RColorBrewer"))
source("http://bioconductor.org/biocLite.R")
biocLite(c("flowCore", "flowViz", "hexbin"))

#Unbedingt das Zielverzeichnis anpassen, wenn nötig ("destdir"):

srcdir <- "http://lmdvr.r-forge.r-project.org/code/"
destdir <- "/Users/ulf/R-Studio/01 R-Literatur/Sarkar_2008_Springer_Lattice_Multivariate_Data_Visualization_with_R"
chapters <- sprintf("Chapter%02d.R", 1:14)
for (i in chapters)
{
  download.file(url = paste(srcdir, i, sep = ""),
                destfile = file.path(destdir, i))
}


source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")



## Den R-Commander kann man mit der folgenden Anweisung laden, 
## was aber wegen der R-Studio-Nutzung erstmal egal ist...

#library(Rcmdr)

## Zusätzliche Packages, die vielleicht nötig / von Interesse sind:


install.packages("plotrix")
install.packages("knitr")
install.packages("MPTinR")
library(devtools)
install.packages("glmmADMB", repos="http://R-Forge.R-project.org", type = "source")
install.packages("afex")
install.packages("VGAM")
install.packages("languageR")
install.packages("Zelig")
