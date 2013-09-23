# first: install dependent packages
install.packages(c("MASS", "akima", "robustbase"))

# second: install suggested packages
install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))

# third: install an additional package which provides some C functions
# install.packages('devtools')
library("devtools")
install_github( "WRScpp", "mrxiaohe")

# fourth: install WRS
install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")