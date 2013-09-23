#' @name sjImportSPSS.R (V0.4)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' 
#' @description Import data from SPSS, including NA's, value and variable labels.
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @example 
#' source("sjImportSPSS.R")
#' mydata <- importSPSS("SPSS-data.sav")
#' 
#' @param path The file path to the SPSS data set
importSPSS <- function(path, enc=NA) {
  # init foreign package
  require("foreign")
  # import data as data frame
  data.spss <- read.spss(path, to.data.frame=TRUE, use.value.labels=FALSE, reencode=enc)
  # return data frame
  return(data.spss)
}


# ---------------------------------------------------------------------
#' @description This function retrieves the value labels of an importet
#' SPSS data set and returns the result as list.
#' 
#' @example
#' mydat_labels <- getValueLabels(mydat)
#' 
#' @param dat a data frame containing imported SPSS data
getValueLabels <- function(dat) {
  a <- lapply(dat, FUN = getValLabels)
  return (a)
}


# ---------------------------------------------------------------------
# Help-function
getValLabels <- function(x){
  rev(names(attr(x, "value.labels")))
}


# ---------------------------------------------------------------------
#' @description This function retrieves the variable labels of an importet
#' SPSS data set and returns the result as list.
#' 
#' @example
#' mydat_vars <- getVariableLabels(mydat)
#' 
#' @param dat a data frame containing imported SPSS data
getVariableLabels <- function(dat) {
  return(attr(dat, "variable.labels"))
}


# ---------------------------------------------------------------------
#' @description This function converts variable values with their
#' associated value labels. Might be helpful for factor variables.
#' For instance, if you have a Gender variable with 0/1, and associated
#' labels are male/female, this function would convert all 0 to male and
#' all 1 to female in the data frame.
#' 
#'@param variable a (factor) variable 
convertToLabel <- function(variable) {
  vl <- rev(names(attr(variable, "value.labels")))
  vn <- sort(unique(na.omit(variable)))
  
  for (i in 1:length(vl)) {
    variable[variable==vn[i]] <- vl[i]
  }
  return (variable)
}


# ---------------------------------------------------------------------
noumlaute <- function(variable) {
  ## ----------------------------------------------------------------------                                                                                            
  ## Funktion entfernt stoerende Umlaute, unten stehende Liste ggf. erweitern                                                                                          
  ## ----------------------------------------------------------------------                                                                                            
  variable <- gsub("ä","ae",variable)
  variable <- gsub("ü","ue",variable)
  variable <- gsub("ö","oe",variable)
  variable <- gsub("Ü","Ue",variable)
  variable <- gsub("Ä","Ae",variable)
  variable <- gsub("Ö","Oe",variable)
  variable <- gsub("ß","ss",variable)
  return(variable)
}
