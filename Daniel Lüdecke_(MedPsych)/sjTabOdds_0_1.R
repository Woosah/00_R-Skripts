#' @name sjTabOdds.R (V0.1)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' 
#' @description Pint (multiple) generalized linear models (Odds Ratios)
#' as HTML-Table.
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param ... one or more glm-objects
#' @param file the destination file, which will be in html-format
#' @param labelPredictors labels of the predictor variables, provided as char vector
#' @param labelDependentVariables labels of the dependent variables of all fitted models
#'          which have been used as first parameter(s), provided as char vector
#' @param stringPredictors string constant used as headline for the predictor column.
#'          deaulft is "Predictors".
#' @param stringDependentVariables string constant used as headline for the 
#'          dependent variable columns. default is "Dependent Variables"
#' @param stringModel string constant used as headline for the model names in case no 
#'          labels for the dependent variables are provided (see labelDependentVariables).
#'          default is "Model"
#' @param stringIntercept string constant used as headline for the Intercept row
#'          default is "Intercept"
#' @param pvaluesAsNumbers if TRUE, p-values are shown as numbers. if FALSE (default),
#'          p-values are indicated by asterisks
#' @param boldpvalues if TRUE (default), significant p-values are shown bold faced.
#' @param separateConfColumn if TRUE, the CI values are shown in a separate table column.
#'          default is FALSE
#' @param showAbbrHeadline if TRUE (default), the table data columns have a headline with 
#'          abbreviations for odds ratios, confidence interval and p-values.
#'
#' @examples
# source("lib/sjTabOdds.R")
# lab <- c("Predictor 1", "Predictor 2", "Predictor 3", "Predictor 4")
# labd <- c("Dep. var glm1", "Dep. var glm2", "Dep. var glm3")
# sjt.glm(glm1, glm2, glm3,
#         file="OR_table.html", 
#         labelPredictors=lab,
#         labelDependentVariables=labd,
#         pvaluesAsNumbers=T)
#
# source("lib/sjTabOdds.R")
# y1 <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
# y2 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)
# fitOR1 <- glm(y1 ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic, family=binomial(link="logit"))
# fitOR2 <- glm(y2 ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic, family=binomial(link="logit"))
# lab <- c("Education", "Examination", "Infant Mortality", "Catholic")
# labdep <- c("Fertility", "Agriculture")
# sjt.glm(fitOR1, fitOR2, labelDependentVariables=labdep, labelPredictors=lab, file="or_table1.html")
# sjt.glm(fitOR1, fitOR2, labelDependentVariables=labdep, labelPredictors=lab, file="or_table2.html", pvaluesAsNumbers=T)
# sjt.glm(fitOR1, fitOR2, labelDependentVariables=labdep, labelPredictors=lab, file="or_table3.html", separateConfColumn=T)
# sjt.glm(fitOR1, fitOR2, labelDependentVariables=labdep, labelPredictors=lab, file="or_table4.html", pvaluesAsNumbers=T, separateConfColumn=T)

sjt.glm <- function (..., 
                     file, 
                     labelPredictors, 
                     labelDependentVariables=NULL, 
                     stringPredictors="Predictors", 
                     stringDependentVariables="Dependent Variables", 
                     stringModel="Model",
                     stringIntercept="(Intercept)",
                     pvaluesAsNumbers=FALSE,
                     boldpvalues=TRUE,
                     separateConfColumn=FALSE,
                     showAbbrHeadline=TRUE) {
  
  toWrite = '<html>\n<head>\n<style>table { border-collapse:collapse; border:none; }\nth { border-bottom: 1px solid; }\ntable td { padding:0.2cm; }\ntd.summary { padding-top:0.1cm; padding-bottom:0.1cm }\n.lasttablerow { border-bottom: double; }\n</style>\n</head>\n<body>\n'
  toWrite = paste(toWrite, "<table>", "\n")
  
  input_list <- list(...)

  # -------------------------------------
  # table headline
  # -------------------------------------
  headerColSpan <- length(input_list)
  headerColSpanFactor <- 1
  if (pvaluesAsNumbers) headerColSpanFactor <- headerColSpanFactor+1
  if (separateConfColumn) headerColSpanFactor <- headerColSpanFactor+1
  
  headerColSpan <- headerColSpanFactor * headerColSpan

  toWrite = paste(toWrite, sprintf("  <tr style=\"border-top:2px solid\">\n    <td rowspan=\"2\"><em>%s</em></td>", stringPredictors), "\n")
  toWrite = paste(toWrite, sprintf("    <td colspan=\"%i\" style=\"text-align:center;border-bottom:1px solid;border-top:1px solid\"><em>%s</em></td>", headerColSpan, stringDependentVariables), "\n")
  toWrite = paste(toWrite, "  </tr>\n  <tr>", "\n")
  
  # -------------------------------------
  # table headline: label for dependent variables (model outcomes)
  # -------------------------------------
  if (!is.null(labelDependentVariables)) {
    for (i in 1:length(labelDependentVariables)) {
      if (headerColSpanFactor>1) {
        toWrite = paste(toWrite, sprintf("    <td colspan=\"%i\">%s</td>", headerColSpanFactor, labelDependentVariables[i]), "\n")
      }
      else {
        toWrite = paste(toWrite, sprintf("    <td>%s</td>", labelDependentVariables[i]), "\n")
      }
    }
    toWrite = paste(toWrite, "  </tr>", "\n")
  }
  else {
    for (i in 1:length(input_list)) {
      if (headerColSpanFactor>1) {
        toWrite = paste(toWrite, sprintf("    <td colspan=\"%i\">%s %i</td>", headerColSpanFactor, stringModel, i), "\n")
      }
      else {
        toWrite = paste(toWrite, sprintf("    <td>%s %i</td>", stringModel, i), "\n")
      }
    }
    toWrite = paste(toWrite, "  </tr>", "\n")
  }
  
  # -------------------------------------
  # calculate coefficients and confidence intervalls
  # for all models
  # -------------------------------------
  coeffs <- c()
  confi_lower <- c()
  confi_higher <- c()
  pv <- c()
  
  for (i in 1:length(input_list)) {
    fit <- input_list[[i]]
    coeffs <- rbind(coeffs, exp(coef(fit)))
    confi_lower <- cbind(confi_lower, exp(confint(fit))[,1])
    confi_higher <- cbind(confi_higher, exp(confint(fit))[,2])
    pv <- cbind(pv, round(summary(fit)$coefficients[,4],3))
  }
  
  coeffs <- t(coeffs)
  
  if (!pvaluesAsNumbers) {
    pv <- apply(pv, c(1,2), function(x) {
      if (x>=0.05) x <- c("")
      else if (x>=0.01 && x<0.05) x <- c("*")
      else if (x>=0.001 && x<0.01) x <- c("**")
      else if (x<0.001) x <- c("***")
    })
  }
  else {
    pv <- apply(pv, c(1,2), function(x) {
      if (x <0.05 && boldpvalues) {
        x <- sprintf("<b>%.3f</b>", x)      }
      else {
        x <- sprintf("%.3f", x) 
      }
    })
  }
  
  
  # -------------------------------------
  # table header: or/ci and p-labels
  # -------------------------------------
  if (showAbbrHeadline) {
    toWrite = paste(toWrite, "  <tr>\n    <td>&nbsp;</td>\n")
    colnr <- ifelse(is.null(labelDependentVariables), length(input_list), length(labelDependentVariables))
    for (i in 1:colnr) {
      if (pvaluesAsNumbers) {
        if (separateConfColumn) {
          toWrite = paste(toWrite, "    <td><em>OR</em></td><td><em>CI</em></td><td><em>p</em></td>\n")
        }
        else {
          toWrite = paste(toWrite, "    <td><em>OR (CI)</em></td><td><em>p</em></td>\n")
        }
      }
      else {
        if (separateConfColumn) {
          toWrite = paste(toWrite, "    <td><em>OR</em></td><td><em>CI</em></td>\n")
        }
        else {
          toWrite = paste(toWrite, "    <td><em>OR (CI)</em></td>\n")
        }
      }
    }
    toWrite = paste(toWrite, "  </tr>\n")
  }
  
  
  # -------------------------------------
  # close table headline
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr style=\"border-top:double\">", "\n")
  
  
  # -------------------------------------
  # 1. row: intercept
  # -------------------------------------
  toWrite = paste(toWrite, sprintf("    <td>%s</td>", stringIntercept), "\n")
  for (i in 1:ncol(coeffs)) {
    if (pvaluesAsNumbers) {
      if (separateConfColumn) {
        toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
      else {
        toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)</td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
    }
    else {
      if (separateConfColumn) {
        toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
      else {
        toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f) %s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
    }
  }
  toWrite = paste(toWrite, "  </tr>", "\n")
  
  
  # -------------------------------------
  # subsequent rows: pedictors
  # -------------------------------------
  if (!is.null(labelPredictors)) {
    predlen <- length(labelPredictors)
  }
  else {
    predlen <- ncol(coeffs)-1
  }

  for (i in 1:predlen) {
    toWrite = paste(toWrite, "  <tr>\n", sprintf("    <td>%s</td>", labelPredictors[i]), "\n")
    for (j in 1:ncol(coeffs)) {
      if (pvaluesAsNumbers) {
        if (separateConfColumn) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
      }
      else {
        if (separateConfColumn) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f) %s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
      }
    }
    toWrite = paste(toWrite, "  </tr>", "\n")
  }
  

  PseudoR2 <- function(rr) { # rr must be the result of lm/glm
    n <- nrow(rr$model)
    COX <- (1-exp((rr$deviance-rr$null)/n))
    NR <- COX/(1-exp(-rr$null/n))
    RVAL <- c(N=n, CoxSnell=COX, Nagelkerke=NR)
    return(RVAL)
  }
  Chisquare <- function(rr) {
    return (with(rr, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE), digits=3))
  }
  # -------------------------------------
  # Model-Summary: N
  # -------------------------------------
  if (headerColSpanFactor>1) {
    colspanstring <- sprintf("<td  class=\"summary\" colspan=\"%i\">", headerColSpanFactor)
  }
  else {
    colspanstring <- c("<td class=\"summary\">")
  }
  toWrite = paste(toWrite, "  <tr style=\"border-top:1px solid\">\n    <td class=\"summary\">Observations</td>\n")
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    toWrite = paste(toWrite, sprintf("    %s%i</td>\n", colspanstring, psr[1]))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: pseudo r2
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr>\n    <td class=\"summary\">Pseudo-R<sup>2</sup></td>\n")
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    toWrite = paste(toWrite, sprintf("    %sR<sup>2</sup><sub>CS</sub> = %.3f<br>R<sup>2</sup><sub>N</sub> = %.3f</td>\n", colspanstring, psr[2], psr[3]))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: chisquare
  # -------------------------------------
#   toWrite = paste(toWrite, "  <tr>\n    <td class=\"summary\"><em>p</em>(&Chi;<sup>2</sup>)</td>\n")
#   for (i in 1:length(input_list)) {
#     psr <- PseudoR2(input_list[[i]])
#     toWrite = paste(toWrite, sprintf("    %s%.3f</td>\n", colspanstring, Chisquare(input_list[[i]])))
#   }
#   toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: log likelihood
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr>\n    <td class=\"summary\">-2 Log-Likelihood</td>\n")
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    toWrite = paste(toWrite, sprintf("    %s%.3f</td>\n", colspanstring, -2*logLik(input_list[[i]])))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  
  
    
  # -------------------------------------
  # table footnote
  # -------------------------------------
  toWrite = paste(toWrite, sprintf("  <tr style=\"border-top:2px solid\">\n    <td>Notes</td><td style=\"text-align:right\" colspan=\"%i\"><em>* p<0.005&nbsp;&nbsp;&nbsp;** p<0.01&nbsp;&nbsp;&nbsp;*** p <0.001</em></td>\n  </tr>", headerColSpan), "\n")
  

  # -------------------------------------
  # finish table
  # -------------------------------------
  toWrite = paste(toWrite, "</table>\n</body></html>", "\n")
  write(toWrite, file=file)  
}
