#' @name sjPlotLinreg.R (V0.6)
#' 
#' @author Daniel Lüdecke
#' 
#' @description Plot beta coefficients of linear regressions with confidence intervalls as dot plot
#' see http://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/
#' 
#' Based on an an idea from surefoss:
#' http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/
#'
#' Parameter:
#' @param fit the model of the linear regression (lm-Object).
#' @param title Diagram's title as string.
#'          Example: title=c("my title")
#' @param sort determines whether the predictors are sorted by beta-values (default, or use \code{"beta"} as
#'          parameter) or by standardized beta values (use \code{"std"}).
#' @param type the plot type. may be either \code{b}, \code{bar}, \code{bars} (default) for bar charts,
#'          or \code{l}, \code{line}, \code{lines} for line diagram.
#'          Example: type="lines"
#' @param axisLabels.y labels of the predictor variables (independent vars) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: axisLabels.y=c("Label1", "Label2", "Label3").
#'          Note: If you use the \code{sjImportSPSS.R} script with the \code{getValueLabels} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param showAxisLabels.y Whether x axis text (category names, predictor labels) should be shown (use \code{TRUE})
#'          or not. Default ist \code{TRUE}
#' @param axisLabelSize The size of value labels in the diagram. Default is 4, recommended values range
#'          between 2 and 8.
#' @param axisLabelColor the color of the category labels (predictor labels). Default is a dark grey (grey30)
#' @param axisTitle.x a label for the x axis.
#' @param axisTitleColor the color of the x axis label.
#' @param axisTitleSize the size of the x axis label
#' @param axisLimits defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param valueLabelColor colour of the values (significant beta coefficients) inside the diagrams. Only applies, when parameter
#'          \code{showValueLabels} is set to \code{TRUE}. Use any valid colour value, e.g. \code{valueLabelColor="grey50"} or
#'          \code{valueLabelColor=c("#cc3366")}.
#' @param valueLabelColorNS colour of the non significant values (non significant beta coefficients) inside the diagrams.
#'          Only applies, when parameter \code{showValueLabels} is set to \code{TRUE}. Use any valid colour value, e.g. 
#'          \code{valueLabelColor="grey50"} or \code{valueLabelColor=c("#cc3366")}.
#' @param valueLabelSize size of the value labels. Drfault is 4.5. Recommended Values range from
#'          2 to 8
#' @param valueLabelAlpha the alpha level (transparancy) of the value labels. Default is 0.8, use
#'          any value from 0 to 1.
#' @param axisLabelAngle.y angle for axis-labels, passed as numeric value.
#' @param errorBarColor the color of the error bars that indicate the confidence intervalls
#'          of the beta-coefficients
#' @param errorBarWidth the width of the error bar ends. Default is 0.1
#' @param errorBarSize the size of the error bar. Default is 0.5
#' @param pointColor the colour of the points that indicate the beta-value
#' @param pointSize the size of the points that indicate the beta-value. Default is 3.
#' @param pointColorStdBeta the colour of the points that indicate the standardized 
#'          beta-value
#' @param pointSizeStdBeta the size of the points that indicate the 
#'          standardized beta-value. Default is 3.
#' @param stdBetaLineType the standardized beta-value dots are connected by a thin line
#'          for a better overview. With this parameter you can specify the line type.
#' @param stdBetaLineAlpha the alpha-value for the line that connects the
#'          standardized beta-value dots
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showValueLabels Whether the beta and standardized beta values should be plotted 
#'          to each dot or not.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not
#' @param showModelSummary if \code{TRUE} (default), a summary of the regression model with 
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param showStandardBeta Whether or not the dots for the standardized beta values 
#'          should be plotted to the diagram
#' @param showStandardBetaLine Whether or not the connecting line for the standardized beta values 
#'          should be plotted to the diagram
#'          
#' @examples
# fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
# sjp.lm(fit, gridBreaksAt=2)
# sjp.lm(fit, gridBreaksAt=2, showStandardBeta=FALSE, errorBarWidth=0)


sjp.lm <- function(fit,
                    sort="beta",
                    title=NULL, 
                    axisLabels.y=NULL, 
                    showAxisLabels.y=TRUE,
                    axisLabelSize=1.1,
                    axisLabelColor="darkgray",
                    axisTitle.x="Estimates",
                    axisTitleSize=1.4,
                    axisTitleColor=c("#444444"),
                    axisLimits=NULL,
                    valueLabelColor="grey20",
                    valueLabelColorNS="grey50",
                    valueLabelSize=4.5,
                    valueLabelAlpha=0.8,
                    axisLabelAngle.y=0, 
                    errorBarColor="#3366a0",
                    errorBarWidth=0.1,
                    errorBarSize=0.5,
                    pointColor="#3366a0",
                    pointSize=3,
                    pointColorStdBeta="#cc5533",
                    pointSizeStdBeta=3,
                    stdBetaLineType=2,
                    stdBetaLineAlpha=0.3,
                    breakTitleAt=50, 
                    breakLabelsAt=12, 
                    gridBreaksAt=0.2,
                    borderColor=NULL, 
                    axisColor=NULL, 
                    theme=NULL,
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    showTickMarks=TRUE,
                    showValueLabels=TRUE, 
                    showPValueLabels=TRUE,
                    showModelSummary=TRUE,
                    showStandardBeta=TRUE,
                    showStandardBetaLine=TRUE) {
  
  require(ggplot2)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  # Help function that unlists a list into a vector
  unlistlabels <- function(lab) {
    dummy <- unlist(lab)
    labels <- c()
    for (i in 1:length(dummy)) {
      labels <- c(labels, as.character(dummy[i]))
    }
    return (labels)
  }
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }

  
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    title <- gsub(pattern, '\\1\n', title)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    axisTitle.x <- gsub(pattern, '\\1\n', axisTitle.x)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels.y))
      axisLabels.y[n] <- gsub(pattern, '\\1\n', axisLabels.y[n])
  }
  
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    # get F-statistics
    fstat <- summary(fit)$fstatistic
    # Calculate p-value for F-test
    pval <- pf(fstat[1], fstat[2], fstat[3],lower.tail = FALSE)
    # indicate significance level by stars
    pan <- c("")
    if (pval<=0.005) {
      pan <- c("***")
    }
    else  if (pval<=0.01) {
        pan <- c("**")
    }
    else  if (pval<=0.05) {
      pan <- c("*")
    }
    # create mathematical term
    modsum <- as.character(as.expression(
      substitute(italic(b[0]) == a * "," ~~ R^2 == r2 * "," ~~ "F" == f*panval * "," ~~ "AIC" == aic,
                 list(a=format(coef(fit)[1], digits=3),
                      r2=format(summary(fit)$r.squared, digits=3),
                      f=sprintf("%.2f", fstat[1]),
                      panval=pan,
                      aic=sprintf("%.2f", AIC(fit))))))
  }
  

  # ----------------------------
  # print beta- and p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  pv <- coef(summary(fit))[-1,4]
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # retrieve betas, leave out intercept ([-1])
  bv <- coef(fit)[-1]
  # retrieve standardized betas
  stdbv <- betaCoef(fit)
  # init data column for p-values
  ps <- c(round(bv,2))
  pstdbv <- c(round(stdbv,2))
  # if no values should be shown, clear
  # vector now
  if (!showValueLabels) {
    ps <- rep(c(""), length(ps))
    pstdbv <- rep(c(""), length(pstdbv))
  }
  
  # --------------------------------------------------------
  # copy p-values into data column
  # --------------------------------------------------------
  if (showPValueLabels) {
    for (i in 1:length(pv)) {
      if (pv[i]>=0.05) {
      }
      else if (pv[i]>=0.01 && pv[i]<0.05) {
        ps[i] <- paste(ps[i], "*")
      }
      else if (pv[i]>=0.001 && pv[i]<0.01) {
        ps[i] <- paste(ps[i], "**")
      }
      else {
        ps[i] <- paste(ps[i], "***")
      }
    }  
  }
  
  # --------------------------------------------------------
  # create new data.frame, since ggplot requires data.frame as parameter
  # The data frame contains betas, CI and p-values
  # --------------------------------------------------------
  tmp<-data.frame(cbind(
    # Append beta coefficients, [-1] means that the first
    # row (Intercept) will be removed / ignored
    coefficients(fit)[-1],
    # append CI
    confint(fit, level=0.95)[-1,]))
  # append p-values and standardized beta coefficients
  # further more, we take the stand. beta as string, because in
  # case no values are drawn, we simply use an empty string.
  # finally, we need the p-values of the coefficients, because the value
  # labels may have different colours according to their significance level
  betas <- cbind(tmp, c(ps), betaCoef(fit), c(pstdbv), pv)
  
  # --------------------------------------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- row.names(betas)
  }
  
  # --------------------------------------------------------
  # define sorting critaria. the values on the x-axis are being sorted
  # either by beta-values (sort="beta") or by standardized
  # beta values (sort = anything else)
  # --------------------------------------------------------
  
  # --------------------------------------------------------
  # sort labels descending in order of (std.) beta values
  # --------------------------------------------------------
  if (sort=="beta") {
    axisLabels.y <- axisLabels.y[order(bv)]
  }
  else {
    axisLabels.y <- axisLabels.y[order(stdbv)]
  }
  
  # --------------------------------------------------------
  # sort rows of data frame descending in order of (std.) beta values
  # --------------------------------------------------------
  if (sort=="beta") {
    betas <- betas[order(bv),]
  }
  else {
    betas <- betas[order(stdbv),]
  }
  betas <- cbind(c(seq(1:nrow(betas))), betas)
  betas$p <- as.character(betas$p)
  # give columns names
  names(betas)<-c("xv", "Beta", "lower", "upper", "p", "stdbeta", "pstdbv", "pv")

  
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user-defined range (if "axisLimits"
  # is not NULL)
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    upper_lim <- (ceiling(10*max(betas$upper))) / 10
    lower_lim <- (floor(10*min(betas$lower))) / 10
  }
  else {
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  ticks<-c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  
  
  # --------------------------------------------------------
  # Set theme and default grid colours. grid colours
  # might be adjusted later
  # --------------------------------------------------------
  if (is.null(theme)) {
    ggtheme <- theme_gray()
  }
  else if (theme=="bw") {
    ggtheme <- theme_bw()
  }
  else if (theme=="classic") {
    ggtheme <- theme_classic()
  }
  else if (theme=="minimal") {
    ggtheme <- theme_minimal()
  }
  else if (theme=="none") {
    ggtheme <- theme_minimal()
    majorGridColor <- c("white")
    minorGridColor <- c("white")
    showTickMarks <-FALSE
  }
  
  
  # --------------------------------------------------------
  # Set up grid colours
  # --------------------------------------------------------
  majorgrid <- NULL
  minorgrid <- NULL
  if (!is.null(majorGridColor)) {
    majorgrid <- element_line(colour=majorGridColor)
  }
  if (!is.null(minorGridColor)) {
    minorgrid <- element_line(colour=minorGridColor)
  }
  
  
  # --------------------------------------------------------
  # Set up visibility of tick marks
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  
  
  # --------------------------------------------------------
  # Start plot here!
  # --------------------------------------------------------
  betaplot <- ggplot(betas, aes(y=Beta, x=xv)) +
    # print point
    geom_point(size=pointSize, colour=pointColor) +
    # and error bar
    geom_errorbar(aes(ymin=lower, ymax=upper), size=errorBarSize, width=errorBarWidth, colour=errorBarColor)
  # show points for standard beta values
  if (showStandardBeta) {
    # first, add points that indicate standardized beta values to get an impression of
    # the effect strength of predictors
    betaplot <- betaplot +
      geom_point(aes(y=stdbeta, x=xv), colour=pointColorStdBeta, size=pointSizeStdBeta) +
      # Print std.beta-values. With vertical adjustment, so they don't overlap with the errorbars
      geom_text(aes(label=pstdbv, y=stdbeta, colour=pv>0.05), vjust=1.8, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
      # to better distinguish betas and stand. betas, the stand. beta points can be connected with a line
    if (showStandardBetaLine) {
      # print line for standardized beta values
      betaplot <- betaplot +
        geom_line(aes(y=stdbeta, x=xv), colour=pointColorStdBeta, linetype=stdBetaLineType, alpha=stdBetaLineAlpha)
    }
  }
  betaplot <- betaplot +
    # Print p-values. With vertical adjustment, so they don't overlap with the errorbars
    geom_text(aes(label=p, y=Beta, colour=pv>0.05), vjust=-0.8, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE) +
    # give value labels different colours depending on significance level
    scale_colour_manual(values=c(valueLabelColor, valueLabelColorNS)) +
    # set y-scale-limits, breaks and tick labels
    scale_y_continuous(limits=c(lower_lim,upper_lim), breaks=ticks, labels=ticks) +
    # set value labels to x-axis
    scale_x_discrete(labels=axisLabels.y, limits=c(1:nrow(betas))) +
    # flip coordinates
    coord_flip() +
    labs(title=title, x=NULL, y=axisTitle.x) +
    ggtheme +
    # set axes text and 
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
          axis.text.y = element_text(angle=axisLabelAngle.y))
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      betaplot <- betaplot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      print("Parameter 'borderColor' can only be applied to 'bw' theme.")
    }
  }
  if (!is.null(axisColor)) {
    betaplot <- betaplot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    betaplot <- betaplot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    betaplot <- betaplot + 
      theme(panel.grid.major = majorgrid)
  }
  # check whether modelsummary should be printed
  if (showModelSummary) {
    # add annotations with model summary
    # annotations include intercept-value and model's r-square
    betaplot <- betaplot + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha, vjust=-0.5, hjust=1.1)
  }
  # print plot
  print(betaplot)
}



# Funktion zur Berechnung der VIF-Werte
# (Varianz Inflations Faktoren), die die Multikolinearität (d.h.
# die Korrelation der unabhängigen Variablen einer linearen Regression
# untereinander) prüft.
#
# Werte über 5 deuten auf Kolinearität hin, d.h. entsprechende Variablen
# sind ggf. zu verwerfen. Bei hoher Signifikanz der Variablen kann auch
# eine Grenze von 10 angesetzt werden, die nicht überschritten werden soll.
#
# In schwarz über den Balken steht der VIF-Wert, in weiß in den Balken
# sind die Signifikanz-Niveaus (p-Werte) der jeweiligen UV zu sehen.
#
# Parameter
#' @param linreg das Modell der linearen Regression (lm-Funktion), wobei in der
#'               lm-funktion der Parameter \code{x} auf \code{TRUE} gesetzt werden muss.
#' @param showValueLabels gibt an, ob die Zahlenwerte über den Balken angezeigt werden sollen
#' @param showPValueLabels gibt an, ob die p-Werte (Sternchen) in den Balken abgezeigt werden sollen
#' @param printnumbers gibt an, ob die VIF-Werte in der Konsole ausgegeben werden sollen
VIFValues <- function(linreg,
                       showValueLabels=TRUE, 
                       showPValueLabels=TRUE, 
                       printnumbers=TRUE) {
  # variance inflation factor
  require(HH)
  # claculate VIF
  val <- vif(linreg) 
  # Wenn VIF-Werte an Konsole ausgegeben werden sollen, diese nun ausdrucken
  if (printnumbers) {
    print(val)
  }
  # retrieve highest VIF-value to determine y-axis range
  maxval <- val[which.max(val)]
  # determine upper limit of y-axis
  upperLimit <-10
  # check whether maxval exceeds the critical VIF-Limit
  # of 10. If so, set upper limit to max. value
  if (maxval >= upperLimit) {
    upperLimit <- ceiling(maxval)
  }
  # ----------------------------
  # print p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  pv <- coef(summary(linreg))[-1,4]
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # init data column for p-values
  # copy OR-values into data column
  ps <- c(round(val,2))
  psv <- c(1:length(pv))
  # copy p-values into data column
  if (showPValueLabels) {
    for (i in 1:length(pv)) {
      if (pv[i]>0.05) {
        psv[i] <- c("")
      }
      else if (pv[i]>0.01 && pv[i]<=0.05) {
        psv[i] <- c("*")
      }
      else if (pv[i]>0.001 && pv[i]<=0.01) {
        psv[i] <- c("**")
      }
      else {
        psv[i] <- c("***")
      }
    }  
  }
  # Neuen data.frame erstellen, da ggplot einen data.frame als parameter
  # braucht. Der data.frame beinhaltet betas, CI und p-Werte.
  tmp<-data.frame(cbind(val))
  
  mydat <- cbind(tmp)
  # Neue Variable erstellen, damit die Ergebnisse sortiert werden
  # können (siehe reorder in ggplot-Funktion)
  mydat$vars<-row.names(mydat)
  # die variablenlabel sollen noch mal sortiert werden, nach 
  # VIF-Werten aufsteigend. Dies ist für die X-Achsenbeschriftung
  # nötig, da diese sonst nicht mehr mit den sortierten VIF-Werten
  # (Balkenreihenfolge auf X-Achse) übereinstimmt
  mydat <- cbind(mydat, mydat[order(val),2])
  # Spalten sollen Namen kriegen
  names(mydat)<-c("vif", "vars", "label")
                 
  # p-werte und sternchen als neue spalten anhängen
  mydat <- cbind(mydat, ps, psv)

  require(ggplot2)
  # grafik ausgeben, dabei die variablen der X-Achse nach aufsteigenden
  # VIF-Werten ordnen
  print(ggplot(mydat, aes(x=reorder(vars, vif), y=vif)) +
    # Balken zeichnen. Stat=identity heißt, dass nicht die counts, sondern
    # die tatsächlichen Zahlenwerte (VIF-Werte) abgebildet werden sollen
    geom_bar(stat="identity", width=0.7, fill="#80acc8") +
    # grüne Linie zeichnen, die den guten Bereich anzeigt (VIF < 5)
    geom_hline(yintercept=5, linetype=2, colour="darkgreen", alpha=0.7) +
    # rote  Linie zeichnen, die den tolerablen Bereich anzeigt (VIF < 10)
    geom_hline(yintercept=10, linetype=2, colour="darkred", alpha=0.7) +
    # p_Werte (Sternchen) setzen. Leicht versetzt (v/hjust), damit lesbar
    # p-Werte in schwarz über balken (vjust negativ), sternchen in weiß
    # in den balken (vjust positiv)
    geom_text(aes(label=ps, y=vif), vjust=-0.5, colour="black", size=4) +
    geom_text(aes(label=psv, y=vif), vjust=1.2, colour="white", size=6) +
    # grüne und rote Line beschriften
    annotate("text", x=1, y=4.7, label="good", size=4, colour="darkgreen") +
    annotate("text", x=1, y=9.7, label="tolerable", size=4, colour="darkred") +
    # als X-Achsenbeschriftung die Variablennamen setzen
    scale_x_discrete(labels=mydat$label) +
    # Keine weiteren Titel an X- und Y-Achse angeben
    labs(title="Variance Inflation Factors", x=NULL, y=NULL) +
    # maximale Obergrenze der Y-Achse setzen
    scale_y_continuous(limits=c(0, upperLimit), expand=c(0,0)) +
    # Beschriftung der X-Achse (Variablenlabel) in 45-Grad-Winkel setzen
    theme(axis.text.x=element_text(angle=45, vjust=0.5, size=rel(1.2))))
}

plotModelAssumptions.lm <- function(linreg, showOriginalModelOnly=TRUE, completeDiagnostic=FALSE) {
  library(car)
  library(lmtest)
  require(ggplot2)

  # ---------------------------------
  # remove outliers
  # ---------------------------------
  # copy current model
  model <- linreg
  # get r2
  rs <- summary(model)$r.squared
  # maximum loops
  maxloops <- 10
  maxcnt <- maxloops
  # remember how many cases have been removed
  removedcases <- 0
  loop <- TRUE
  # start loop
  while(loop==TRUE) {
    # get outliers of model
    ol <- outlierTest(model)
    # retrieve variable numbers of outliers
    vars <- as.numeric(attr(ol$p, "names"))
    # update model by removing outliers
    dummymodel <- update(model, subset=-c(vars))
    # retrieve new r2
    dummyrs <- summary(dummymodel)$r.squared
    # decrease maximum loops
    maxcnt <- maxcnt -1
    # check whether r2 of updated model is lower
    # than previous r2 or if we have already all loop-steps done,
    # stop loop
    if(dummyrs<rs || maxcnt<1) {
      loop <- FALSE
    }
    else {
      # else copy new model, which is the better one (according to r2)
      model <- dummymodel
      # and get new r2
      rs <- dummyrs
      # count removed cases
      removedcases <- removedcases + length(vars)
    }
  }
  
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  cat(sprintf(("\nRemoved %i cases during %i step(s).\nR-square/adj. R-square of original model: %f / %f\nR-square/adj. R-square of updated model: %f / %f\n\n"), 
              removedcases,
              maxloops-(maxcnt+1), 
              summary(linreg)$r.squared, 
              summary(linreg)$adj.r.squared,
              summary(model)$r.squared, 
              summary(model)$adj.r.squared))
  
  modelOptmized <- ifelse(removedcases>0, TRUE, FALSE)
  if (showOriginalModelOnly) modelOptmized <- FALSE
  
  # ---------------------------------
  # show VIF-Values
  # ---------------------------------
  VIFValues(linreg, printnumbers=FALSE)
  if (modelOptmized) VIFValues(model, printnumbers=FALSE)
  
  # ---------------------------------
  # Print non-normality of residuals and outliers both of original and updated model
  # ---------------------------------
  y <- quantile(linreg$resid[!is.na(linreg$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  interc <- y[1L] - slope * x[1L]
  print(ggplot(linreg, aes(sample=.stdresid)) + 
          stat_qq() + 
          geom_abline(slope=slope, intercept=interc, color="blue") +
          ggtitle("Non-normality of residuals and outliers (original model)"))

  if (modelOptmized) {
    y <- quantile(model$resid[!is.na(model$resid)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    interc <- y[1L] - slope * x[1L]
    print(ggplot(model, aes(sample=.stdresid)) + 
            stat_qq() + 
            geom_abline(slope=slope, intercept=interc, color="blue") +
            ggtitle("Non-normality of residuals and outliers (updated model)"))
  }
  
  # ---------------------------------
  # Print non-normality of residuals both of original and updated model
  # ---------------------------------
  print(ggplot(linreg, aes(x=.resid, y=..density..)) + 
          geom_histogram(binwidth=0.2, fill="grey60", colour="grey30") +
          geom_density(fill="#4080cc", alpha=0.2) +
          ggtitle("Non-normality of residuals (original model)"))
  
  if (modelOptmized) {
    print(ggplot(model, aes(x=.resid, y=..density..)) + 
          geom_histogram(binwidth=0.2, fill="grey60", colour="grey30") +
          geom_density(fill="#4080cc", alpha=0.2) +
          ggtitle("Non-normality of residuals (updated model)"))
  }
  
  # ---------------------------------
  # Non-constant residuals
  # ---------------------------------
  # Frage: Können hohe Werte auf der X-Achse genauso gut hervorgesagt
  # werden wie niedrige Werte auf der X-Achse? Das Muster muss sich ähneln
  # über den Verlauf der X-Achse
  # A linear trend would mean that the error of the model (the difference between observed and fitted values) 
  # is in some way systematic. If, for instance, lower fitted values have residuals that are more towards the 0 line. 
  # Higher fitted values are consistently more off, so the model is more wrong with larger values. So, ideally what 
  # you want is something that is akin towards a horizontal line. In such case, the data is somehow not homogenous 
  # maybe because one part of the data is more variable than another. If that is the case, you might need to transform 
  # the data in order to make it meet the assumptions that are necessary for linear models.  
  print(ggplot(linreg, aes(x=.fitted, y=.resid)) +
          geom_hline(yintercept=0, alpha=0.7) +
          geom_point() +
          geom_smooth(se=F) +
          ggtitle("Homoskedastizitaet (Homogenitaet der Varianzen, Zufallsverteilung der Residuen, Ursprungsmodell)"))
  
  if (modelOptmized) {
    print(ggplot(model, aes(x=.fitted, y=.resid)) +
            geom_hline(yintercept=0, alpha=0.7) +
            geom_point() +
            geom_smooth(se=F) +
            ggtitle("Homoskedastizitaet (Homogenitaet der Varianzen, Zufallsverteilung der Residuen, aktuelles Modell)"))
  }
  
  # ---------------------------------
  # summarize old and new model
  # ---------------------------------
  sjp.lm(linreg, title="Original model")
  if (modelOptmized) sjp.lm(model, title="Updated model")

  
  if (completeDiagnostic) {
    # ---------------------------------
    # Non-linearity
    # ---------------------------------
    print(crPlots(linreg))
    
    # ---------------------------------
    # non-independence of residuals
    # ---------------------------------
    print(durbinWatsonTest(linreg))
    
    # ---------------------------------
    # Print leverage plots
    # ---------------------------------
    print(leveragePlots(linreg))
    
    # ---------------------------------
    # Non-constant residuals
    # ---------------------------------
    print(ncvTest(linreg))
    print(bptest(linreg))
    print(spreadLevelPlot(linreg))
  }
  
  # return updated model
  return(model)
}


# standardisierte Beta-Koeffizienten berechnen
# Funktion erstellen (Paramater: lm-Objekt)
betaCoef <- function(MOD) {
  b <- summary(MOD)$coef[-1, 1]
  sx <- sapply(MOD$model[-1], sd)
  sy <- sapply(MOD$model[1], sd)
  beta <- b * sx/sy
  return(beta)
}
