#' @name sjPlotOdds.R (V0.7)
#' 
#' @author Daniel Lüdecke
#' 
#' @description Plot odds ratios with confidence intervalls as bar chart or dot plot
#' see http://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/
#' 
#' Based on the script from surefoss:
#' http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/
#'
#' Parameter:
#' @param fit the model of the logistic regression (glm-Object)
#' @param title Diagram's title as string
#'          Example: title=c("my title")
#' @param axisLabels.y labels of the predictor variables (independent vars, odds) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: axisLabels.y=c("Label1", "Label2", "Label3").
#'          Note: If you use the \code{sjImportSPSS.R} script with the \code{getValueLabels} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param axisLabelSize The size of value labels in the diagram. Default is 1.1, recommended values range
#'          between 0.7 and 3.0
#' @param showAxisLabels.y Whether odds names (predictor labels) should be shown or not
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param axisTitle.x a label ("title") for the x axis.
#' @param axisTitleColor the color of the x axis label
#' @param axisTitleSize the size of the x axis label
#' @param axisLimits defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param axisLabelAngle.y angle for axis-labels where the predictor labels (\code{axisLabels.y}) are printed. Note
#'          that due to the coordinate flip, the acutal x-axis with predictor labels are appearing on the y-axis.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#' @param transformTicks if \code{TRUE}, the grid bars have exponential distances, i.e. they
#'          visually have the same distance from one grid bar to the next. Default is \code{FALSE} which
#'          means that grids are plotted on every \code{gridBreaksAt}'s position, thus the grid bars
#'          become narrower with higher odds ratio values.
#' @param hideErrorBar if \code{TRUE}, the error bars that indicate the confidence intervals of the odds ratios are not
#'          shown. Only applies if parameter \code{type} is \code{bars}. Default value is \code{FALSE}.
#' @param pointSize the size of the points that indicate the beta-value. Default is 3.
#' @param barColor a two-dimensional vector that colors the odds values (i.e. points and error bars in case the
#'          parameter \code{type} is \code{plots} or the bar charts in case of \code{bars}). The first color value indicates
#'          odds ratio values larger than 1, the second color value indicates odds ratio values lower or equal to 1.
#'          Default colors is a blue/red-scheme. You can also use \code{"bw"} or \code{"black"} for only one colouring
#'          in almost black, \code{"gray"} / \code{"grey"} / \code{"gs"} for a grayscale or \code{"brewer"} for colours
#'          from the color brewer palette.
#'          If barColors is \code{brewer}, use the \code{colorPalette} parameter to specify a palette of the color brewer
#'          Else specify your own color values as vector (e.g. \code{barColors=c("#f00000", "#00ff00")})
#' @param colorPalette if parameter \code{barColor} is \code{brewer}, specify a color palette from the color brewer here.
#'          All color brewer palettes supported by ggplot are accepted here.
#' @param barWidth the width of the bars in bar charts. only applies if parameter \code{type} is \code{bars}. Default is 0.5
#' @param barAlpha the alpha value of the bars in bar charts. only applies if parameter \code{type} is \code{bars}. Default is 1
#' @param axisLabelColor colour of the tick labels at the axis (variable names, odds names)
#' @param valueLabelColor the colour of the odds values. These values are printed above the plots respectively beside the
#'          bar charts. default color is \code{"black"}.
#' @param valueLabelSize size of the value labels. Drfault is 4.5. Recommended Values range from
#'          2 to 8
#' @param valueLabelAlpha the alpha level (transparancy) of the value labels. Default is 1, use
#'          any value from 0 to 1.
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param barOutline If \code{TRUE}, each bar gets a colored outline. only applies if parameter \code{type} is \code{bars}.
#' @param outlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}
#' @param interceptLineType the linetype of the intercept line (zero point). Default is \code{2} (dashed line)
#' @param interceptLineColor the color of the intercept line. default value is \code{"grey70"}
#' @param errorBarWidth the width of the error bar ends. Default is \code{0.1}
#' @param errorBarSize the size (thickness) of the error bars. Default is \code{0.5}
#' @param errorBarLineType the linetype of error bars. Default is \code{1} (solid line)
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#' @param showValueLabels Whether the beta and standardized beta values should be plotted 
#'          to each dot or not.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not
#' @param showModelSummary if \code{TRUE} (default), a summary of the regression model with 
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
sjp.glm <- function(fit, 
                    title=NULL, 
                    axisLabels.y=NULL, 
                    axisLabelSize=1.1,
                    axisLabelAngle.y=0, 
                    axisLabelColor="darkgray", 
                    axisTitle.x="Odds Ratios",
                    axisTitleSize=1.2,
                    axisTitleColor=c("#444444"),
                    axisLimits=NULL,
                    breakTitleAt=50, 
                    breakLabelsAt=12, 
                    gridBreaksAt=0.2,
                    transformTicks=FALSE,
                    type="plots",
                    hideErrorBars=FALSE,
                    errorBarWidth=0.1,
                    errorBarSize=0.5,
                    errorBarLinetype=1,
                    pointSize=3,
                    colorPalette="Paired",
                    barColor=NULL,
                    barWidth=0.3,
                    barAlpha=1,
                    valueLabelColor="black",
                    valueLabelSize=4.5,
                    valueLabelAlpha=1,
                    axisColor=NULL, 
                    borderColor=NULL, 
                    barOutline=FALSE, 
                    outlineColor="black", 
                    interceptLinetype=2,
                    interceptLinecolor="grey70",
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    theme=NULL,
                    showAxisLabels.y=TRUE,
                    showTickMarks=TRUE,
                    showValueLabels=TRUE, 
                    showPValueLabels=TRUE,
                    showModelSummary=TRUE) {
  # load ggplot
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
  
  
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
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
  if (!is.null(axisLabels.y)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels.y))
      axisLabels.y[n] <- gsub(pattern, '\\1\n', axisLabels.y[n])
  }
  

  # create data frame for ggplot
  tmp <- data.frame(cbind(exp(coef(fit)), exp(confint(fit))))
  # ----------------------------
  # print p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  pv <- coef(summary(fit))[-1,4]
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # retrieve odds ratios, leave out intercept ([-1])
  ov <- exp(coef(fit))[-1]
  # init data column for p-values
  ps <- NULL
  for (i in 1:length(pv)) {
    ps[i] <- c("")
  }
  # ----------------------------
  # copy OR-values into data column
  # ----------------------------
  if (showValueLabels) {
    for (i in 1:length(pv)) {
      ps[i] <- c(round(ov[i],2))
    }
  }
  # ----------------------------
  # copy p-values into data column
  # ----------------------------
  if (showPValueLabels) {
    for (i in 1:length(pv)) {
      if (pv[i]>0.05) {
      }
      else if (pv[i]>0.01 && pv[i]<=0.05) {
        ps[i] <- paste(ps[i], "*")
      }
      else if (pv[i]>0.001 && pv[i]<=0.01) {
        ps[i] <- paste(ps[i], "**")
      }
      else {
        ps[i] <- paste(ps[i], "***")
      }
    }  
  }
  # ----------------------------
  # remove intercept
  # ----------------------------
  odds <- cbind(tmp[-1,])
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- row.names(odds)
  }
  

  # ----------------------------
  # sort labels descending in order of
  # odds ratio values
  # This is necessary because the OR-values are reorderd by size
  # in the ggplot function below
  # ----------------------------
  axisLabels.y <- axisLabels.y[order(ov)]
  

  # ----------------------------
  # bind p-values to data frame
  # ----------------------------
  odds <- cbind(odds, ps)
  # set column names
  names(odds)<-c("OR", "lower", "upper", "p")
  lhj <- ifelse(odds$OR>1, 1.3, -0.3)
  odds <- cbind(odds, labhjust=lhj)
  # ----------------------------
  # Create new variable. Needed for sorting the variables / OR
  # in the graph (see reorder in ggplot-function)
  # ----------------------------
  odds$vars<-row.names(odds)

  
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # check whether we have bar chart and error bars hidden
    # in this case, the upper limit does not correspond to the
    # upper CI, but to the highest OR value
    if (type=="bars" && hideErrorBars) {
      upper_lim <- (ceiling(10*max(odds$OR))) / 10
      lower_lim <- (floor(10*min(odds$OR))) / 10
    }
    else {
      # else we have confindence intervals displayed, so
      # the range corresponds to the boundaries given by
      # the CI's
      upper_lim <- (ceiling(10*max(odds$upper))) / 10
      lower_lim <- (floor(10*min(odds$lower))) / 10
    }
  }
  else {
    # Here we have user defind axis range
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # --------------------------------------------------------
  # Define axis ticks, i.e. at which position we have grid
  # bars.
  # --------------------------------------------------------
  ticks<-c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  # since the odds are plotted on a log-scale, the grid bars'
  # distance shrinks with higher odds values. to provide a visual
  # proportional distance of the grid bars, we can apply the
  # exponential-function on the tick marks
  if (transformTicks) {
    ticks <- exp(ticks)-1
    ticks <- round(ticks[which(ticks<=upper_lim)],1)
  }
  
  
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    modsum <- as.character(as.expression(
      substitute("(Intercept)" == ic * "," ~~ lambda == la * "," ~~ chi^2 == c2 * "," ~~ "AIC" == aic,
                 list(ic=sprintf("%.2f", exp(coef(fit)[1])),
                      la=sprintf("%.2f", logLik(fit)),
                      c2=sprintf("%.2f", with(fit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)), digits=3),
                      aic=sprintf("%.2f", fit$aic)))))
  }
  

  # --------------------------------------
  # Formatierungen: Generell bei ggplot gilt: "fill"-Wert in
  # "aes"-Parameter der ggplot-Funktion bezieht sich darauf,
  # welche Werte eine neue Farbe kriegen sollen (mapping).
  # Innerhalb von geom_bar etc. bezieht sich der "fill"-Parameter
  # auf die verschiedenen Farbwerte, die gesetzt werden sollen.
  # --------------------------------------
  
  # --------------------------------------------------------
  # define bar / line colors
  # --------------------------------------------------------
  # if we have no odds lower than one, swicth fill colours
  # so we have the correct colour for odds > 1
  switchcolors <- ifelse (length(which(ov<1))==0, TRUE, FALSE)
  # check whether barColor is defined
  if (is.null(barColor)) {
    # define default colours
    if (switchcolors) barcols <- c("#3399cc", "#cc5544") else barcols <- c("#cc5544", "#3399cc")
  }
  else {
    # if we have b/w colors, i.e. no differentiation between odds > 1 and < 1,
    # we simply set both colors for ORs lower and greater than 1 to the same color-value
    if (barColor=="bw" || barColor=="black") {
      barcols <- c("#333333", "#333333")
    }
    # grey-scale colors
    else if (barColor=="gray" || barColor=="grey" || barColor=="gs") {
      if (switchcolors) barcols <- c("#555555", "#999999") else barcols <- c("#999999", "#555555")
    }
    else {
      # else, use user-colors
      barcols <- barColor
    }
  }
  # check whether we have brewer color scale
  if (!is.null(barColor) && barColor=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "oddsColorss"
    if (type=="plots") {
      # plots need scale_colour
      scalecolors <- scale_colour_brewer(palette=colorPalette, guide=FALSE)
    }
    else {
      # bars need scale_fill
      scalecolors <- scale_fill_brewer(palette=colorPalette, guide=FALSE)
    }
  }
  else {
    if (type=="plots") {
      scalecolors <- scale_colour_manual(values=barcols, guide=FALSE)
    }
    else {
      scalecolors <- scale_fill_manual(values=barcols, guide=FALSE)
    }
  }
  
  
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
  # Set up visibility oftick marks
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  
  
  # --------------------------------------------------------
  # check whether bars should have an outline
  # --------------------------------------------------------
  if (!barOutline) {
    outlineColor <- waiver()
  }
  
  
  # --------------------------------------------------------
  # body of plot, i.e. this is the same in both bar and dot plots
  # --------------------------------------------------------
  if (type=="plots") {
    # Order odds according to beta-coefficients
    plotHeader <- ggplot(odds, aes(y=OR, x=reorder(vars, OR)))
  }
  else {
    # Order odds according to beta-coefficients, fill bars according to
    # OR-value greater / lower than 1
    plotHeader <- ggplot(odds,aes(y=OR, x=reorder(vars, OR), fill=(OR>1)))    
  }
  # --------------------------------------------------------
  # start with dot-plotting here
  # --------------------------------------------------------
  if (type=="plots") {
    plotHeader <- plotHeader +
      # Order odds according to beta-coefficients, colour points and lines according to
      # OR-value greater / lower than 1
      geom_point(size=pointSize, aes(colour=(OR>1))) +
      # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin=lower, ymax=upper, colour=(OR>1)), width=errorBarWidth, size=errorBarSize, linetype=errorBarLinetype) +
      # print value labels and p-values
      geom_text(aes(label=p, y=OR), vjust=-0.7, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha)
  }
  # --------------------------------------------------------
  # start with bar plots here
  # --------------------------------------------------------
  else if (type=="bars") {
    # Order odds according to beta-coefficients, colour points and lines according to
    # OR-value greater / lower than 1
    plotHeader <- plotHeader +
      # stat-parameter indicates statistics
      # stat="bin": y-axis relates to count of variable
      # stat="identity": y-axis relates to value of variable
      geom_bar(stat="identity", position="identity", width=barWidth, colour=outlineColor, alpha=barAlpha) +
      # print value labels and p-values
      geom_text(aes(label=p, y=1), vjust=-1, hjust=odds$labhjust, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha)
    if (hideErrorBars==FALSE) {
      plotHeader <- plotHeader +
        # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=errorBarWidth, size=errorBarSize, linetype=errorBarLinetype)
    }
  }
  # check whether modelsummary should be printed
  if (showModelSummary) {
    # add annotations with model summary
    # here we print out the log-lik-ratio "lambda" and the chi-square significance of the model
    # compared to the null-model
    plotHeader <- plotHeader + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha, vjust=-0.5, hjust=1.1)
  }
  plotHeader <- plotHeader +
    # Intercept-line
    geom_hline(yintercept=1, linetype=interceptLinetype, color=interceptLinecolor) +
    labs(title=title, x=NULL, y=axisTitle.x) +
    scale_x_discrete(labels=axisLabels.y) +
    # logarithmic scale for odds
    scale_y_log10(limits=c(lower_lim,upper_lim), breaks=ticks, labels=ticks) +
    scalecolors +
    ggtheme +
    coord_flip() +
    # set axes text and 
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
          axis.text.y = element_text(angle=axisLabelAngle.y))
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      plotHeader <- plotHeader + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      print("Parameter 'borderColor' can only be applied to 'bw' theme.")
    }
  }
  if (!is.null(axisColor)) {
    plotHeader <- plotHeader + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    plotHeader <- plotHeader + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    plotHeader <- plotHeader + 
      theme(panel.grid.major = majorgrid)
  }

  
  # print plot
  plot(plotHeader)
}


plotModelAssumptions.glm <- function(logreg) {
  require(car)
  require(faraway)
  require(ggplot2)
  
  # ---------------------------------
  # remove outliers
  # ---------------------------------
  # copy current model
  model <- logreg
  # get AIC-Value
  aic <- logreg$aic
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
    # retrieve new AIC-value
    dummyaic <- dummymodel$aic
    # decrease maximum loops
    maxcnt <- maxcnt -1
    # check whether AIC-value of updated model is larger
    # than previous AIC-value or if we have already all loop-steps done,
    # stop loop
    if(dummyaic >= aic || maxcnt<1) {
      loop <- FALSE
    }
    else {
      # else copy new model, which is the better one (according to AIC-value)
      model <- dummymodel
      # and get new AIC-value
      aic <- dummyaic
      # count removed cases
      removedcases <- removedcases + length(vars)
    }
  }
  
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  cat(sprintf(("\nRemoved %i cases during %i step(s).\nAIC-value of original model: %.2f\nAIC-value of updated model: %.2f\n\n"), 
              removedcases,
              maxloops-(maxcnt+1), 
              logreg$aic, 
              model$aic))
  
  # ------------------------------------------------------
  # Overdispersion
  # Sometimes we can get a deviance that is much larger than expected 
  # if the model was correct. It can be due to the presence of outliers, 
  # sparse data or clustering of data. A half-normal plot of the residuals 
  # can help checking for outliers:
  # ------------------------------------------------------
  halfnorm(residuals(logreg))
  halfnorm(residuals(model))

  # -------------------------------------
  # Anova-Test
  # We can see that all terms were highly significant when they were 
  # introduced into the model.
  print(anova.glm(logreg,test="Chisq"))
  print(anova.glm(model,test="Chisq"))
  # -------------------------------------
  

  sjp.glm(logreg, title="Original model")
  sjp.glm(model, title="Updated model")
  
  # return updated model
  return(model)
}