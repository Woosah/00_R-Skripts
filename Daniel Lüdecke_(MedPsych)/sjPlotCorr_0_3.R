#' @name sjPlotCorr.R (V0.3)
#' 
#' @author Daniel Luedecke (d.luedecke@uke.de)
#' 
#' @description Plot correlations as ellipses or tiles
#' See http://strengejacke.wordpress.com/2013/04/18/examples-for-sjplotting-functions-including-correlations-and-proportional-tables-with-ggplot-rstats/
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param data a correlation object, built with the R-cor-function, or a data frame
#'          which correlations should be calculated
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param axisLabels Labels for the x- andy y-axis
#' @param type indicates whether the geoms of correlation values should be plotted
#'          as \code{circle} (default) or as \code{tile}.
#' @param sortCorrelations if \code{TRUE} (default), the axis labels are sorted
#'          according to the correlation strength. if \code{FALSE}, axis labels
#'          appear in order of how variables were included in the cor-computation or
#'          data frame.
#' @param decimals indicates how many decimal values after comma are printed when
#'          the values labels are shown. Default is 3. Only applies when
#'          \code{showCorrelationValueLabels} is \code{TRUE}.
#' @param missingDeletion indicates how missing values are treated. May be either
#'          \code{listwise} (default) or \code{pairwise}.
#' @param corMethod indicates the correlation computation method. May be one of
#'          \code{spearman} (default), \code{pearson} or \code{kendall}.
#' @param geomAlpha specify the transparancy (alpha value) of geom objects (circles or tiles)
#' @param valueLabelColor the color of the value labels (numbers) inside the diagram
#' @param valueLabelSize The size of value labels in the diagram. Default is 4.5, recommended values range
#'          between 2 and 8
#' @param valueLabelAlpha specify the transparancy (alpha value) of value labels
#' @param circleSize specifies the circle size factor. The circle size depends on the correlation
#'          value multiplicated with this factor. Default is 15.
#' @param outlineColor defines the outline color of geoms (circles or tiles). Default is black.
#' @param outlineSize defines the outline size of geoms (circles or tiles). Default is 1.
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisLabelSize The size of variable labels at the axes. Default is 1.1, recommended values range
#'          between 0.5 and 3.0
#' @param axisLabelColor user defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels
#' @param axisLabelAngle.x angle for x-axis-labels
#' @param axisLabelAngle.y angle for y-axis-labels
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param hideDiagCircle if \code{TRUE} (default), the geoms of the diagonal correlations
#'          (self-correlations with value "1") are not plotted. Only applies if parameter
#'          \code{type} is \code{circle}.
#' @param hideLegend show or hide the legend. The legend indicates the strength of correlations
#'          by gradient colour fill.
#' @param showCorrelationValueLabels Whether correlation values should be plotted to each geom
#' @param showCorrelationPValues Whether significance levels (p-values) of correlations should 
#'          be plotted to each geom
#' @param pvaluesAsNumbers if \code{TRUE}, the significance levels (p-values) are printed as numbers.
#'          if \code{FALSE} (default), asterisks are used.
#' @param fillColor a color palette for fillng the geoms. If not specified, the 5th diverging color palette
#'          from the color brewer palettes (RdBu) is used, resulting in red colors for negative and blue colors
#'          for positive correlations, that become lighter the weaker the correlations are. Use any
#'          color palette that is suitbale for the \code{scale_fill_gradientn} parameter of gplot2.
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#'          
#' @examples
# df <- as.data.frame(cbind(rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)))
# sjp.corr(df)
# sjp.corr(df, type="tile", theme="none")


sjp.corr <- function(data,
                     title=NULL,
                     axisLabels=NULL,
                     type="circle",
                     sortCorrelations=TRUE,
                     decimals=3,
                     missingDeletion="listwise",
                     corMethod="spearman",
                     geomAlpha=0.8,
                     valueLabelColor="black",
                     valueLabelSize=4.5,
                     valueLabelAlpha=1,
                     circleSize=15,
                     outlineColor="black",
                     outlineSize=1,
                     axisColor=NULL, 
                     borderColor=NULL, 
                     axisLabelSize=1.1,
                     axisLabelColor="grey30",
                     axisLabelAngle.x=0, 
                     axisLabelAngle.y=0, 
                     breakTitleAt=50, 
                     breakLabelsAt=12, 
                     hideDiagCircle=TRUE,
                     hideLegend=TRUE,
                     legendTitle=NULL,
                     showCorrelationValueLabels=TRUE,
                     showCorrelationPValues=TRUE,
                     pvaluesAsNumbers=FALSE,
                     showTickMarks=FALSE,
                     fillColor=NULL,
                     majorGridColor=NULL,
                     minorGridColor=NULL,
                     theme=NULL) {
  
  require(ggplot2)
  require(reshape2)
  require(scales)

  
  # ----------------------------
  # check for valid parameter
  # ----------------------------
  if (corMethod!="pearson" && corMethod!="spearman" && corMethod!= "kendall") {
    stop("Parameter 'corMethod' must be one of: pearson, spearman or kendall")
  }
  
  
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (class(data)=="matrix") {
    corr <- data
    cpvalues <- NULL
  }
  else {
    # missing deletion corresponds to
    # SPSS listwise
    if (missingDeletion=="listwise") {
      data <- na.omit(data)
      corr <- cor(data, method=corMethod)
    }
    # missing deletion corresponds to
    # SPSS pairwise
    else {
      corr <- cor(data, method=corMethod, use="pairwise.complete.obs")
    }
    #---------------------------------------
    # if we have a data frame as parameter,
    # compute p-values of significances
    #---------------------------------------
    computePValues <- function(df) {
      cp <- c()
      for (i in 1:ncol(df)) {
        pv <- c()
        for (j in 1:ncol(df)) {
          test <- cor.test(df[,i], df[,j], alternative="two.sided", method=corMethod)
          pv <- cbind(pv, round(test$p.value,4))
        }
        cp <- rbind(cp, pv)
      }
      return (cp)
    }
    cpvalues <- computePValues(data)
  }
  
  
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels)) {
    axisLabels <- row.names(corr)
  }
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
  if (!is.null(axisLabels) && is.list(axisLabels)) {
    axisLabels <- unlistlabels(axisLabels)
  }
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) {
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    title <- gsub(pattern, '\\1\n', title)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels))
      axisLabels[n] <- gsub(pattern, '\\1\n', axisLabels[n])
  }

  
  # --------------------------------------------------------
  # order correlations from highest to lowest correlation coefficient
  # --------------------------------------------------------
  if (sortCorrelations) {
    neword <- order(corr[1,])
    orderedCorr <- corr[neword, neword]
    # order variable labels as well
    axisLabels <- axisLabels[neword]
    if (!is.null(cpvalues)) {
      cpvalues <- cpvalues[neword, neword]
    }
  }
  else {
    orderedCorr <- rev(corr)
    axisLabels <- rev(axisLabels)
    if (!is.null(cpvalues)) {
      cpvalues <- rev(cpvalues)
    }
  }  
  
  
  # --------------------------------------------------------
  # prepare a ordering-index-column needed for the data frame
  # that is passed to the ggplot
  # --------------------------------------------------------
  yo <- c()
  for (i in 1:nrow(corr)) {
    yo <- c(yo, c(rep(i,nrow(corr))))
  }
  
  # --------------------------------------------------------
  # melt correlation matrix and create data frame
  # --------------------------------------------------------
  orderedCorr <- melt(orderedCorr)
  if (!is.null(cpvalues)) cpvalues <- melt(cpvalues)
  # bind additional information like order for x- and y-axis
  # as well as the size of plotted points
  orderedCorr <- cbind(orderedCorr, ordx=c(1:nrow(corr)), ordy=yo, psize=c(exp(abs(orderedCorr$value))*circleSize))
  # if the diagonal circles should be hidden, set their point size to 0
  if (hideDiagCircle) {
    orderedCorr$psize[which(orderedCorr$value>=0.999)] <- 0
  }

  orderedCorr$ordx <- as.factor(orderedCorr$ordx)
  orderedCorr$ordy <- as.factor(orderedCorr$ordy)
  

  # --------------------------------------------------------
  # add column with significance value
  # --------------------------------------------------------
  cpv <- c()
  if (!is.null(cpvalues)) {
    if (!pvaluesAsNumbers) {
      if (cpvalues$value>=0.05) cpv <- c("")
      else if (cpvalues$value>=0.01 && cpvalues$value<0.05) cpv <- c("*")
      else if (cpvalues$value>=0.001 && cpvalues$value<0.01) cpv <- c("**")
      else if (cpvalues$value<0.001) cpv <- c("***")
    }
    else {
      cpv <- c(sprintf("\n(%.3f)", round(cpvalues$value,3)))
    }
  }
  else {
    cpv <- c("")
  }
  orderedCorr$ps <- cpv

  
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
  if (!showCorrelationValueLabels) {
    correlationValueLabels <- c("")
  }
  else {
    correlationValueLabels <- c(round(orderedCorr$value,decimals))
    if (showCorrelationPValues) {
      correlationPValues <- orderedCorr$ps
    }
    else {
      correlationPValues <- c("")
    }
  }
  
  
  print(sprintf("Computing correlation using %s-method with %s-deletion...", corMethod, missingDeletion))
  
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  corrPlot <- ggplot(data=orderedCorr, aes(x=ordx, y=ordy, fill=value))
  # --------------------------------------------------------
  # determine the geom type, either points when "type" is "circles"
  # --------------------------------------------------------
  if (type=="circle") {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geop <- geom_point(shape=21, size=orderedCorr$psize, alpha=geomAlpha)
    }
    # ... and apply colour-attribute
    else {
      geop <- geom_point(shape=21, size=orderedCorr$psize, alpha=geomAlpha, colour=outlineColor)
    }
    corrPlot <- corrPlot + 
      geop
  }
  # --------------------------------------------------------
  # or boxes / tiles when "type" is "tile"
  # --------------------------------------------------------
  else {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geot <- geom_tile()
    }
    # ... and apply colour-attribute
    else {
      geot <- geom_tile(size=outlineSize, colour=outlineColor)
    }
    corrPlot <- corrPlot +
      geot
  }
  # fill gradient colour from distinct color brewer palette. negative correlations are dark
  # red, positive corr. are dark blue, and they become lighter the closer they are to a
  # correlation coefficient of zero
  if (is.null(fillColor)) {
    corrPlot <- corrPlot +
      scale_x_discrete(labels=axisLabels) +
      scale_y_discrete(labels=axisLabels) +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=brewer_pal("div",5)(5), limits=c(-1,1))
  }
  else {
    corrPlot <- corrPlot +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=fillColor, limits=c(-1,1))
  }
  corrPlot <- corrPlot +
    geom_text(label=sprintf("%s%s", correlationValueLabels, correlationPValues), colour=valueLabelColor, alpha=valueLabelAlpha, size=valueLabelSize) +
    labs(title=title, x=NULL, y=NULL, fill=legendTitle) +
    ggtheme +
    # set font size for axes.
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          axis.text.y = element_text(angle=axisLabelAngle.y))
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      corrPlot <- corrPlot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      print("Parameter 'borderColor' can only be applied to 'bw' theme.")
    }
  }
  if (!is.null(axisColor)) {
    corrPlot <- corrPlot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    corrPlot <- corrPlot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    corrPlot <- corrPlot + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideLegend) {
    corrPlot <- corrPlot + 
      guides(fill=FALSE)
  }
  # print plot
  plot(corrPlot)
}
