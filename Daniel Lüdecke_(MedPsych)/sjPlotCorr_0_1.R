#' @name sjPlotCorr.R (V0.9)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' 
#' @description Plot correlations as ellipses or tiles
#' See 
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param data a correlation object, built with the R-cor-function, or a data frame
#'          which correlations should be calculated
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
sjp.corr <- function(data,
                     title=NULL,
                     axisLabels=NULL,
                     type="circle",
                     sortCorrelations=TRUE,
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
                     showTickMarks=FALSE,
                     fillColor=NULL,
                     majorGridColor=NULL,
                     minorGridColor=NULL,
                     theme=NULL) {
  
  require(ggplot2)
  require(reshape2)
  require(scales)

  
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (class(data)=="matrix") {
    corr <- data
  }
  else {
    corr <- cor(data, method="spearman", use="pairwise.complete.obs")
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
  }
  else {
    orderedCorr <- rev(corr)
    axisLabels <- rev(axisLabels)
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
    correlationValueLabels <- c(round(orderedCorr$value,2))
  }
  
  
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
    geom_text(label=correlationValueLabels, colour=valueLabelColor, alpha=valueLabelAlpha, size=valueLabelSize) +
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
