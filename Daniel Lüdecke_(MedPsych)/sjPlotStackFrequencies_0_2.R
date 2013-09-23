#' @name sjPlotStackFrequencies.R (V0.2)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' @note Thanks to Forrest Stevens (http://www.clas.ufl.edu/users/forrest/) for bug fixes
#' 
#' @description Plot items (variables) of a scale as stacked proportional bars
#' see http://strengejacke.wordpress.com/2013/07/17/plotting-likert-scales-net-stacked-distributions-with-ggplot-rstats/
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param items a data frame with each column representing one likert-item.
#' @param legendLabels a list or vector of strings that indicate the likert-scale-categories and which
#'          appear as legend text.
#' @param weightBy a weight factor that will be applied to weight all cases from \code{items}
#' @param weightByTitleString if a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: weightByTitleString=" (weighted)"
#' @param hideLegend Indicates whether legend (guide) should be shown or not
#' @param reverseX if \code{TRUE}, the item order on the x-axis is reversed.
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param legendTitle Title of the diagram's legend
#' @param includeN if \code{TRUE} (default), the N of each item is included into axis labels
#' @param axisLabels.x Labels for the x-axis breaks
#' @param axisLabelSize The size of category labels at the axes. Default is 1.1, recommended values range
#'          between 0.5 and 3.0
#' @param axisLabelAngle.x angle for axis-labels
#' @param axisLabelColor user defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels
#' @param valueLabelSize The size of value labels in the diagram. Default is 4, recommended values range
#'          between 2 and 8
#' @param valueLabelColor The color of value labels in the diagram. Default is black.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param diagramMargins if \code{TRUE} (default), the diagram has marginsm, i.e. the y-axis is not exceeded
#'          to the diagram's boundaries.
#' @param barWidth Width of bars. Recommended values for this parameter are from 0.4 to 1.5
#' @param barColor user defined color for bars.
#'          If not specified (\code{NULL}), a default blue color palette will be used 
#'          for the bar charts. You can use pre-defined color-sets that are independent from the amount of categories:
#'          If barColor is \code{brewer}, use the \code{colorPalette} parameter to specify a palette of the color brewer
#'          Else specify your own color values as vector (e.g. \code{barColor=c("darkred", "red", "green", "darkgreen")})
#' @param colorPalette if \code{barColor} is \code{brewer}, specify a color palette from the color brewer here. All color brewer 
#'          palettes supported by ggplot are accepted here.
#' @param barAlpha specify the transparancy (alpha value) of bars
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param barOutline If \code{TRUE}, each bar gets a colored outline
#' @param outlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param axisTitle.x a label for the x axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#' @param axisTitle.y a label for the y axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param axisTitleColor the color of the x and y axis labels. refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param axisTitleSize the size of the x and y axis labels. refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar
#' @param showItemLabels Whether x axis text (category names) should be shown or not
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showSeparatorLine if \code{TRUE}, a line is drawn to visually "separate" each bar in the diagram.
#' @param separatorLineColor the color of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}
#' @param separatorLineSize the size of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}
#' @param legendPos the position of the legend. Default is \code{right}. Use one of the following values:
#'          \code{right}, \code{left}, \code{bottom}, \code{top}
#' @param legendSize the size of the legend
#' @param legendBorderColor the border color of the legend
#' @param legendBackColor the background color of the legend
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#' @param flipCoordinates If \code{TRUE}, the x and y axis are swapped
#' 
#' @examples
# items <- list(c("Q1", "Q2", "Q3", "Q4", "Q5"))
# levels_42 <- list(c("Independent", "Slightly dependent", "Dependent", "Severely dependent"))
# likert_4 <- data.frame(as.factor(sample(1:4, 500, replace=T, prob=c(0.2,0.3,0.1,0.4))),
#                        as.factor(sample(1:4, 500, replace=T, prob=c(0.5,0.25,0.15,0.1))),
#                        as.factor(sample(1:4, 500, replace=T, prob=c(0.25,0.1,0.4,0.25))),
#                        as.factor(sample(1:4, 500, replace=T, prob=c(0.1,0.4,0.4,0.1))),
#                        as.factor(sample(1:4, 500, replace=T, prob=c(0.35,0.25,0.15,0.25))))
# sjp.stackfrq(likert_4, legendLabels=levels_42, axisLabels.x=items)
# levels_62 <- list(c("Independent", "Slightly dependent", "Dependent", "Very dependent", "Severely dependent", "Very severely dependent"))
# likert_6 <- data.frame(as.factor(sample(1:6, 500, replace=T, prob=c(0.2,0.1,0.1,0.3,0.2,0.1))),
#                        as.factor(sample(1:6, 500, replace=T, prob=c(0.15,0.15,0.3,0.1,0.1,0.2))),
#                        as.factor(sample(1:6, 500, replace=T, prob=c(0.2,0.25,0.05,0.2,0.2,0.2))),
#                        as.factor(sample(1:6, 500, replace=T, prob=c(0.2,0.1,0.1,0.4,0.1,0.1))),
#                        as.factor(sample(1:6, 500, replace=T, prob=c(0.1,0.4,0.1,0.3,0.05,0.15))))
# sjp.stackfrq(likert_6, legendLabels=levels_62, axisLabels.x=items)


sjp.stackfrq <- function(items,
                        legendLabels,
                        weightBy=NULL,
                        weightByTitleString=NULL,
                        hideLegend=FALSE,
                        reverseX=TRUE,
                        title=NULL, 
                        legendTitle=NULL,
                        includeN=TRUE,
                        axisLabels.x=NULL,
                        axisLabelSize=1.1,
                        axisLabelAngle.x=0, 
                        axisLabelColor="grey30", 
                        valueLabelSize=4,
                        valueLabelPosOnTop=TRUE,
                        valueLabelColor="black",
                        breakTitleAt=50, 
                        breakLabelsAt=30, 
                        breakLegendTitleAt=30, 
                        breakLegendLabelsAt=28,
                        gridBreaksAt=0.2,
                        diagramMargins=FALSE,
                        barWidth=0.5, 
                        barColor=NULL,
                        colorPalette="GnBu",
                        barAlpha=1,
                        borderColor=NULL, 
                        axisColor=NULL, 
                        barOutline=FALSE, 
                        outlineColor="black", 
                        majorGridColor=NULL,
                        minorGridColor=NULL,
                        axisTitle.x=NULL,
                        axisTitle.y=NULL,
                        axisTitleColor="black",
                        axisTitleSize=1.3,
                        theme=NULL,
                        showTickMarks=FALSE,
                        showValueLabels=TRUE,
                        showItemLabels=TRUE,
                        showSeparatorLine=FALSE,
                        separatorLineColor="grey80",
                        separatorLineSize=0.3,
                        legendPos="right",
                        legendSize=1,
                        legendBorderColor="white",
                        legendBackColor="white",
                        flipCoordinates=TRUE) {

  require(ggplot2)
  require(scales)
  require(plyr)

  
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
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) {
    axisLabels.x <- unlistlabels(axisLabels.x)
  }
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }

  
  # --------------------------------------------------------
  # Check whether N of each item should be included into
  # axis labels
  # --------------------------------------------------------
  if (includeN && !is.null(axisLabels.x)) {
    for (i in 1:length(axisLabels.x)) {
      axisLabels.x[i] <- paste(axisLabels.x[i], sprintf(" (n=%i)", length(na.omit(items[,i]))), sep="")
    }
  }

  
  # -----------------------------------------------
  # if we have legend labels, we know the exact
  # amount of groups
  # -----------------------------------------------
  countlen <- length(legendLabels)
  
  
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable. do this for each item that was
  # passed as parameter
  #---------------------------------------------------
  mydat <- c()
  # iterate item-list
  for (i in 1:ncol(items)) {
    # get each single items
    variable <- items[,i]
    # -----------------------------------------------
    # create proportional table so we have the percentage
    # values that should be used as y-value for the bar charts
    # We now have a data frame with categories, group-association
    # and percentage values (i.e. each cell as separate row in the
    # data frame)
    # -----------------------------------------------
    # check whether counts should be weighted or not
    if (is.null(weightBy)) {
      df <- as.data.frame(prop.table(table(variable)))
    }
    else {
      df <- as.data.frame(prop.table(round(xtabs(weightBy ~ variable),0)))
    }
    # give columns names
    names(df) <- c("var", "prc")

		##	FRS:  I can't suss out why we convert to numeric but rather than
		##		this I'm just going to number them:
    ## convert to numeric
    #df$var <- as.numeric(as.character(df$var))
    ## if categories start with zero, fix this here
    #if (min(df$var)==0) {
    #  df$var <- df$var+1
    #}
		df$var <- 1:length(df$var)

    # Create a vector of zeros 
    prc <- rep(0,countlen)
    # Replace the values in prc for those indices which equal df$var
    prc[df$var] <- df$prc
    # create new data frame. We now have a data frame with all
    # variable categories abd their related percentages, including
    # zero counts, but no(!) missings!
    mydf <- as.data.frame(cbind(grp=i, cat=1:countlen, prc))
    # now, append data frames
    mydat <- as.data.frame(rbind(mydat, mydf))
  }
  # ----------------------------
  # make sure group and count variable 
  # are factor values
  # ----------------------------
  mydat$grp <- as.factor(mydat$grp)
  mydat$cat <- as.factor(mydat$cat)
  # add half of Percentage values as new y-position for stacked bars
  mydat = ddply(mydat, "grp", transform, ypos = cumsum(prc) - 0.5*prc)
  
  
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  pattern <- c(paste('(.{1,', breakLegendLabelsAt, '})(\\s|$)', sep=""))
  for (n in 1:length(legendLabels)) {
    legendLabels[n] <- gsub(pattern, '\\1\n', legendLabels[n])
  }
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) {
    # if yes, wrap legend title line
    pattern <- c(paste('(.{1,', breakLegendTitleAt, '})(\\s|$)', sep=""))
    legendTitle <- gsub(pattern, '\\1\n', legendTitle)
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) {
      title <- paste(title, weightByTitleString, sep="")
    }
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    title <- gsub(pattern, '\\1\n', title)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.x)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels.x)) {
      axisLabels.x[n] <- gsub(pattern, '\\1\n', axisLabels.x[n])
    }
  }
  # If axisLabels.x were not defined, simply set numbers from 1 to
  # amount of items
  else {
    axisLabels.x <- c(1:length(items))
  }
  
  
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseX) {
    axisLabels.x <- rev(axisLabels.x)
  }

  
  # --------------------------------------------------------
  # define vertical position for labels
  # --------------------------------------------------------
  if (flipCoordinates) {
    # if we flip coordinates, we have to use other parameters
    # than for the default layout
    vert <- 0.35
  }
  else {
    vert <- waiver()
  }
  # --------------------------------------------------------
  # check whether bars should have an outline
  # --------------------------------------------------------
  if (!barOutline) {
    outlineColor <- waiver()
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
  # set diagram margins
  # --------------------------------------------------------
  if (diagramMargins) {
    expgrid <- waiver()
  }
  else {
    expgrid <- c(0,0)
  }
  # --------------------------------------------------------
  # Hide or show Tick Marks and Category Labels (x axis text) 
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showItemLabels) {
    axisLabels.x <- c("")
  }


  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  if (is.null(barColor)) {
    scalecolors <- scale_fill_brewer(labels=legendLabels, palette="PuBu")
  }
  else if (barColor=="gs") {
    scalecolors <- scale_fill_grey(labels=legendLabels)
  }
  else if (barColor=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "barColor"
    scalecolors <- scale_fill_brewer(palette=colorPalette, labels=legendLabels)
  }
  else if (barColor=="bw") {
    barColor <- rep("white", length(legendLabels))
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
  }
  else {
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
  }
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  if (showValueLabels) {
    # if we have dodged bars or dots, we have to use a slightly dodged position for labels
    # as well, sofor better reading
    ggvaluelabels <-  geom_text(aes(y=ypos, label=sprintf("%.01f%%", 100*prc)),
                                size=valueLabelSize,
                                vjust=vert,
                                # hjust=hort,
                                colour=valueLabelColor)
  }
  else {
    ggvaluelabels <-  geom_text(label="")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- waiver()
  }
  else {
    gridbreaks <- c(seq(0, 1, by=gridBreaksAt))
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
  #
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change x axis order then
  # --------------------------------------------------------
  if (reverseX) {
    baseplot <- ggplot(mydat, aes(x=rev(grp), y=prc, fill=cat))
  }
  else {
    baseplot <- ggplot(mydat, aes(x=grp, y=prc, fill=cat))
  }  
  baseplot <- baseplot +
    # plot bar chart
    geom_bar(stat="identity", position="stack", colour=outlineColor, width=barWidth, alpha=barAlpha)
  # --------------------------------------------------------
  # check whether bars should be visually separated by an 
  # additional separator line
  # --------------------------------------------------------
  if (showSeparatorLine) {
    baseplot <- baseplot +
      geom_vline(x=c(seq(1.5, length(items), by=1)), size=separatorLineSize, colour=separatorLineColor)
  }
  # --------------------------------------------------------
  # Hide or show Legend
  # --------------------------------------------------------
  if (hideLegend) {
    # remove guide / legend
    baseplot <- baseplot + guides(fill=FALSE)
  }
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title=title, x=axisTitle.x, y=axisTitle.y, fill=legendTitle) +
    # print value labels to the x-axis.
    # If parameter "axisLabels.x" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scale_x_discrete(labels=axisLabels.x) +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks=gridbreaks, limits=c(0, 1), expand=expgrid, labels=percent) +
    scalecolors  +
    ggtheme
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (flipCoordinates) {
    baseplot <- baseplot + coord_flip()
  }
  # set font size for axes.
  baseplot <- baseplot + 
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x))
  # --------------------------------------
  # set position and size of legend
  # --------------------------------------
  if (!hideLegend) {
    baseplot <- baseplot + 
      theme(legend.position = legendPos,
            legend.text = element_text(size=rel(legendSize)),
            legend.background = element_rect(colour=legendBorderColor, fill=legendBackColor))
  }
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      baseplot <- baseplot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      print("Parameter 'borderColor' can only be applied to 'bw' theme.")
    }
  }
  if (!is.null(axisColor)) {
    baseplot <- baseplot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    baseplot <- baseplot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    baseplot <- baseplot + 
      theme(panel.grid.major = majorgrid)
  }
  plot(baseplot)
}
