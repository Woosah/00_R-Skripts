#' @name sjPlotPropTable.R (V0.1)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' 
#' @description Plot proportional crosstables of two variables as ggplot diagram.
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param y the variable which proportions (percentage values) should be plotted. The percentage proportion
#'          (within table row, table column or complete table, see parameter \code{tableIndex} of this variable 
#'          are along the y-axis, the variable's categories on the x-axis.
#' @param x the grouping variable, where each value represents a single bar chart within each category of
#'          the \code{y} variable.
#' @param weightBy a weight factor that will be applied to weight all cases from \code{y}
#' @param weightByTitleString if a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: weightByTitleString=" (weighted)"
#' @param type the plot type. may be either \code{b}, \code{bar}, \code{bars} (default) for bar charts,
#'          or \code{l}, \code{line}, \code{lines} for line diagram,
#' @param tableIndex indicates which data from the proportional table should be plotted. Use \code{row} for
#'          calculating row percentages, \code{col} for column percentages and \code{cell} for cell percentages.
#'          Only when \code{tableIndex} is \code{col}, an additional bar chart with the total sum of each column (i.e.
#'          of each category on the x-axis) can be added with the parameter \code{showTotalColumn}.
#' @param barPosition Indicates whether bars should be positioned side-by-side (default, or use \code{dodge} as
#'          parameter) or stacked (use \code{stack} as parameter).
#' @param hideLegend Indicates whether legend (guide) should be shown or not
#' @param reverseX Whether the categories along the x-axis should apper in reversed order or not
#' @param maxYlim indicates how to calculate the maximum limit of the y-axis.
#'          If \code{TRUE}, the y-axes ranges from 0 to 100%.
#'          If \code{FALSE}, the maximum y-axis depends on the highest percentage value of a
#'          variable's answer category. In this case, the y-axis breaks may change,
#'          depending on the variable.
#' @param upperYlim Uses a pre-defined upper limit for the y-axis. Overrides the \code{maxYlim} parameter.
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param legendTitle Title of the diagram's legend
#' @param axisLabels.x Labels for the x-axis breaks
#' @param legendLabels Labels for the guide/legend
#' @param axisLabelSize.x The size of category labels at the axes. Default is 1.1, recommended values range
#'          between 0.5 and 3.0
#' @param valueLabelSize The size of value labels in the diagram. Default is 4, recommended values range
#'          between 2 and 8
#' @param axisLabelAngle.x angle for axis-labels
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
#' @param barWidth Width of bars. Recommended values for this parameter are from 0.4 to 1.5
#' @param barSpace spacing between bars. If unchanges, the grouped bars are sticked together and have no space
#'          in between. Recommended values for this parameter are from 0 to 0.5
#' @param barColor user defined color for bars.
#'          If not specified (\code{NULL}), a default red-green-yellow color palette will be used for the bar charts
#'          If barColor is \code{gs}, a greyscale will be used
#'          If barColor is \code{bw}, a monochrome white filling will be used
#'          If barColor is \code{brewer}, use the \code{colorPalette} parameter to specify a palette of the color brewer
#'          Else specify your own color values as vector (e.g. \code{barColor=c("#f00000", "#00ff00", "#0080ff")})
#' @param colorPalette if \code{barColor} is \code{brewer}, specify a color palette from the color brewer here. All color brewer 
#'          palettes supported by ggplot are accepted here.
#' @param barAlpha specify the transparancy (alpha value) of bars
#' @param lineType the linetype when using line diagrams. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param lineSize the size of lines in a line diagram. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param lineAlpha the alpha value of lines in a line diagram. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param lineDotSize size of dots. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param smoothLines prints a smooth line curve. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param axisLabelColor.x user defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param barOutline If \code{TRUE}, each bar gets a colored outline
#' @param outlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar
#' @param valueLabelPosOnTop Whether value labels should be displayed on top of dodged bars or inside the bars. Default
#'          is \code{TRUE}, i.e. the value labels are displayed on top of the bars. Only applies if parameter \code{barPosition}
#'          is \code{dodge} (default).
#' @param showCategoryLabels Whether x axis text (category names) should be shown or not
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showPlotAnnotation If \code{TRUE}, the groups of dots in a dot-plot are highlighted with a shaded rectangle
#' @param showMeanIntercept if \code{TRUE}, a vertical line in histograms is drawn to indicate the mean value of the count
#'          variables. Only applies to histogram-charts.
#' @param showMeanValue if \code{TRUE} (default value), the mean value is printed to the vertical line that indicates the mean value
#'          of the count variables. Only applies to histogram-charts.
#' @param showStandardDeviation if \code{TRUE}, the standard deviation is annotated as shaded rectangle around the mean intercept
#'          line. Only applies to histogram-charts. The shaded rectangles have borders in the group colors, so it's easier to see
#'          which shaded area belongs to which mean value resp. group
#' @param showTableSummary if \code{TRUE} (default), a summary of the cross tabulation with N, chi-square, df and p-value is printed
#'          to the upper right corner of the diagram. Only applies to bar-charts or dot-plots, i.e. when parameter \code{type} is
#'          either \code{bars} or \code{dots}
#' @param tableSummaryPos position of the model summary which is printed when \code{showTableSummary} is \code{TRUE}. Default is
#'          \code{"r"}, i.e. it's printed to the upper right corner. Use \code{"l"} for upper left corner.
#' @param showTotalColumn if \code{tableIndex} is \code{col}, an additional bar chart with the sum within each category and
#'          it's percentages will be added to each category.
#' @param valueLabelColor the color of the value labels (numbers) inside the diagram
#' @param axisTitle.x a label for the x axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#' @param axisTitle.y a label for the y axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param axisTitleColor the color of the x and y axis labels. refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param axisTitleSize the size of the x and y axis labels. refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#' @param flipCoordinates If \code{TRUE}, the x and y axis are swapped
sjp.xtab <- function(y,
                    x,
                    title=NULL, 
                    legendTitle=NULL,
                    weightBy=NULL,
                    weightByTitleString=NULL,
                    type="bars",
                    tableIndex="col",
                    reverseX=FALSE,
                    maxYlim=TRUE, 
                    upperYlim=NULL, 
                    axisLabels.x=NULL, 
                    axisLabelColor.x="darkgray", 
                    axisLabelAngle.x=0, 
                    axisLabelSize.x=1.1,
                    legendLabels=NULL,
                    valueLabelSize=4,
                    valueLabelColor="black",
                    valueLabelPosOnTop=TRUE,
                    stringTotal="Total",
                    breakTitleAt=50, 
                    breakLabelsAt=12, 
                    breakLegendTitleAt=20, 
                    breakLegendLabelsAt=20,
                    gridBreaksAt=0.2,
                    barWidth=0.7, 
                    barSpace=0.1,
                    barPosition="dodge",
                    barColor=NULL,
                    colorPalette="GnBu",
                    barAlpha=1,
                    lineType=1,
                    lineSize=1,
                    lineAlpha=1,
                    lineDotSize=3,
                    smoothLines=FALSE,
                    borderColor=NULL, 
                    axisColor=NULL, 
                    barOutline=TRUE, 
                    outlineColor="black", 
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    showValueLabels=TRUE,
                    showCategoryLabels=TRUE,
                    showTickMarks=TRUE,
                    showTableSummary=TRUE,
                    tableSummaryPos="r",
                    showTotalColumn=TRUE,
                    hideLegend=FALSE,
                    axisTitle.x=NULL,
                    axisTitle.y=NULL,
                    axisTitleColor="black",
                    axisTitleSize=1.3,
                    theme=NULL,
                    flipCoordinates=FALSE) {

  require(ggplot2)
  require(scales)
  require(plyr)
  require(vcd)

  # determine table index, i.e. if row-percentages, column-percentages
  # or cell-percentages should be displayed
  tindex <- ifelse (tableIndex=="row", 1, 2)
  
  # --------------------------------------------------------
  # We have several options to name the diagram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type=="b" || type=="bar") {
    type <- c("bars")
  }
  if (type=="l" || type=="line") {
    type <- c("lines")
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
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) {
    axisLabels.x <- unlistlabels(axisLabels.x)
  }
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  
  
  # -----------------------------------------------
  # handle zero-counts
  # -----------------------------------------------
  # Determine length of count and group var
  grplen <- length(unique(na.omit(x)))
  countlen <- length(unique(na.omit(y)))
  # if we have legend labels, we know the exact
  # amount of groups
  if (is.null(legendLabels)) {
    grpcount <- grplen
  }
  else {
    grpcount <- length(legendLabels)
  }
  # if we have category labels, we know the exact
  # amount of categories
  if (is.null(axisLabels.x)) {
    catcount <- countlen
  }
  else {
    catcount <- length(axisLabels.x)
  }
  
  
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable
  #---------------------------------------------------
  if (is.null(weightBy)) {
    ftab <- table(y, x)
  }
  else {
    ftab <- round(xtabs(weightBy ~ y + x),0)
  }

  
  # -----------------------------------------------
  # create proportional table so we have the percentage
  # values that should be used as y-value for the bar charts
  # We now have a data frame with categories, group-association
  # and percentage values (i.e. each cell as separate row in the
  # data frame)
  # -----------------------------------------------
  if (tableIndex=="cell") {
    df <- as.data.frame(prop.table(ftab))
  }
  else {
    df <- as.data.frame(prop.table(ftab,tindex))
  }
  # -----------------------------------------------
  # Bind N-values as extra column to the data frame
  # -----------------------------------------------
  df <- cbind(df, as.data.frame(ftab)[,3])
  names(df) <- c("Count", "Group", "Perc", "Sum")

  
  # -----------------------------------------------
  # don't show bar with category sum score when we 
  # have column or cell percentages
  # -----------------------------------------------
  if (tableIndex=="row" || tableIndex=="cell") {
    showTotalColumn <- FALSE
  }

  
  # -----------------------------------------------
  # Sum scores / total percentages for each category
  # -----------------------------------------------
  if (showTotalColumn) {
    # retrieve category counts / percentages, exclude missings of both category and count variable
    dummy <- as.data.frame(prop.table(table(y[which(!is.na(x))])))
    # "insert" dummy column
    dummy <- dummy[,c(1,1,2)]
    # bind sum score
    dummy <- cbind(dummy, c(apply(ftab, MARGIN=1, function(x) sum(x))))
    names(dummy) <- c("Count", "Group", "Perc", "Sum")
    # "modify" resp. correct the Group-column
    dummy$Group <- as.factor(rep(max(na.omit(x))+1))
    # bind data to data frame
    df <- rbind(df, dummy)
  }


  # --------------------------------------------------------
  # Define amount of categories, include zero counts
  # --------------------------------------------------------
  # Zero counts of categories are not plotted by default because
  # these categories don't appear in the data. If we assume a
  # "quasi-continuous" scale (categories from 1 to 4 etc.), we now
  # identify the zero counts and add / insert them into the data frame.
  # This enables us to plot zero counts as well.
  # We guess the maximum amount of categories either by the amount
  # of supplied category labels. If no category labels were passed
  # as parameter, we assume that the maximum value found in the category
  # columns represents the highest category number
  # -----------------------------------------------
  # Handle zero-counts in group-variable
  # only possible if we know the exact number of groups,
  # by passing legend labels
  # -----------------------------------------------
  if (grplen != grpcount) {
    # if the maximum value of the group variable differs from the estimated
    # group length we probably have missing categoriesm, i.e. one group has no
    # cases. Then, we insert an empty row here
    df$Group <- as.numeric(as.character(df$Group))
    # range of groups from lowest to highest group value
    allgroups <- factor(c(min(df$Group):max(df$Group)))
    # retrieve zero-counts, i.e. which group is missing in the data frame
    miss <- as.numeric(as.character(allgroups[!allgroups %in% df$Group]))
    # retrieve subset of all rows where group is from lowest group-value to 
    # missing group
    dummy1 <- df[apply(df, MARGIN=1, function(x) all(x[2]<miss)),]
    # retrieve subset of all rows where group is from missing group to
    # highest group-value
    dummy2 <- df[apply(df, MARGIN=1, function(x) all(x[2]>miss)),]
    # create dummy-data frame that contains the missing row with zero-values
    emptyrows <- as.data.frame(cbind(Count=c(1:countlen), Group=miss, Perc=0.00, Sum=0))
    emptyrows$Count <- as.factor(as.character(emptyrows$Count))
    emptyrows$Group <- as.factor(as.character(emptyrows$Group))
    # bind all three subsets together to a complete data frame
    df <- rbind(dummy1, emptyrows, dummy2)
  }
  # set group-variable as factor
  df$Group <- as.factor(df$Group)
  # -----------------------------------------------
  # Handle zero-counts in count-variable
  # only possible if we know the exact number of categories,
  # by passing category labels
  # -----------------------------------------------
  if (countlen != catcount) {
    # separate data frame for grouping variable. we need this to
    # determine the number of groups
    dfgrp <- as.data.frame(table(df$Group))
    # determine the number of groups
    gcnt <- nrow(dfgrp)
    mydat <- NULL
    # fill in possible zero counts in each group
    for (i in 1:gcnt) {
      # get subset of data frame with each group
      subdf <- df[df$Group == dfgrp$Var1[i],]
      # convert factors to numeric (due to calculations they have
      # to be treated like that)
      subdf$Count <- as.numeric(as.character(subdf$Count))
      subdf$Group <- as.numeric(as.character(subdf$Group))
      # Create a vector of zeros 
      frq <- rep(0,catcount)
      sm <- rep(0,catcount)
      gp <- rep(dfgrp$Var1[i],catcount)
      # Replace the values in freq for those indices which equal dummyf$xa
      # by dummyf$ya so that remaining indices are ones which you 
      # intended to insert 
      frq[subdf$Count] <- subdf$Perc
      sm[subdf$Count] <- subdf$Sum
      # create new data frame. We now have a data frame with all
      # variable categories abd their related counts, including
      # zero counts, but no(!) missings!
      dummydat <- as.data.frame(cbind(Count = 1:catcount, Group=gp, Perc=frq, Sum=sm))
      # append dummy data frame to final data frame
      mydat <- as.data.frame(rbind(mydat, dummydat))
    }
    # copy final data frame
    df <- mydat
  }
  # ----------------------------
  # make sure group and count variable 
  # are factor values
  # ----------------------------
  df$Count <- as.factor(df$Count)
  df$Group <- as.factor(df$Group)
  # add half of Percentage values as new y-position for stacked bars
  df = ddply(df, "Count", transform, ypos = cumsum(Perc) - 0.5*Perc)
  
  
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showTableSummary) {
    # calculate chi square value
    chsq <- chisq.test(ftab)
    # check whether variables are dichotome or if they have more
    # than two categories. if they have more, use Cramer's V to calculate
    # the contingency coefficient
    if (nrow(ftab)>2 || ncol(ftab)>2) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        c2=sprintf("%.2f", chsq$statistic),
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", assocstats(ftab)$cramer),
                        pva=sprintf("%.3f", chsq$p.value)))))
    }
    # if variables have two categories (2x2 table), use phi to calculate
    # the degree of association
    else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        c2=sprintf("%.2f", chsq$statistic),
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", assocstats(ftab)$phi),
                        pva=sprintf("%.3f", chsq$p.value)))))
    }
  }  
  
  
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as parameter
  if (is.null(legendLabels)) {
    # if not, use category text of group variable as legend text
    legendLabels <- c(df$Group)
  }
  legendLabels <- c(legendLabels, stringTotal)
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
  
  
  # --------------------------------------------------------
  # Trim labels and title to appropriate size
  # --------------------------------------------------------
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
  # amount of categories (=number of rows) in dataframe instead
  else  {
    axisLabels.x <- c(1:catcount)
  }
  # --------------------------------------------------------
  
  
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseX) {
    axisLabels.x <- rev(axisLabels.x)
  }

  
  # --------------------------------------------------------
  # Prepare bar charts
  # --------------------------------------------------------
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  if (!is.null(upperYlim)) {
    upper_lim <- upperYlim
  }
  else {
    # else calculate upper y-axis-range depending
    # on the amount of cases...
    if (maxYlim) {
      upper_lim <- 1
    }
    else {
      # ... or the amount of max. answers per category
      upper_lim <- max(((100*df$Perc)+10)/100)
      if (upper_lim >1) {
        upper_lim <- 1
      }
    }
  }

  
  # --------------------------------------------------------
  # define vertical position for labels
  # --------------------------------------------------------
  if (flipCoordinates) {
    # if we flip coordinates, we have to use other parameters
    # than for the default layout
    vert <- 0.35
    hort <- ifelse (barPosition=="dodge", -0.2, waiver())
  }
  else {
    hort <- waiver()
    vpos <- ifelse(valueLabelPosOnTop==TRUE, -0.4, 1.2)
    vert <- ifelse (barPosition=="dodge", vpos, waiver())
  }
  # align dodged position of labels to bar positions
  posdodge <- ifelse(type=="lines", 0, barWidth + barSpace)
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
  # Hide or show Tick Marks and Category Labels (x axis text) 
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showCategoryLabels) {
    axisLabels.x <- c("")
  }
  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  if (is.null(barColor)) {
    # for lines, we have a different default colour palette because RYG is
    # better to distinguish with thin lines. For bars, we take blue colours
    # as default
    fpal <- ifelse(type=="lines", "RdYlGn", "PuBu")
    scalecolors <- scale_fill_brewer(labels=legendLabels, palette=fpal)
    scalecolorsline <- scale_colour_brewer(labels=legendLabels, palette="RdYlGn")
  }
  else if (barColor=="gs") {
    scalecolors <- scale_fill_grey(labels=legendLabels)
    scalecolorsline <- scale_colour_grey(labels=legendLabels)
  }
  else if (barColor=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "barColor"
    scalecolors <- scale_fill_brewer(palette=colorPalette, labels=legendLabels)
    scalecolorsline <- scale_colour_brewer(palette=colorPalette, labels=legendLabels)
  }
  else if (barColor=="bw") {
    barColor <- rep("white", length(legendLabels))
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
    scalecolorsline <- scale_colour_manual(values=barColor, labels=legendLabels)
  }
  else {
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
    scalecolorsline <- scale_colour_manual(values=barColor, labels=legendLabels)
  }
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  if (showValueLabels) {
    # if we have dodged bars or dots, we have to use a slightly dodged position for labels
    # as well, sofor better reading
    if (flipCoordinates) {
      if (barPosition=="dodge") {
        ggvaluelabels <-  geom_text(aes(y=0, label=sprintf("%.01f%% (n=%i)", 100*Perc, Sum)),
                                    size=valueLabelSize,
                                    position=position_dodge(posdodge),
                                    vjust=vert,
                                    hjust=hort,
                                    colour=valueLabelColor)
      }
      else {
        ggvaluelabels <-  geom_text(aes(y=ypos, label=sprintf("%.01f%% (n=%i)", 100*Perc, Sum)),
                                    size=valueLabelSize,
                                    vjust=vert,
                                    hjust=hort,
                                    colour=valueLabelColor)
      }
    }
    else {
      if (barPosition=="dodge") {
        ggvaluelabels <-  geom_text(aes(y=Perc, label=sprintf("%.01f%%\n(n=%i)", 100*Perc, Sum)),
                                    size=valueLabelSize,
                                    position=position_dodge(posdodge),
                                    vjust=vert,
                                    hjust=hort,
                                    colour=valueLabelColor)
      }
      else {
        ggvaluelabels <-  geom_text(aes(y=ypos, label=sprintf("%.01f%%\n(n=%i)", 100*Perc, Sum)),
                                    size=valueLabelSize,
                                    vjust=vert,
                                    hjust=hort,
                                    colour=valueLabelColor)
      }
    }
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
    gridbreaks <- c(seq(0, upper_lim, by=gridBreaksAt))
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
  # Print plot
  # ----------------------------------
  # construct final plot, base constructor
  # first, set x scale
  # ----------------------------------
  scalex <- scale_x_discrete(labels=axisLabels.x)
  # ----------------------------------
  # check whether bars or lines should be printed
  # ----------------------------------
  if (type=="bars") {
    if (barPosition=="dodge") {
      geob <- geom_bar(stat="identity", position=position_dodge(barWidth+barSpace), colour=outlineColor, width=barWidth, alpha=barAlpha)
    }
    else {
      geob <- geom_bar(stat="identity", position="stack", colour=outlineColor, width=barWidth, alpha=barAlpha)
    }
  }
  # check if we have lines
  else if (type=="lines") {
    # if category order is reversed, we also have to apply this to the
    # mapping of geom_line
    if (reverseX) {
      # check whether lines should be smoothed or not
      if (smoothLines) {
        geob <- geom_line(data=df, aes(x=rev(as.numeric(Count)), y=Perc, colour=Group), linetype=lineType, alpha=lineAlpha, size=lineSize, stat="smooth")
      }
      else {
        geob <- geom_line(data=df, aes(x=rev(as.numeric(Count)), y=Perc, colour=Group), linetype=lineType, alpha=lineAlpha, size=lineSize)
      }
    }
    else {
      if (smoothLines) {
        geob <- geom_line(data=df, aes(x=as.numeric(Count), y=Perc, colour=Group), linetype=lineType, alpha=lineAlpha, size=lineSize, stat="smooth")
      }
      else {
        geob <- geom_line(data=df, aes(x=as.numeric(Count), y=Perc, colour=Group), linetype=lineType, alpha=lineAlpha, size=lineSize)
      }
    }
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change x axis order then
  # --------------------------------------------------------
  if (reverseX) {
    baseplot <- ggplot(df, aes(x=rev(Count), y=Perc, fill=Group))
  }
  else {
    baseplot <- ggplot(df, aes(x=Count, y=Perc, fill=Group))
  }  
  baseplot <- baseplot +
    # plot bar chart
    geob
  # if we have line diagram, print lines here
  if (type=="lines") {
    baseplot <- baseplot + 
      geom_point(size=lineDotSize, alpha=lineAlpha, shape=21, show_guide=FALSE)
  }
  # check whether table summary should be printed
  if (showTableSummary) {
    # add annotations with table summary
    # here we print out total N of cases, chi-square and significance of the table
    if (tableSummaryPos=="r") {
      baseplot <- baseplot + annotate("text", label=modsum, parse=TRUE, x=Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, vjust=1.6, hjust=1.1)
    }
    else {
      baseplot <- baseplot + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, vjust=1.6, hjust=-0.1)
    }
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
    scalex +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks=gridbreaks, limits=c(0, upper_lim), expand=c(0,0), labels=percent) +
    scalecolors +
    ggtheme
  # when we have lines, we additionally need to apply "scale_colour"...
  if (type=="lines") {
    baseplot <- baseplot + scalecolorsline
  }
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (flipCoordinates) {
    baseplot <- baseplot + coord_flip()
  }
  # set font size for axes.
  baseplot <- baseplot + 
    theme(axis.text = element_text(size=rel(axisLabelSize.x), colour=axisLabelColor.x), 
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x))
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
