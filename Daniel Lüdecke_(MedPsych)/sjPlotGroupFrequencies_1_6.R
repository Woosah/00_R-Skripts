#' @name sjPlotGroupFrequencies.R (V1.6)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' 
#' @description Plot grouped or stacked frequency bars of a variables 
#' as ggplot diagram.
#' See http://strengejacke.wordpress.com/2013/03/05/easily-plotting-grouped-bars-with-ggplot
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param y the variable which frequencies should be plotted. The counts of this variable are along the
#'          y-axis, the variable's categories on the x-axis.
#' @param x the grouping variable, where each value represents a single bar chart within each category of
#'          the \code{y} variable.
#' @param weightBy a weight factor that will be applied to weight all cases from \code{y}
#' @param weightByTitleString if a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: weightByTitleString=" (weighted)"
#' @param interactionVar an interaction variable which can be used for box plots. divides each category indicated
#'          by \code{x} into the factors of \code{interactionVar}, so that each category of \code{x}
#'          is subgrouped into \code{interactionVar}'s categories. Only applies when parameter \code{type}
#'          is \code{box} or \code{violin} (resp. their alternative strings like boxplot, boxplots or v).
#' @param barPosition Indicates whether bars should be positioned side-by-side (default, or use \code{dodge} as
#'          parameter) or stacked (use \code{stack} as parameter).
#'          If \code{type} is \code{histogram}, you can use either \code{dodge} (default value), which displays the bars side-by-side,
#'          or \code{identity}, which results in overlaying bars. In the latter case, it's recommended to adjust the 
#'          \code{barAlpha} value.
#' @param type the plot type. may be either \code{b}, \code{bar}, \code{bars} (default) for bar charts,
#'          \code{l}, \code{line}, \code{lines} for line diagram,
#'          \code{d}, \code{dot}, \code{dots} for dot plots,
#'          \code{h}, \code{hist}, \code{histogram} for grouped histograms, 
#'          \code{box}, \code{boxplot}, \code{boxplots} for box plots
#'          or \code{v}, \code{violin} for violin box plots
#' @param dotSize size of dots. Applies only when \code{type} is set to \code{dots}
#' @param hideLegend Indicates whether legend (guide) should be shown or not
#' @param maxYlim indicates how to calculate the maximum limit of the y-axis.
#'          If \code{TRUE}, the upper y-limit corresponds to the amount of cases,
#'          i.e. y-axis for each plot of a data base are the same.
#'          If \code{FALSE}, the maximum y-axis depends on the highest count of a
#'          variable's answer category. In this case, the y-axis breaks may change,
#'          depending on the variable.
#' @param upperYlim Uses a pre-defined upper limit for the y-axis. Overrides the \code{maxYlim} parameter.
#' @param useFacetGrid \code{TRUE} when bar charts should be plotted as facet grids instead of integrated single
#'          bar charts. Ideal for larger amount of groups.
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param legendTitle Title of the diagram's legend
#' @param axisLabels.x Labels for the x-axis breaks. Passed as vector of strings.
#'          Example: axisLabels.x=c("Label1", "Label2", "Label3").
#'          Note: If you use the \code{sjImportSPSS.R} script with the \code{getValueLabels} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param interactionVarLabels Labels for the x-axis breaks when having interaction variables included.
#'          These labels replace the \code{axisLabels.x}. Only applies, when using box or violin plots
#'          and \code{interactionVar} is not \code{NULL}.
#'          Example: See \code{axisLabels.x}
#' @param legendLabels Labels for the guide/legend
#'          Example: See \code{axisLabels.x}
#' @param axisLabelSize The size of axis labels of both x and y axis. Default is 1.1, recommended values range
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
#'          grid is being printed
#' @param barWidth Width of bars. Recommended values for this parameter are from 0.4 to 1.5
#' @param innerBoxPlotWidth the width of the inner box plot that is plotted inside of violin plots. Only applies 
#'          if \code{type} is \code{"violin"}. Default value is 0.15
#' @param innerBoxPlotDotSize size of mean dot insie a violin plot. Applies only when \code{type} is set to \code{violin}
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
#' @param smoothLines prints a smooth line curve. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param axisLabelColor user defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param barOutline If \code{TRUE}, each bar gets a colored outline
#' @param outlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar
#' @param showPercentageValues \code{TRUE} if percentage values should be plotted to each bar, if \code{FALSE},
#'          %-values are removed
#' @param showAxisLabels.x Whether x axis text (category names) should be shown or not
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
#' @param showGroupCount if \code{TRUE}, the count within each group is added to the category labels (e.g. "Cat 1 (n=87)").
#'          Default value is \code{FALSE}.
#' @param tableSummaryPos position of the model summary which is printed when \code{showTableSummary} is \code{TRUE}. Default is
#'          \code{"r"}, i.e. it's printed to the upper right corner. Use \code{"l"} for upper left corner.
#' @param meanInterceptLineType the linetype of the mean intercept line. Only applies to histogram-charts and when
#'          \code{showMeanIntercept} is \code{TRUE}.
#' @param meanInterceptLineSize the size of the mean intercept line. Only applies to histogram-charts and when
#'          \code{showMeanIntercept} is \code{TRUE}.
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
#' @param legendPos the position of the legend, if a legend is drawn. Use \code{"bottom"}, \code{"top"}, \code{"left"}
#'          or \code{"right"} to position the legend above, below, on the left or right side of the diagram. Right
#'          positioning is default.
#' @param legendSize the text size of the legend. Default is 1. Relative size, so recommended values are from 0.3 to
#'          2.5
#' @param legendBorderColor color of the legend's border. Default is "white", so no visible border is drawn
#' @param legendBackColor fill color of the legend's background. Default is "white", so no visible background is drawn
#' @param flipCoordinates If \code{TRUE}, the x and y axis are swapped
#' @param omitNA If \code{TRUE}, missings are not included in the frequency calculation and diagram plot
#' 
#' @examples
# sjp.grpfrq(discoveries, sample(1:3, length(discoveries), replace=T), type="hist", showValueLabels=FALSE, showMeanIntercept=TRUE)
# sjp.grpfrq(ChickWeight$weight, as.numeric(ChickWeight$Diet), type="box")
# sjp.grpfrq(ChickWeight$weight, as.numeric(ChickWeight$Diet), type="v")
# sjp.grpfrq(sample(1:3, length(ChickWeight$Diet), replace=T), as.numeric(ChickWeight$Diet), barSpace=0.2)


sjp.grpfrq <- function(y,
                      x,
                      weightBy=NULL,
                      weightByTitleString=NULL,
                      interactionVar=NULL,
                      type="bars",
                      dotSize=4,
                      hideLegend=FALSE,
                      maxYlim=FALSE, 
                      upperYlim=NULL, 
                      useFacetGrid=FALSE,
                      title=NULL, 
                      legendTitle=NULL,
                      axisLabels.x=NULL, 
                      axisLabelSize=1.1,
                      axisLabelColor="darkgray", 
                      axisLabelAngle.x=0, 
                      interactionVarLabels=NULL,
                      legendLabels=NULL,
                      valueLabelSize=4,
                      valueLabelColor="black",
                      breakTitleAt=50, 
                      breakLabelsAt=12, 
                      breakLegendTitleAt=20, 
                      breakLegendLabelsAt=20,
                      gridBreaksAt=NULL,
                      barPosition="dodge",
                      barWidth=0.6,
                      barSpace=0,
                      barColor=NULL,
                      barAlpha=1,
                      innerBoxPlotWidth=0.15,
                      innerBoxPlotDotSize=3,
                      colorPalette="GnBu",
                      lineType=1,
                      lineSize=1,
                      lineAlpha=1,
                      smoothLines=FALSE,
                      borderColor=NULL, 
                      axisColor=NULL, 
                      barOutline=TRUE, 
                      outlineColor="black", 
                      majorGridColor=NULL,
                      minorGridColor=NULL,
                      showValueLabels=TRUE,
                      showPercentageValues=TRUE,
                      showAxisLabels.x=TRUE,
                      showTickMarks=TRUE,
                      showPlotAnnotation=TRUE,
                      showMeanIntercept=FALSE,
                      showMeanValue=TRUE,
                      showStandardDeviation=FALSE,
                      showTableSummary=TRUE,
                      showGroupCount=FALSE,
                      tableSummaryPos="r",
                      meanInterceptLineType=2,
                      meanInterceptLineSize=0.5,
                      axisTitle.x=NULL,
                      axisTitle.y=NULL,
                      axisTitleColor="black",
                      axisTitleSize=1.3,
                      theme=NULL,
                      legendPos="right",
                      legendSize=1,
                      legendBorderColor="white",
                      legendBackColor="white",
                      flipCoordinates=FALSE,
                      omitNA=TRUE) {

  require(ggplot2)
  require(plyr)
  require(vcd)
  
  # --------------------------------------------------------
  # count variable may not be a factor!
  # --------------------------------------------------------
  if (is.factor(y)) {
    y <- as.numeric(as.character(y))
  }

  
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
  if (type=="d" || type=="dot") {
    type <- c("dots")
  }
  if (type=="h" || type=="hist") {
    type <- c("histogram")
    showTableSummary <- FALSE
    showGroupCount <- FALSE
  }
  if (type=="box" || type=="boxplot") {
    type <- c("boxplots")
  }
  if (type=="v") {
    type <- c("violin")
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
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) {
    axisLabels.x <- unlistlabels(axisLabels.x)
  }
  if (!is.null(interactionVarLabels) && is.list(interactionVarLabels)) {
    interactionVarLabels <- unlistlabels(interactionVarLabels)
  }

    
  #---------------------------------------------------
  # weight variable
  #---------------------------------------------------
  weightby <- function(var, weight) {
    items <- unique(var)
    newvar <- c()
    for (i in 1:length(items)) {
      newcount = round(sum(weight[which(var==items[i])]))
      newvar <- c(newvar, rep(items[i], newcount))
    }
    return (newvar)
  }
#   if (!is.null(weightBy)) {
#     y <- weightby(y, weightBy)
#   }
  
  
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
  # handle zero-counts
  # -----------------------------------------------
  # Determine length of count and group var
  grplen <- length(unique(na.omit(x)))
  # determine maximum values
  # first, check the total amount of different factor levels
  catcount_1 <- length(unique(na.omit(y)))
  # second, check the maximum factor level
  catcount_2 <- max(na.omit(y))
  # if categories start with zero, fix this here
  if (min(na.omit(y))==0) {
    catcount_2 <- catcount_2+1
  }
  # catcount should contain the higher values, i.e. the maximum count of
  # categories (factor levels) corresponds either to the highest factor level
  # value or to the amount of different factor levels, depending on which one
  # is larger
  catcount <- ifelse (catcount_1 > catcount_2, catcount_1, catcount_2)
  catmin <- min(na.omit(y))
  lower_lim <- 0
  # get the highest answer category of "variable", so we know where the
  # range of the x-axis ends
  if (!is.null(axisLabels.x)) {
    catcount <- length(axisLabels.x)
  }
  # if we have legend labels, we know the exact
  # amount of groups
  if (is.null(legendLabels)) {
    grpcount <- grplen
  }
  else {
    grpcount <- length(legendLabels)
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
  # new data frame from variables
  df <- as.data.frame(ftab)
  # separate data frame for grouping variable. we need this to
  # determine the number of groups
  dfgrp <- as.data.frame(table(df$x))
  # Factors have to be transformed into numeric values
  # for continiuos x-axis-scale
  df$y <- as.numeric(as.character(df$y))
  # if categories start with zero, fix this here
  if (min(df$y)==0) {
    df$y<- df$y+1
  }
  # convcert group variables to chars
  df$x <- as.character(df$x)
  dfgrp$Var1 <- as.character(dfgrp$Var1)
  # determine the number of groups
  grpcnt <- nrow(dfgrp)
  # init data frame. this data frame will contain the final variables
  # needed for plotting with ggplot
  mydat <- NULL
  # fill in possible zero counts in each group
  for (i in 1:grpcnt) {
    # get subset of data frame with each group
    subdf <- df[df$x == dfgrp$Var1[i],]
    # Create a vector of zeros 
    frq <- rep(0,catcount)
    # Replace the values in freq for those indices which equal dummyf$xa
    # by dummyf$ya so that remaining indices are ones which you 
    # intended to insert 
    frq[subdf$y] <- subdf$Freq
    # retrieve group variable as character. grouping might be indicated by
    # "A" or "B", not just 1 or 2.
    group <- as.character(dfgrp$Var1[i])
    # create new data frame. We now have a data frame with all
    # variable categories abd their related counts, including
    # zero counts, but no(!) missings!
    dummydat <- as.data.frame(cbind(count = 1:catcount, group, frq, layer=1:catcount))
    # append dummy data frame to final data frame
    mydat <- as.data.frame(rbind(mydat, dummydat))
  }
  # --------------------------------------------------------
  # Handle missings
  # --------------------------------------------------------
  if (!omitNA) {
    # check for missings in each answer category of our count variable
    for (j in 1:catcount) {
      # get amount of missings
      frq <- length(which(is.na(x[which(y==j)])))
      # create data frame
      tmpdf <- as.data.frame(cbind(count=j, group = "NA", frq=as.numeric(as.character(frq)), layer=j))
      # append dummy data frame to final data frame
      mydat <- as.data.frame(rbind(mydat, tmpdf))
    }
  }
  # convert grouping variable to character
  mydat$group <- as.numeric(as.character(mydat$group))
  # convert frequencies to numeric
  mydat$frq <- as.numeric(as.character(mydat$frq))
  # convert layer to numeric
  mydat$layer <- as.numeric(as.character(mydat$layer))

  
  # -----------------------------------------------
  # Handle zero-counts in group-variable
  # only possible if we know the exact number of groups,
  # by passing legend labels
  # -----------------------------------------------
  if (grplen != grpcount) {
    # if the maximum value of the group variable differs from the estimated
    # group length we probably have missing categoriesm, i.e. one group has no
    # cases. Then, we insert an empty row here
    # range of groups from lowest to highest group value
    allgroups <- factor(c(min(mydat$group):max(mydat$group)))
    # retrieve zero-counts, i.e. which group is missing in the data frame
    miss <- as.numeric(as.character(allgroups[!allgroups %in% mydat$group]))
    # retrieve subset of all rows where group is from lowest group-value to 
    # missing group
    dummy1 <- mydat[apply(mydat, MARGIN=1, function(xy) all(xy[2]<miss)),]
    # retrieve subset of all rows where group is from missing group to
    # highest group-value
    dummy2 <- mydat[apply(mydat, MARGIN=1, function(xy) all(xy[2]>miss)),]
    # create dummy-data frame that contains the missing row with zero-values
    emptyrows <- as.data.frame(cbind(count=c(1:catcount), group=miss, frq=0, layer=1:catcount))
    emptyrows$count <- as.factor(as.character(emptyrows$count))
    emptyrows$group <- as.factor(as.character(emptyrows$group))
    # bind all three subsets together to a complete data frame
    mydat <- rbind(dummy1, emptyrows, dummy2)
  }
  # set group-variable as factor
  mydat$group <- as.factor(mydat$group)
  
  
  # --------------------------------------------------------
  # calculate percentages
  # --------------------------------------------------------
  # init empty vector, with length of data frame's rows
  prz <- rep(0,nrow(mydat))
  # iterate all data frame rows
  for (k in 1: length(prz)) {
    # if we have facet grids, calculate percentage
    # within each group
    if (useFacetGrid) {
      # get frequency value of each row and divide it by the sum of frequencies of
      # all frequencies of the current row's group
      prz[k] <- c(round(100*mydat[k,3]/sum(mydat$frq[mydat$group==mydat[k,2]]),2))
    }
    # if we have dodged/stacked bars or plots, calculate percentage
    # within each category
    else {
      # get frequency value of each row and divide it by the sum of frequencies of
      # all frequencies of the current row's category
      prz[k] <- c(round(100*mydat[k,3]/sum(mydat$frq[mydat$count==mydat[k,1]]),2))
    } 
  }
  # bind percentage as final column
  mydat <- as.data.frame(cbind(mydat, prz))
  # convert prz to numeric
  mydat$texty <- as.numeric(as.character(mydat$prz))
  # add half of Percentage values as new y-position for stacked bars
  mydat = ddply(mydat, "count", transform, ypos = cumsum(frq) - 0.5*frq)
  # --------------------------------------------------------
  
  
  # --------------------------------------------------------
  # If we have boxplots, use different data frame structure
  # --------------------------------------------------------
  if (type=="boxplots" || type=="violin") {
    if (is.null(weightBy)) {
      w <- 1
    }
    else {
      w <- weightBy
    }
    if (is.null(interactionVar)) {
      mydat <- na.omit(data.frame(cbind(group=x, frq=y, wb=w)))
    }
    else {
      mydat <- na.omit(data.frame(cbind(group=x, frq=y, ia=interactionVar, wb=w)))
      mydat$ia <- as.factor(mydat$ia)
    }
    mydat$group <- as.factor(mydat$group)
  }

  
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  mannwhitneyu <-function(count, grp) {
    if (min(na.omit(grp))==0) {
      grp <- grp+1
    }
    completeString <- c("")
    cnt <- length(unique(na.omit(grp)))
    for (i in 1:cnt) {
      for (j in i:cnt) {
        if (i!=j) {
          xsub <- count[which(grp==i | grp==j)]
          ysub <- grp[which(grp==i | grp==j)]
          ysub <- ysub[which(!is.na(xsub))]
          xsub <- as.numeric(na.omit(xsub))
          ysub <- as.numeric(na.omit(ysub))
          wt <- wilcox.test(xsub ~ ysub)
          modsum <- as.character(as.expression(
            substitute(p[pgrp] == pval, list(pgrp=sprintf("(%i|%i)", i, j),
                                        pval=sprintf("%.3f", wt$p.value)))))
          completeString <- sprintf("%s * \",\" ~ ~ %s", 
                                    completeString, 
                                    modsum)
        }
      }
    }
    return (paste("\"Mann-Whitney-U:\" ~ ~ ", substring(completeString, 12), sep=""))
    # return (paste("Mann-Whitney-U", completeString, sep=""))
    # return (substring(completeString, 12))
  }
  if (showTableSummary) {
    if (type=="boxplots" || type=="violin") {
      modsum <- mannwhitneyu(y, x)
    }
    else {
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
  }  
  
  
  # --------------------------------------------------------
  # If we have a histogram, caluclate means of groups
  # --------------------------------------------------------
  if (type=="histogram") {
    # retrieve all unique factor levels
    faclvl <- unique(na.omit(x))
    # order factors
    faclvl <- faclvl[order(faclvl)]
    # create new data frame for the geom object that prints the
    # vertical line
    vldat <- NULL
    # convert table to df
    ftabdf <- as.data.frame(apply(ftab, MARGIN=2, function(x) cbind(x)))
    colvalues <- as.numeric(attr(ftab, "dimnames")[[1]])
    ftabdf$fac <- colvalues
    # iterate all unique categories, so we can calculate
    # mean of "y" in each group
    for (f in 1:length(faclvl)) {
      # get mean from each group
      # m <- mean(na.omit(y[which(x==faclvl[f])]))
      m <- sum(ftabdf[,f]*ftabdf$fac) / sum(ftabdf[,f])
      # get standard deviation from each group
      stdv <- sd(na.omit(y[which(x==faclvl[f])]))
      # add new row with group and associated mean
      vldat <- as.data.frame(rbind(vldat, c(faclvl[f], m, stdv, yfactor=f)))
    }
    # add row names
    names(vldat) <- c("group", "mw", "stddev", "yfactor")
    # convert group to factor
    vldat$group <- as.factor(vldat$group)
  }
  

  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as parameter
  if (is.null(legendLabels)) {
    # if not, use category text of group variable as legend text
    legendLabels <- c(dfgrp$Var1)
  }
  # wrap legend text lines
  pattern <- c(paste('(.{1,', breakLegendLabelsAt, '})(\\s|$)', sep=""))
  legendLabels <- gsub(pattern, '\\1\n', legendLabels)
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
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    axisTitle.x <- gsub(pattern, '\\1\n', axisTitle.x)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.y)) {
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    axisTitle.y <- gsub(pattern, '\\1\n', axisTitle.y)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.x)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels.x))
      axisLabels.x[n] <- gsub(pattern, '\\1\n', axisLabels.x[n])
  }
  # If axisLabels.x were not defined, simply set numbers from 1 to
  # amount of categories (=number of rows) in dataframe instead
  else  {
    axisLabels.x <- c(1:catcount)
  }
  # check length of x-axis-labels of interaction variable and split 
  # longer strings into new lines
  if (!is.null(interactionVar)) {
    if (!is.null(interactionVarLabels)) {
      pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
      for (n in 1:length(interactionVarLabels))
        interactionVarLabels[n] <- gsub(pattern, '\\1\n', interactionVarLabels[n])
    }
    # If interaction-variable-labels were not defined, simply set numbers from 1 to
    # amount of categories instead
    else  {
      iavarLabLength <- length(unique(na.omit(interactionVar)))
      interactionVarLabels <- c(1:iavarLabLength)
    }
  }
  # If missings are not removed, add an
  # "NA" to labels and a new row to data frame which contains the missings
  if (!omitNA) {
    axisLabels.x = c(axisLabels.x, "NA")
  }
  # --------------------------------------------------------
  # add group counts to category labels
  # --------------------------------------------------------
  if (showGroupCount) {
    nas <- ifelse(omitNA==TRUE, "ifany", "no")
    # check whether we have interaction variables or not
    if (!is.null(interactionVarLabels)) {
      # retrieve group counts by converting data column
      # into table
      if (is.null(weightBy)) {
        gc <- table(x, interactionVar, useNA=nas)
      }
      else {
        gc <- table(weightby(x, weightBy), interactionVar, useNA=nas)
      }
      # determinte loop-steps
      lst <- length(interactionVarLabels)
      # iterate category labels
      for (i in 1:lst) {
        # remember original label
        ial <- interactionVarLabels[i]
        # add group count to each cat. label
        interactionVarLabels[i] <- paste(ial, " (n=", gc[1,i], ")", sep="")
        interactionVarLabels[i+lst] <- paste(ial, " (n=", gc[2,i], ")", sep="")
      }
    }
    else {
      sums <- colSums(ftab)
      # iterate category labels
      for (i in 1:length(sums)) {
        # add group count to each cat. label
        axisLabels.x[i] <- paste(axisLabels.x[i], " (n=", sums[i], ")", sep="")
      }
    }
  }
  
  
  # --------------------------------------------------------
  # Prepare bar charts
  # --------------------------------------------------------
  trimViolin <- FALSE
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  if (!is.null(upperYlim)) {
    upper_lim <- upperYlim
  }
  else {
    # if we have boxplots, we have different ranges, so we can adjust
    # the y axis
    if (type=="boxplots" || type=="violin") {
      # use an extra standard-deviation as limits for the y-axis when we have boxplots
      lower_lim <- min(na.omit(y)) - floor(sd(na.omit(y)))
      upper_lim <- max(na.omit(y)) + ceiling(sd(na.omit(y)))
      # make sure that the y-axis is not below zero
      if (lower_lim < 0) {
        lower_lim <- 0
        trimViolin <- TRUE
      }
    }
    # else calculate upper y-axis-range depending
    # on the amount of cases...
    else if (maxYlim || barPosition=="stack") {
      upper_lim <- basisYlim(length(y))
    }
    else {
      # ... or the amount of max. answers per category
      upper_lim <- freqYlim(mydat$frq)
    }
  }

  
  # --------------------------------------------------------
  # define bar colors
  # --------------------------------------------------------
  # define vertical position for labels
  if (flipCoordinates) {
    # if we flip coordinates, we have to use other parameters
    # than for the default layout
    vert <- ifelse(type == "dots", 0.45, 0.35)
    hort <- -0.2
  }
  else {
    hort <- waiver()
    if (barPosition=="stack") {
      vert <- waiver()
    }
    else if (showPercentageValues) {
      # value labels need a different vertical adjustement, depending on
      # whether we plot dots or bars
      vert <- ifelse(type == "dots", -0.5, -0.2)
    }
    else {
      vert <- ifelse(type == "dots", -0.9, -0.5)
    }
  }
  # align dodged position of labels to bar positions
  posdodge <- ifelse(type=="lines", 0, barWidth + barSpace)
  # --------------------------------------------------------
  # check whether bars should have an outline
  # --------------------------------------------------------
  if (!barOutline) {
    outlineColor <- waiver()
  }
  # init shaded rectangles for plot
  ganno <- NULL
  # check whether we have dots or bars
  if (type=="dots") {
    # position_dodge displays dots in a dodged position so we avoid overlay here. This may lead
    # to a more difficult distinction of group belongings, since the dots are "horizontally spread"
    # over the digram. For a better overview, we can add a "PlotAnnotation" (see "showPlotAnnotation) here.
    geob <- geom_point(position=position_dodge(0.8), size=dotSize, shape=21)
    # create shaded rectangle, so we know which dots belong to the same category
    if (showPlotAnnotation) {
      ganno <- annotate("rect", xmin=mydat$layer-0.4, xmax=mydat$layer+0.4, ymin=0, ymax=c(upper_lim), fill="grey80", alpha=0.1)
    }
  }
  else if (type=="bars") {
    if (barPosition=="dodge") {
      geob <- geom_bar(stat="identity", position=position_dodge(barWidth+barSpace), colour=outlineColor, width=barWidth, alpha=barAlpha)
    }
    else {
      geob <- geom_bar(stat="identity", position="stack", colour=outlineColor, width=barWidth, alpha=barAlpha)
    }
  }
  else if (type=="lines") {
    if (smoothLines) {
      geob <- geom_line(data=mydat, aes(x=as.numeric(count), y=frq, colour=group), linetype=lineType, alpha=lineAlpha, size=lineSize, stat="smooth")
    }
    else {
      geob <- geom_line(data=mydat, aes(x=as.numeric(count), y=frq, colour=group), linetype=lineType, alpha=lineAlpha, size=lineSize)
    }
  }
  else if (type=="boxplots") {
    geob <- geom_boxplot(colour=outlineColor, width=barWidth, alpha=barAlpha)
  }
  else if (type=="violin") {
    geob <- geom_violin(colour=outlineColor, width=barWidth, alpha=barAlpha, trim=trimViolin)
  }
  else {
    geob <- geom_histogram(stat="identity", binwidth=barWidth, position=barPosition, alpha=barAlpha)
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
  if (!showAxisLabels.x) {
    axisLabels.x <- c("")
  }
  # --------------------------------------------------------
  # Hide or show Legend
  # --------------------------------------------------------
  if (hideLegend) {
    # remove guide / legend
    gguide <- guides(fill=FALSE)
  }
  else {
    # show guide with group fill colors
    gguide <- guides(fill=mydat$grp)  
  }

  
  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  if (is.null(barColor)) {
    scalecolors <- scale_fill_brewer(labels=legendLabels, palette="Set1")
    scalecolorsline <- scale_colour_brewer(labels=legendLabels, palette="Set1")
    scalecolorvline <- scale_colour_brewer(palette="Set1")
  }
  else if (barColor=="gs") {
    scalecolors <- scale_fill_grey(labels=legendLabels)
    scalecolorsline <- scale_colour_grey(labels=legendLabels)
    scalecolorvline <- scale_colour_grey()
  }
  else if (barColor=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "barColor"
    scalecolors <- scale_fill_brewer(palette=colorPalette, labels=legendLabels)
    scalecolorsline <- scale_colour_brewer(palette=colorPalette, labels=legendLabels)
    scalecolorvline <- scale_colour_brewer(palette=colorPalette)
  }
  else if (barColor=="bw") {
    barColor <- rep("white", length(legendLabels))
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
    scalecolorsline <- scale_colour_manual(values=barColor, labels=legendLabels)
    scalecolorvline <- scale_colour_manual(values=barColor)
  }
  else {
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
    scalecolorsline <- scale_colour_manual(values=barColor, labels=legendLabels)
    scalecolorvline <- scale_colour_manual(values=barColor)
  }
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  # don't display value labels when we have boxplots or violin plots
  if (type=="boxplots" || type=="violin") {
    showValueLabels <- FALSE
  }
  if (showValueLabels) {
    # if we have facet grids, we have different x and y positions for the value labels
    # so we need to take this into account here
    if (useFacetGrid) {
      # if we want percentage values, we have different sprintf-parameters
      if (showPercentageValues) {
        ggvaluelabels <-  geom_text(aes(x=count, y=frq, label=sprintf("%i\n(%.01f%%)", frq, prz), group=group),
                                    size=valueLabelSize,
                                    vjust=vert,
                                    colour=valueLabelColor)
      }
      else {
        ggvaluelabels <-  geom_text(aes(x=count, y=frq, label=sprintf("%i", frq), group=group),
                                    size=valueLabelSize,
                                    vjust=vert,
                                    colour=valueLabelColor)
      }
    }
    else {
      # if we have stacked bars, we need to apply this stacked y-position to the labels as well
      if (barPosition=="stack") {
        if (showPercentageValues) {
          ggvaluelabels <-  geom_text(aes(y=ypos, label=sprintf("%i\n(%.01f%%)", frq, prz)),
                                      size=valueLabelSize,
                                      vjust=vert,
                                      colour=valueLabelColor)
        }
        else {
          ggvaluelabels <-  geom_text(aes(y=ypos, label=sprintf("%i", frq)),
                                      size=valueLabelSize,
                                      vjust=vert,
                                      colour=valueLabelColor)
        }
      }
      else {
        # if we have dodged bars or dots, we have to use a slightly dodged position for labels
        # as well, sofor better reading
        if (showPercentageValues) {
          if (flipCoordinates) {
            ggvaluelabels <-  geom_text(aes(y=frq, label=sprintf("%i (%.01f%%)", frq, prz)),
                                        size=valueLabelSize,
                                        position=position_dodge(posdodge),
                                        vjust=vert,
                                        hjust=hort,
                                        colour=valueLabelColor)
          }
          else {
            ggvaluelabels <-  geom_text(aes(y=frq, label=sprintf("%i\n(%.01f%%)", frq, prz)),
                                        size=valueLabelSize,
                                        position=position_dodge(posdodge),
                                        vjust=vert,
                                        hjust=hort,
                                        colour=valueLabelColor)
          }
        }
        else {
          ggvaluelabels <-  geom_text(aes(y=frq, label=sprintf("%i", frq)),
                                      position=position_dodge(posdodge),
                                      size=valueLabelSize,
                                      hjust=hort,
                                      vjust=vert,
                                      colour=valueLabelColor)
        }
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
  # ----------------------------------
  if (type=="histogram") {
    mydat$count <- as.numeric(as.character(mydat$count))
    baseplot <- ggplot(mydat, aes(x=count, y=frq, fill=group))
    scalex <- scale_x_continuous(limits=c(catmin, catcount))
  }
  else if (type=="boxplots" || type=="violin") {
    require(quantreg)
    if (is.null(interactionVar)) {
      baseplot <- ggplot(mydat, aes(x=group, y=frq, fill=group, weight=wb))
      scalex <- scale_x_discrete(labels=axisLabels.x)
    }
    else {
      baseplot <- ggplot(mydat, aes(x=interaction(ia, group), y=frq, fill=group, weight=wb))
      scalex <- scale_x_discrete(labels=interactionVarLabels)
    }
  }
  else {
    baseplot <- ggplot(mydat, aes(x=factor(count), y=frq, fill=group))
    scalex <- scale_x_discrete(labels=axisLabels.x)
  }
  # check whether we have dots plotted, and if so, use annotation
  # We have to use annotation first, because the diagram's layers are plotted
  # in the order as they're passed to the ggplot-command. Since we don't want the
  # shaded rectangles to overlay the dots, we add them first
  if (!is.null(ganno) && !useFacetGrid) {
    baseplot <- baseplot + ganno
  }
  baseplot <- baseplot +
    # plot bar chart
    geob
  # if we have line diagram, print lines here
  if (type=="lines") {
    baseplot <- baseplot + 
      geom_point(size=dotSize, alpha=lineAlpha, shape=21, show_guide=FALSE)
  }
  # if we have a histogram, add mean-lines
  if (type=="histogram" && showMeanIntercept) {
    baseplot <- baseplot + 
      # vertical lines indicating the mean
      geom_vline(data=vldat, aes(xintercept=mw, colour=group), linetype=meanInterceptLineType, size=meanInterceptLineSize) +
      # we need scale_colour instead of scale_fill for the lines
      scalecolorvline
    # check whether meanvalue should be shown.
    if (showMeanValue) {
      baseplot <- baseplot + 
        # use annotation instead of geomtext, because we need mean value only printed once
        annotate("text", x=vldat$mw, y=upper_lim, parse=TRUE, label=sprintf("italic(bar(x)[%i]) == %.2f", vldat$yfactor, vldat$mw), size=valueLabelSize, colour=valueLabelColor, hjust=1.05, vjust=vldat$yfactor*2)
    }
    # check whether the user wants to plot standard deviation area
    if (showStandardDeviation) {
      baseplot <- baseplot +
        # first draw shaded rectangle. these are by default in grey colour with very high transparancy
        annotate("rect", xmin=vldat$mw-vldat$stddev, xmax=vldat$mw+vldat$stddev, fill="grey50", ymin=0, ymax=c(upper_lim), alpha=0.1) +
        # draw border-lines for shaded rectangles in the related group colours.
        geom_vline(data=vldat, aes(xintercept=mw-stddev, colour=group), linetype=3, size=meanInterceptLineSize, alpha=0.7) +
        geom_vline(data=vldat, aes(xintercept=mw+stddev, colour=group), linetype=3, size=meanInterceptLineSize, alpha=0.7)
      # if mean values are plotted, plot standard deviation values as well
      if (showMeanValue) {
        baseplot <- baseplot + 
          # use annotation instead of geomtext, because we need standard deviations only printed once
          annotate("text", x=vldat$mw+vldat$stddev, y=upper_lim, parse=TRUE, label=sprintf("italic(s[%i]) == %.2f", vldat$yfactor, round(vldat$stddev,1)), size=valueLabelSize, colour=valueLabelColor, hjust=1.1, vjust=vldat$yfactor*2)
      }
    }
  }
  # if we have a violin plot, add an additional boxplot inside to show
  # more information
  if (type=="violin") {
    baseplot <- baseplot +
      geom_boxplot(width=innerBoxPlotWidth, fill="white", outlier.colour=NA)
  }
  # if we have boxplots or violon plots, also add a point that indicates
  # the mean value
  if (type=="boxplots" || type=="violin") {
    # different fill colours, because violin boxplots have white background
    fcsp <- ifelse(type=="boxplots", "white", "black")
    baseplot <- baseplot +
      stat_summary(fun.y="mean", geom="point", shape=21, size=innerBoxPlotDotSize, fill=fcsp)
  }
  # If we have bars or dot plots, we show Pearson's chi-square test results
  if (type=="bars" || type=="dots" || type=="lines" || type=="boxplots" || type=="violin") {
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
  }
  # continue with plot objects...
  baseplot <- baseplot +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    # coord_cartesian(ylim=c(0, upper_lim)) +
    scale_y_continuous(breaks=gridbreaks, limits=c(lower_lim, upper_lim), expand=c(0,0)) +
    # guide / legend
    gguide +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title=title, x=axisTitle.x, y=axisTitle.y, fill=legendTitle) +
    # print value labels to the x-axis.
    # If parameter "axisLabels.x" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scalex
  # when we have lines, we additionally need to apply "scale_colour"...
  if (type=="lines") {
    baseplot <- baseplot + scalecolorsline
  }
  baseplot <- baseplot + 
    scalecolors +
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
  if (useFacetGrid) {
    # --------------------------------------------------
    # Here we start when we have a faces grid instead of
    # a grouped bar plot.
    # --------------------------------------------------
      # set font size for axes.
    baseplot <- baseplot + 
      theme(strip.text = element_text(face="bold",size=rel(1.2))) +
      facet_wrap( ~ group)
  }
  # ----------------------------------
  # Plot integrated bar chart here
  # ----------------------------------
  plot(baseplot)
}


# Berechnet die aufgerundete Obergrenze der y-Achse anhand
# der maximal möglichen Fallzahl einer Antwortmöglichkeit
# Dadurch werden Balkendiagramme eines Datensatzes immer im
# gleichen Vergältnis dargestellt, da die y-Achse nie variiert,
# sondern immer von 0 bis (Anzahl der Fälle) geht.
#
# Parameter:
# - len: die Anzahl an max. möglichen Fällen
basisYlim <- function(len) {
  anzahl <- 1
  while (len>=(10*anzahl)) {
    anzahl <- anzahl * 10
  }
  
  while(len>=anzahl) {
    anzahl <- anzahl + round(anzahl/10,0)
  }
  
  #  retval <- (ceiling(len/anzahl)*anzahl)
  #  return (retval)
  return (anzahl)
}

# Berechnet die aufgerundete Obergrenze der y-Achse anhand
# des höchsten Datenwertes einer Antwortmöglichkeit.
# Dadurch werden Balkendiagramme eines Datensatzes immer unterschiedlich
# dargestellt, je nach Anzahl der häufigsten Antworten. Die y-Achse
# geht immer von 0 bis (maximale Antworthäufigkeit einer Variable)
#
# Parameter:
# - var: die Variable mit den Antwortmöglichkeiten
freqYlim <- function(var) {
  # suche die Antwort mit den häufigsten Antworten,
  # also den höchsten Wert einer Variablenausprägung
  len <- max(var)
  
  if (len<100) {
    anzahl <- 10
  }
  else {
    anzahl <- 100
  }
  
  li <- ceiling(len/anzahl)
  if ((li %% 2) == 1) {
    li <- li+1
  }
  
  retval <- li*anzahl
  return (retval)
}

convertToLabel <- function(variable) {
  vl <- rev(names(attr(variable, "value.labels")))
  vn <- sort(unique(na.omit(variable)))
  
  for (i in 1:length(vl)) {
    variable[variable==vn[i]] <- vl[i]
  }
  return (variable)
}
