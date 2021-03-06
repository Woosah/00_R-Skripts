#' @name sjPlotFrequencies.R (V1.9)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' 
#' @description Plot frequencies of a variables as ggplot diagram
#' See http://strengejacke.wordpress.com/2013/02/25/simplify-frequency-plots-with-ggplot-in-r-rstats/
#' for further details.
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param y the variable which frequencies should be plotted
#' @param weightBy a weight factor that will be applied to weight all cases from \code{y}
#' @param weightByTitleString if a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: weightByTitleString=" (weighted)"
#' @param maxYlim indicates how to calculate the maximum limit of the y-axis.
#'          If TRUE, the upper y-limit corresponds to the amount of cases,
#'          i.e. y-axis for each plot of a data base are the same.
#'          If FALSE, the maximum y-axis depends on the highest count of a
#'          variable's answer category. In this case, the y-axis breaks may change,
#'          depending on the variable.
#' @param upperYlim Uses a pre-defined upper limit for the y-axis. Overrides the maxYlim parameter.
#' @param order determines whether categories on x-axis should be order according to the frequencies or not. 
#'          Default is \code{"none"}, so categories are not ordered by frequency. Use \code{"asc"} or
#'          \code{"desc"} for sorting categories ascending or descending in relation to the frequencies.
#' @param type specifies the type of distribution plot that will be plotted.
#'          \code{"bar"}, \code{"bars"} or \code{"b"} for simple bars (the default setting).
#'          \code{"h"}, \code{"hist"} or \code{"histogram"} for a histogram.
#'          \code{"line"}, \code{"lines"} or \code{"l"} for a histogram with filled area with line.
#'          \code{"dens"}, \code{"d"} or \code{"density"} for a density plot.
#'          \code{"box"}, \code{"boxplot"} or \code{"boxplots"} for box plots.
#'          \code{"v"} or \code{"violin"} for violin
#' @param title Title of diagram as string.
#'          Example: title=c("my title")
#' @param axisLabels.x Labels for the x-axis breaks
#'          Example: axisLabels.x=c("Label1", "Label2", "Label3").
#'          Note: If you use the \code{sjImportSPSS.R} script with the \code{getValueLabels} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param axisLabelAngle.x angle for axis-labels
#' @param axisLabelSize The size of axis labels of both x and y axis. Default is 1.1, recommended values range
#'          between 0.5 and 3.0
#' @param valueLabelSize The size of value labels in the diagram. Default is 4, recommended values range
#'          between 2 and 8
#' @param breakTitleAt Determines how many chars of the title are displayed in 
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Determines how many chars of the title are displayed in 
#'          one line and when a line break is inserted into the title
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed
#' @param barWidth Width of bars. Default is 0.6, recommended values range from 0.2 to 2.0
#' @param innerBoxPlotWidth the width of the inner box plot that is plotted inside of violin plots. Only applies 
#'          if \code{type} is \code{"violin"}. Default value is 0.15
#' @param innerBoxPlotDotSize size of mean dot insie a violin plot. Applies only when \code{type} is set to \code{violin}
#' @param barColor user defined color for bars. If not specified, a default blue
#'          color palette will be used for the bar charts
#' @param barAlpha specify the transparancy (alpha value) of bars
#' @param axisLabelColor user defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param barOutline If TRUE, each bar gets a colored outline
#' @param outlineColor The color of the bar outline. Only applies, if barOutline is set to TRUE
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar
#' @param showPercentageValues TRUE if percentage values should be plotted to each bar, if FALSE % values are removed
#' @param showAxisLabels.x Whether x axis text (category names) should be shown or not
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showMeanIntercept if TRUE, a vertical line in histograms is drawn to indicate the mean value of the count
#'          variables. Only applies to histogram-charts.
#' @param showMeanValue if TRUE (default value), the mean value is printed to the vertical line that indicates the mean value
#'          of the count variables. Only applies to histogram-charts.
#' @param showStandardDeviation if \code{TRUE}, the standard deviation is annotated as shaded rectangle around the mean intercept
#'          line. Only applies to histogram-charts.
#' @param meanInterceptLineType the linetype of the mean intercept line. Only applies to histogram-charts and when showMeanIntercept is TRUE.
#' @param meanInterceptLineSize the size of the mean intercept line. Only applies to histogram-charts and when showMeanIntercept is TRUE.
#' @param showNormalCurve if \code{TRUE}, a normal curve is plotted over the histogram. Default is
#'          \code{FALSE}. Only applies when histrograms are plotted.
#' @param normalCurveColor specify the color of the normal curve line. Only
#'          applies if \code{showNormalCurve} is \code{TRUE}.
#' @param normalCurveSize specifiy the size of the normal curve line. Only
#'          applies if \code{showNormalCurve} is \code{TRUE}.
#' @param normalCurveAlpha specify the transparancy (alpha value) of the normal curve. Only
#'          applies if \code{showNormalCurve} is \code{TRUE}.
#' @param valueLabelColor the color of the value labels (numbers) inside the digram
#' @param axisTitle.x a label for the x axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#' @param axisTitle.y a label for the y axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param axisTitleColor the color of the x and y axis labels. refers to axisTitle.x and axisTitle.y, not to the tick mark 
#'          or category labels.
#' @param axisTitleSize the size of the x and y axis labels. refers to axisTitle.x and axisTitle.y, not to the tick mark 
#'          or category labels.
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#' @param omitNA If TRUE, missings are not included in the frequency calculation and diagram plot
#' 
#' @examples
# sjp.frq(ChickWeight$weight, type="box")
# sjp.frq(discoveries, type="hist", showMeanIntercept=TRUE)
# sjp.frq(ChickWeight$weight, type="v")
# sjp.frq(ChickWeight$Diet)


sjp.frq <- function(y, 
                   title=NULL, 
                   weightBy=NULL,
                   weightByTitleString=NULL,
                   maxYlim=TRUE, 
                   upperYlim=NULL,
                   order="none",
                   type="bars",
                   axisLabels.x=NULL, 
                   axisLabelAngle.x=0, 
                   axisLabelSize=1.1,
                   axisLabelColor="darkgray", 
                   valueLabelSize=4,
                   valueLabelColor="black",
                   breakTitleAt=50, 
                   breakLabelsAt=12, 
                   gridBreaksAt=NULL,
                   barWidth=0.6,
                   barColor=NULL, 
                   barAlpha=1,
                   barOutline=TRUE, 
                   barOutlineSize=0.2,
                   innerBoxPlotWidth=0.15,
                   innerBoxPlotDotSize=3,
                   borderColor=NULL, 
                   axisColor=NULL,
                   outlineColor="black",
                   majorGridColor=NULL,
                   minorGridColor=NULL,
                   showValueLabels=TRUE,
                   showPercentageValues=TRUE,
                   showAxisLabels.x=TRUE,
                   showTickMarks=TRUE,
                   showMeanIntercept=FALSE,
                   showMeanValue=TRUE,
                   showStandardDeviation=TRUE,
                   showNormalCurve=FALSE,
                   meanInterceptLineType=2,
                   meanInterceptLineSize=0.5,
                   normalCurveColor="red",
                   normalCurveSize=0.7,
                   normalCurveAlpha=0.4,
                   axisTitle.x=NULL,
                   axisTitle.y=NULL,
                   axisTitleColor="black",
                   axisTitleSize=1.3,
                   theme=NULL,
                   flipCoordinates=FALSE,
                   omitNA=TRUE) {

  # --------------------------------------------------------
  # count variable may not be a factor!
  # --------------------------------------------------------
  if (is.factor(y)) {
    y <- as.numeric(as.character(y))
  }

  
  # --------------------------------------------------------
  # We have several options to name the histrogram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type=="b" || type=="bar") {
    type <- c("bars")
  }
  if (type=="h" || type=="hist") {
    type <- c("histogram")
  }
  if (type=="d" || type=="density") {
    type <- c("dens")
  }
  if (type=="l" || type=="lines") {
    type <- c("line")
  }
  if (type=="box" || type=="boxplot") {
    type <- c("boxplots")
  }
  if (type=="v") {
    type <- c("violin")
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
  if (!is.null(weightBy)) {
    y <- weightby(y, weightBy)
  }
  df <- as.data.frame(table(y))
  names(df) <- c("y", "Freq")
  
  
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

  
  # --------------------------------------------------------
  # Define amount of category, include zero counts
  # --------------------------------------------------------
  # Zero counts of categories are not plotted by default just becaus
  # these categories don't appear in the data. If we assume a
  # "quasi-continuous" scale (categories from 1 to 4 etc.), we now
  # identify the zero counts and add / insert them into the data frame.
  # This enables us to plot zero counts as well.
  # We guess the maximum amount of categories either by the amount
  # of supplied category labels. If no category labels were passed
  # as parameter, we assume that the maximum value found in the category
  # columns represents the highest category number
  catcount <- 0
  lower_lim <- 0
  catmin <- min(na.omit(y))
  # Factors have to be transformed into numeric values
  # for continiuos x-axis-scale
  df$y <- as.numeric(as.character(df$y))
  # if categories start with zero, fix this here
  if (min(df$y)==0) {
    df$y <- df$y+1
  }
  # get the highest answer category of "y", so we know where the
  # range of the x-axis ends
  if (!is.null(axisLabels.x)) {
    catcount <- length(axisLabels.x)
  }
  else {
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
  }
  # Create a vector of zeros 
  frq <- rep(0,catcount)
  # Replace the values in freq for those indices which equal dummyf$xa
  # by dummyf$ya so that remaining indices are ones which you 
  # intended to insert 
  frq[df$y] <- df$Freq
  # create new data frame. We now have a data frame with all
  # variable categories abd their related counts, including
  # zero counts, but no(!) missings!
  mydat <- as.data.frame(cbind(var=1:catcount, frq))
  # caculate missings here
  missingcount <- length(which(is.na(y)))
  # --------------------------------------------------------
  
  
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
    axisLabels.x <- c(1:nrow(mydat))
  }
  # --------------------------------------------------------
  
  
  # --------------------------------------------------------
  # Handle missings
  # --------------------------------------------------------
  # If missings are not removed, add an
  # "NA" to labels and a new row to data frame which contains the missings
  if (!omitNA) {
    axisLabels.x = c(axisLabels.x, "NA")
    mydat <- rbind(mydat, c(catcount+1, missingcount))
    # also add a columns with percentage values of count distribution
    mydat <- data.frame(cbind(mydat, prz = c(round(100*mydat$frq/length(y),2))))
  }
  else {
    # also add a columns with percentage values of count distribution
    mydat <- data.frame(cbind(mydat, prz = c(round(100*mydat$frq/length(na.omit(y)),2))))
  }
  # --------------------------------------------------------
  
  
  # --------------------------------------------------------
  # Order categories ascending or descending
  # --------------------------------------------------------
  if (order=="asc" || order=="desc") {
    ord <- order(mydat$frq, decreasing=(order=="desc"))
    mydat$frq <- mydat$frq[ord]
    mydat$prz <- mydat$prz[ord]
    axisLabels.x <- axisLabels.x[ord]
  }
  
  
  # --------------------------------------------------------
  # If we have a histogram, caluclate means of groups
  # --------------------------------------------------------
  mittelwert <- mean(y, na.rm=TRUE)
  stddev <- sd(y, na.rm=TRUE)
  
  
  # --------------------------------------------------------
  # If we have boxplots, use different data frame structure
  # --------------------------------------------------------
  if (type=="boxplots" || type=="violin") {
    mydat <- na.omit(data.frame(cbind(grp=1, frq=y, var=y)))
    mydat$grp <- as.factor(mydat$grp)
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
    # in case we have a histrogram, calculate
    # max. y lim depending on highest value
    if (type!="bars") {
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
      else {
        # ... or the amount of max. answers per category
        upper_lim <- histYlim(y)
      }
    }
    else {
      # else calculate upper y-axis-range depending
      # on the amount of cases...
      if (maxYlim) {
        upper_lim <- basisYlim(length(y))
      }
      else {
        # ... or the amount of max. answers per category
        upper_lim <- freqYlim(catcount)
      }
    }
  }
  require(ggplot2)
  # --------------------------------------------------------
  # check whether bars should have an outline
  # --------------------------------------------------------
  if (!barOutline) {
    outlineColor <- waiver()
  }
  # --------------------------------------------------------
  # define bar colors
  # --------------------------------------------------------
  # check whether barcolor is defined
  if (is.null(barColor)) {
    geob <- geom_bar(stat="identity", colour=outlineColor, width=barWidth, alpha=barAlpha)
    # set default color for histograms
    barColor <- c("#4080c0")
  }
  else {
    # continue here, if barcolor is defined.
    geob <- geom_bar(stat="identity", fill=barColor, colour=outlineColor, width=barWidth, alpha=barAlpha)
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
  # Set value labels
  # --------------------------------------------------------
  if (flipCoordinates) {
    # adjust vertical position for labels, based on whether percentage values
    # are shown or not
    vert <- ifelse(showPercentageValues == TRUE, 0.5, 0.1)
    hort <- 1.5
  }
  else {
    # adjust vertical position for labels, based on whether percentage values
    # are shown or not
    vert <- ifelse(showPercentageValues == TRUE, -0.2, -0.6)
    hort <- waiver()
  }
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  # don't display value labels when we have boxplots or violin plots
  if (type=="boxplots" || type=="violin") {
    showValueLabels <- FALSE
  }
  if (showValueLabels) {
    # here we have counts and percentages
    if (showPercentageValues) {
      ggvaluelabels <-  geom_text(label=sprintf("%i\n(%.01f%%)", mydat$frq, mydat$prz),
                                  size=valueLabelSize,
                                  vjust=vert,
                                  hjust = hort,
                                  colour=valueLabelColor)
    }
    else {
      # here we have counts, without percentages
      ggvaluelabels <-  geom_text(label=sprintf("%i", mydat$frq),
                                  hjust = hort,
                                  size=valueLabelSize,
                                  vjust=vert,
                                  colour=valueLabelColor)
    }
  }
  else {
    # no labels
    ggvaluelabels <-  geom_text(label="")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  maxx <- max(mydat$var) + 1
  if (is.null(gridBreaksAt)) {
    gridbreaks <- waiver()
    histgridbreaks <- waiver()
  }
  else {
    gridbreaks <- c(seq(lower_lim, upper_lim, by=gridBreaksAt))
    histgridbreaks <- c(seq(lower_lim, maxx, by=gridBreaksAt))
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
  # ----------------------------------
  # set y scaling and label texts
  # ----------------------------------
  # set Y-axis, depending on the calculated upper y-range.
  # It either corresponds to the maximum amount of cases in the data set
  # (length of var) or to the highest count of var's categories.
  yscale <- scale_y_continuous(limits=c(lower_lim, upper_lim), expand=c(0,0), breaks=gridbreaks)
  # ----------------------------------
  # Print plot
  # ----------------------------------
  # Check how many categories we have on the x-axis.
  # If it exceeds the user defined limits, plot
  # histrogram instead of bar chart
  # ----------------------------------
  if (type=="bars") {
    # mydat is a data frame that only contains one variable (var).
    # Must be declared as factor, so the bars are central aligned to
    # each x-axis-break. 
    baseplot <- ggplot(mydat, aes(x=factor(var), y=frq, fill=var)) + 
      geob +
      yscale + 
      # remove guide / legend
      guides(fill=FALSE) +
      # show absolute and percentage value of each bar.
      ggvaluelabels +
      # print value labels to the x-axis.
      # If parameter "axisLabels.x" is NULL, the category numbers (1 to ...) 
      # appear on the x-axis
      scale_x_discrete(labels=axisLabels.x)
    # check whether coordinates should be flipped, i.e.
    # swap x and y axis
    if (flipCoordinates) {
      baseplot <- baseplot + coord_flip()
    }
  }
  else {
    # --------------------------------------------------
    # Here we start when we have a histogram instead of
    # bar plot.
    # --------------------------------------------------
    # Start density plot here
    # --------------------------------------------------
    if (type=="boxplots" || type=="violin") {
      baseplot <- ggplot(mydat, aes(x=grp, y=frq))
      if (type=="boxplots") {
        baseplot <- baseplot + 
          geom_boxplot(colour=outlineColor, width=barWidth, alpha=barAlpha, fill=barColor)
      }
      else {
        baseplot <- baseplot + 
          geom_violin(colour=outlineColor, width=barWidth, alpha=barAlpha, fill=barColor, trim=trimViolin) +
          # if we have a violin plot, add an additional boxplot inside to show
          # more information
          geom_boxplot(width=innerBoxPlotWidth, fill="white", outlier.colour=NA)
      }
      # if we have boxplots or violon plots, also add a point that indicates
      # the mean value
      # different fill colours, because violin boxplots have white background
      fcsp <- ifelse(type=="boxplots", "white", "black")
      baseplot <- baseplot +
        stat_summary(fun.y="mean", geom="point", shape=21, size=innerBoxPlotDotSize, fill=fcsp)
      # no additional labels for the x- and y-axis, only diagram title
      baseplot <- baseplot + 
        yscale +
        # no need for x-axis-label
        scale_x_discrete(labels="")
    }
    # --------------------------------------------------
    # Start density plot here
    # --------------------------------------------------
    else if (type=="dens") {
        geoh <- geom_histogram(fill=barColor, colour=outlineColor, size=barOutlineSize, alpha=barAlpha)
        densityDat <- data.frame(cbind(y))
        # First, plot histogram with density curve
        baseplot <- ggplot(densityDat, aes(x=y, y=..density..)) +
          geoh +
          # transparent density curve above bars
          geom_density(fill="cornsilk", alpha=0.3) +
          # remove margins from left and right diagram side
          scale_x_continuous(expand=c(0,0), breaks=histgridbreaks)
    }
    else {
      # -----------------------------------------------------------------
      # Since the density curve shows no absolute numbers (counts) on the
      # y-axis, have also the opportunity to plot "real" histrograms with 
      # counts on the y-axis
      # -----------------------------------------------------------------
      basehist <- geom_histogram(stat="identity", fill=barColor, colour=outlineColor, size=barOutlineSize, alpha=barAlpha, binwidth=barWidth)
      basehistline <- geom_area(fill=barColor, alpha=0.3)
      # base constructor
      baseplot <- ggplot(mydat, aes(x=var, y=frq))
      # check whether user wants line or bar histogram
      if (type=="line") {
        baseplot <- baseplot + basehistline + geom_line()
      }
      else {
        baseplot <- baseplot + basehist
      }
      # check whether user wants to overlay the histogram
      # with a normal curve
      if (showNormalCurve) {
        baseplot <- baseplot +
          stat_function(fun=function(x, mean, sd, n) { n*dnorm(x=x, mean=mean, sd=sd) },
                        args=with(mydat, c(mean=mittelwert, sd=stddev, n=length(y))),
                        colour=normalCurveColor,
                        size=normalCurveSize,
                        alpha=normalCurveAlpha)
      }
      # if we have a histogram, add mean-lines
      if (showMeanIntercept) {
        baseplot <- baseplot + 
          # vertical lines indicating the mean
          geom_vline(xintercept=mittelwert, linetype=meanInterceptLineType, size=meanInterceptLineSize)
        # check whether meanvalue should be shown.
        if (showMeanValue) {
          baseplot <- baseplot + 
            # use annotation instead of geomtext, because we need mean value only printed once
            annotate("text", x=mittelwert, y=upper_lim, parse=T, label=paste("italic(bar(x)) == ", "'", c(round(mittelwert,1)), "'"), size=valueLabelSize, colour=valueLabelColor, hjust=1.1, vjust=2.2)
        }
        # check whether the user wants to plot standard deviation area
        if (showStandardDeviation) {
          baseplot <- baseplot +
            # first draw shaded rectangle. these are by default in grey colour with very high transparancy
            annotate("rect", xmin=mittelwert-stddev, xmax=mittelwert+stddev, ymin=0, ymax=c(upper_lim), fill="grey70", alpha=0.2) +
            # draw border-lines for shaded rectangle
            geom_vline(xintercept=mittelwert-stddev, linetype=3, size=meanInterceptLineSize, alpha=0.7) +
            geom_vline(xintercept=mittelwert+stddev, linetype=3, size=meanInterceptLineSize, alpha=0.7)
          # if mean values are plotted, plot standard deviation values as well
          if (showMeanValue) {
            baseplot <- baseplot + 
              # use annotation instead of geomtext, because we need mean value only printed once
              annotate("text", x=mittelwert, y=upper_lim, label=sprintf("italic(s) == %.2f", round(stddev,1)), parse=TRUE, size=valueLabelSize, colour=valueLabelColor, hjust=1.15, vjust=4.2)
          }
        }
      }
      baseplot <- baseplot +
        # show absolute and percentage value of each bar.
        ggvaluelabels +
        # remove margins from left and right diagram side
        scale_x_continuous(limits=c(catmin,maxx), expand=c(0,0), breaks=histgridbreaks) +
        yscale
    }
  }
  # set axes text and 
  baseplot <- baseplot + 
    labs(title=title, x=axisTitle.x, y=axisTitle.y) +
    ggtheme +
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor))
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
  len <- var[which.max(var)]
  
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

histYlim <- function(var) {
  # suche die Antwort mit den häufigsten Antworten,
  # also den höchsten Wert einer Variablenausprägung
  len <- max(table(var))
  
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

# usage:
# df<-insertRowToDF(df,5,c(16,0)); # inserting the values (16,0) after the 5th row
insertRowToDF<-function(X,index_after,vector_to_insert){
  stopifnot(length(vector_to_insert) == ncol(X)) # to check valid row to be inserted
  X<-rbind(X[1:index_after,],vector_to_insert,X[(index_after+1):nrow(X),])
  row.names(X)<-1:nrow(X)
  return (X)
}