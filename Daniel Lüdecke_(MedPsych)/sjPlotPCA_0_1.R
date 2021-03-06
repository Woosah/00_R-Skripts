#' @name sjPlotPCA (V0.1)
#' 
#' @author Daniel Lüdecke (d.luedecke@uke.de)
#' 
#' @description Plot correlations as ellipses or tiles
#' See 
#' 
#' Distributed under the GNU GPL v3 (and higher) license. There’s no warranty that the scripts
#' work 100% correctly! If you find any bugs or have suggestions on how to improve them, please let me know.
#' 
#' @param data a data frame with factors (each columns one variable) that should be used 
#'          to compute a PCA, or a correlation object computed with the R-cor-function
#' @param numberOfFactors a predefined number of factors to use for the calculating the varimax
#'          rotation. By default, this value is \code{NULL} and the amount of factors is
#'          calculated according to the Kaiser-criteria. See paramater \code{plotEigenvalues}.
#' @param factorLoadingTolerance specifies the minimum difference a variable needs to have between
#'          factor loadings (components) in order to indicate a clear loading on just one factor and not
#'          diffusing over all factors. For instance, a variable with 0.8, 0.82 and 0.84 factor loading 
#'          on 3 possible factors can not be clearly assigned to just one factor and thus would be removed
#'          from the principal component analysis. By default, the minimum difference of loading values
#'          between the highest and 2nd highest factor should be 0.1
#' @param plotEigenvalues if \code{TRUE}, a plot showing the Eigenvalues according to the
#'          Kaiser criteria is plotted to determine the number of factors.
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#' @example
# likert_4 <- data.frame(sample(1:4, 500, replace=T, prob=c(0.2,0.3,0.1,0.4)),
#                        sample(1:4, 500, replace=T, prob=c(0.5,0.25,0.15,0.1)),
#                        sample(1:4, 500, replace=T, prob=c(0.4,0.15,0.25,0.2)),
#                        sample(1:4, 500, replace=T, prob=c(0.25,0.1,0.4,0.25)),
#                        sample(1:4, 500, replace=T, prob=c(0.1,0.4,0.4,0.1)),
#                        sample(1:4, 500, replace=T,),
#                        sample(1:4, 500, replace=T, prob=c(0.35,0.25,0.15,0.25)))
# colnames(likert_4) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")
# sjp.pca(likert_4)
# pca <- prcomp(na.omit(likert_4), retx=TRUE, center=TRUE, scale.=TRUE)
# sjp.pca(pca, plotEigen=TRUE, type="circle")
sjp.pca <- function(data,
                    numberOfFactors=NULL,
                    factorLoadingTolerance=0.1,
                    plotEigenvalues=FALSE,
                    title=NULL,
                    axisLabels.y=NULL,
                    type="tile",
                    geomAlpha=0.8,
                    valueLabelColor="black",
                    valueLabelSize=4.5,
                    valueLabelAlpha=1,
                    circleSize=10,
                    outlineColor="black",
                    outlineSize=0.2,
                    axisColor=NULL, 
                    borderColor=NULL, 
                    axisLabelSize=1.1,
                    axisLabelColor="grey30",
                    axisLabelAngle.x=0, 
                    axisLabelAngle.y=0, 
                    breakTitleAt=50, 
                    breakLabelsAt=12, 
                    hideLegend=TRUE,
                    legendTitle=NULL,
                    showValueLabels=TRUE,
                    showTickMarks=FALSE,
                    showCronbachsAlpha=TRUE,
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
  if (class(data)=="prcomp") {
    pcadata <- data
    dataframeparam <- FALSE
  }
  else {
    pcadata <- prcomp(na.omit(data), retx=TRUE, center=TRUE, scale.=TRUE)
    dataframeparam <- TRUE
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
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels.y))
      axisLabels.y[n] <- gsub(pattern, '\\1\n', axisLabels.y[n])
  }

  
  # ----------------------------
  # calculate eigenvalues
  # ----------------------------
  pcadata.eigenval <- pcadata$sdev^2
  # ----------------------------
  # retrieve best amount of factors according
  # to Kaiser-critearia, i.e. factors with eigen value > 1
  # ----------------------------
  pcadata.kaiser <- which(pcadata.eigenval<1)[1]-1
  # ----------------------------
  # plot eigenvalues
  # ----------------------------
  if (plotEigenvalues) {
    # create data frame with eigen values
    mydat <- as.data.frame(cbind(xpos=1:length(pcadata.eigenval), eigen=pcadata.eigenval))
    # plot eigenvalues as line curve
    eigenplot <- 
      # indicate eigen vlaues > 1
      ggplot(mydat, aes(x=xpos, y=eigen, colour=eigen>1)) +
        geom_line() + geom_point() +
        geom_hline(y=1, linetype=2, colour="grey50") +
        # print best number of factors according to eigen value
        annotate("text", label=sprintf("Factors: %i", pcadata.kaiser), x=Inf, y=Inf, vjust=2, hjust=1.2) +
        scale_x_continuous(breaks=c(seq(1,nrow(mydat), by=2))) +
        labs(title=NULL, y="Eigenvalue", x="Number of factors")
    plot(eigenplot)
    # print statistics
    print("--------------------------------------------")
    print(summary(pcadata))
    print("Eigenvalues:")
    print(pcadata.eigenval)
    print("--------------------------------------------")
  }

  
  # --------------------------------------------------------
  # varimax rotation, retrieve factor loadings
  # --------------------------------------------------------
  # check for predefined number of factors
  if (!is.null(numberOfFactors) && is.numeric(numberOfFactors)) {
    pcadata.kaiser <- numberOfFactors
  }
  pcadata.varim = varimaxrota(pcadata, pcadata.kaiser)
  # create data frame with factor loadings
  df <- as.data.frame(pcadata.varim$loadings[,1:ncol(pcadata.varim$loadings)])
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- row.names(df)
  }
  # --------------------------------------------------------
  # this function retrieves a list with the column index ("factor" index)
  # where each case of the data frame has its highedt factor loading.
  # So we know to which "group" (factor dimension) each case of the 
  # data frame belongs to according to the pca results
  # --------------------------------------------------------
  getItemLoadings <- function(dataframe) {
    # clear vector
    itemloading <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in 1:nrow(dataframe)) {
      # get factor loadings for each item
      rowval <- abs(df[i,])
      # retrieve highest loading and remeber that column
      itemloading <- c(itemloading, which(rowval==max(rowval)))
    }
    # return a vector with index numbers indicating which items
    # loads the highest on which factor
    return (itemloading)
  }
  # --------------------------------------------------------
  # this function checks which items have unclear factor loadings,
  # i.e. which items do not strongly load on a single factor but
  # may load almost equally on several factors
  # --------------------------------------------------------
  getRemovableItems <- function(dataframe) {
    # clear vector
    removers <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in 1:nrow(dataframe)) {
      # get factor loadings for each item
      rowval <- abs(df[i,])
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload-max2load)<factorLoadingTolerance) {
        # if difference is below the tolerance,
        # remeber row-ID so we can remove that items
        # for further PCA with updated data frame
        removers <- c(removers, i)
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return (removers)
  }
  # --------------------------------------------------------
  # this function calculates the cronbach's alpha value for
  # each factor scale, i.e. all variables with the highest loading
  # for a factor are taken for the reliability test. The result is
  # an alpha value for each factor dimension
  # --------------------------------------------------------
  getCronbach <- function(dataframe, itemloadings) {
    # clear vector
    cbv <- c()
    CronbachAlpha <- function(X) { # X must be matrix or data.frame with more than 2 columns
      if (is.null(ncol(X)) || ncol(X)<2) {
        print("Too less columns in this factor to calculate alpha value!")
        return(0)
      }
      return (dim(X)[2]/(dim(X)[2]-1)*(1-sum(apply(X,2,var))/var(rowSums(X))))
    }    
    # iterate all highest factor loadings of items
    for (n in 1:length(unique(itemloadings))) {
      # calculate cronbach's alpha for those cases that all have the
      # highest loading on the same factor
      cbv <- as.data.frame(rbind(cbv, cbind(nr=n, CronbachAlpha(dataframe[,which(itemloadings==n)]))))
    }
    # just for vertical position adjustment when we print the alpha values
    vpos <- rep(c(-0.25, -1), nrow(cbv))
    cbv <- cbind(cbv, vpos[1:nrow(cbv)])
    names(cbv) <- c("nr", "alpha", "vpos")
    # cbv now contains the factor numbers and the related alpha values
    # for each "factor dimension scale"
    return(cbv)
  }
  # ----------------------------------
  # Cronbach's Alpha can only be calculated when having a data frame
  # with each component / variable as column
  # ----------------------------------
  if (dataframeparam) {
    # get alpha values
    alphaValues <- getCronbach(data, getItemLoadings(df))
  }
  else {
    print("Cronbach's Alpha can only be calculated when having a data frame with each component / variable as column")
    showCronbachsAlpha <- FALSE
  }
  # retrieve those items that have unclear factor loadings, i.e.
  # which almost load equally on several factors. The tolerance
  # that indicates which difference between factor loadings is
  # considered as "equally" is defined via factorLoadingTolerance
  removableItems <- getRemovableItems(df)
  # rename columns, so we have numbers on x axis
  names(df) <- c(1:ncol(df))
  # convert to long data
  df <- melt(df)
  # we need new columns for y-positions and point sizes
  df <- cbind(df, ypos=c(1:nrow(pcadata.varim$loadings)), psize=c(exp(abs(df$value))*circleSize))
  # rename first column for more intuitive name
  colnames(df)[1] <- c("xpos")
  # df$value <- abs(df$value)

  
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
  if (!showValueLabels) {
    valueLabels <- c("")
  }
  else {
    valueLabels <- c(round(df$value,2))
  }
  
  
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  heatmap <- ggplot(data=df, aes(x=xpos, y=ypos, fill=value))
  # --------------------------------------------------------
  # determine the geom type, either points when "type" is "circles"
  # --------------------------------------------------------
  if (type=="circle") {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geo <- geom_point(shape=21, size=df$psize, alpha=geomAlpha)
    }
    # ... and apply colour-attribute
    else {
      geo <- geom_point(shape=21, size=df$psize, alpha=geomAlpha, colour=outlineColor)
    }
  }
  # --------------------------------------------------------
  # or boxes / tiles when "type" is "tile"
  # --------------------------------------------------------
  else {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geo <- geom_tile()
    }
    # ... and apply colour-attribute
    else {
      geo <- geom_tile(size=outlineSize, colour=outlineColor)
    }
  }
  heatmap <- heatmap +
    geo +
    scale_y_reverse(breaks=c(seq(1, length(axisLabels.y), by=1)), labels=axisLabels.y)
  # fill gradient colour from distinct color brewer palette. negative correlations are dark
  # red, positive corr. are dark blue, and they become lighter the closer they are to a
  # correlation coefficient of zero
  if (is.null(fillColor)) {
    heatmap <- heatmap +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=brewer_pal("div",5)(5), limits=c(-1,1))
      # scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0)
  }
  else {
    heatmap <- heatmap +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=fillColor, limits=c(-1,1))
      # scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0)
  }
  heatmap <- heatmap +
    geom_text(label=valueLabels, colour=valueLabelColor, alpha=valueLabelAlpha, size=valueLabelSize) +
    labs(title=title, x=NULL, y=NULL, fill=legendTitle) +
    ggtheme +
    # set font size for axes.
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          axis.text.y = element_text(angle=axisLabelAngle.y))
  # show cronbach's alpha value for each scale 
  if (showCronbachsAlpha) {
    heatmap <- heatmap +
      # annotate("text", x=alphaValues$nr, y=Inf, parse=TRUE, label=sprintf("alpha == %.2f", alphaValues$alpha), size=0.9*valueLabelSize, colour=axisLabelColor, vjust=alphaValues$vpos)
      annotate("text", x=alphaValues$nr, y=Inf, parse=TRUE, label=sprintf("alpha == %.2f", alphaValues$alpha), size=0.9*valueLabelSize, colour=axisLabelColor, vjust=-0.5)
  }
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      heatmap <- heatmap + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      print("Parameter 'borderColor' can only be applied to 'bw' theme.")
    }
  }
  if (!is.null(axisColor)) {
    heatmap <- heatmap + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    heatmap <- heatmap + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    heatmap <- heatmap + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideLegend) {
    heatmap <- heatmap + 
      guides(fill=FALSE)
  }
  # print plot
  plot(heatmap)
  
  if (class(data)=="data.frame") {
    print("Following items have been removed:")
    if (!is.null(removableItems)) {
      print(colnames(data)[removableItems])
      return (data[,c(-removableItems)])
    }
    else {
      print("none.")
    }
  }
  else {
    return(pcadata.varim)
  }
}


# Erzeugt eine rotierte Faktorladungen einer Hauptkomponentenanalyse
# (Paramter "data") mit einer bestimmten Anzahl an Faktoren (Parameter "factors")
# auf Grundlage der Varimax-Rotation
#
# Parameter:
# - data: the results (object) from a principal component analysis
#         (prcomp(myData...))
# - factors: the amount of factors. can be calculated from the
#            below function "factorcount"
varimaxrota <- function(data, factors) {
  # Faktorladungen berechnen
  # Die Faktorladungen erhält man durch Multiplikation der Eigenvektoren
  # mit der Diagonalmatrix der ausgewiesenen Standardabweichungen
  ladungen <- data$rotation%*%diag(data$sdev)
  # Zur Durchführung der VARIMAX-Rotation erzeugen wir eine Matrix
  # mit den Faktorladungen der ausgewählten Faktoren (Anzahl = Parameter "factors")
  ladb <- c()
  for (i in 1:factors) {
    ladb <- cbind(ladb, ladungen[,i])
  }
  # Varimax Rotation durchführen
  varib <- varimax(ladb)
  return (varib)
}
