packs <- c("knitr", "ggplot2", "XML", "reshape2", "rCharts")
lapply(packs, require, character.only = T)

olympics <- 
  function(theurl = "http://www.sochi2014.com/en/medal-standings", include.zero = FALSE) {
    
    invisible(lapply(c('ggplot2', 'XML', 'reshape2') , require, character.only=TRUE))
    
    ## Grab Data, Clean and Reshape
    raw <- readHTMLTable(theurl, header=FALSE, 
                         colClasses = c(rep("factor", 2), rep("numeric", 4)))
    raw <- as.data.frame(raw)[, -1]
    colnames(raw) <- c("Country", "Bronze", "Silver", "Gold", "Total")
    raw <- raw[order(raw[, "Total"]), ]
    if (!include.zero) {
      raw <- raw[raw[, "Total"] != 0, ]
    }
    raw[, "Country"] <- factor(raw[, "Country"], levels = raw[, "Country"])
    rownames(raw) <- NULL
    mdat <- melt(raw, value.name = "Count", variable.name = "Place", id.var = "Country")
    
    ## Plot the Data
    plot1 <- ggplot(mdat, aes(x = Count, y = Country, colour = Place)) +
      geom_point() +
      facet_grid(.~Place) + theme_bw()+
      scale_colour_manual(values=c("#CC6600", "#999999", "#FFCC33", "#000000")) 
    print(plot1)
    
    return(invisible(raw))
  }

x <- olympics()

dTable(x)
$show('inline', include_assets = TRUE, cdn = TRUE)


