library(OIdata)
data(birds)
library(ggplot2)
library(gridExtra)

# line breaks between words for levels of birds$effect:
levels(birds$effect) <- gsub(" ", "\n", levels(birds$effect))

xyTable <- table(birds$sky, birds$effect)

qplot(1:10, 1:10, geom = "blank") + 
theme_bw() +
theme(line = element_blank(),
  text = element_blank()) +
annotation_custom(grob = tableGrob(xyTable,
  # change font sizes:
  gpar.coltext = gpar(cex = 1.2),
  gpar.rowtext = gpar(cex = 1.2)),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)