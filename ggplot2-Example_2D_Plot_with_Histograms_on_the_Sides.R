library(ggplot2)
library(gridExtra)
 
mtcars$cyl <- ordered(mtcars$cyl)
p <- ggplot(mtcars, aes(mpg, hp, colour = cyl)) + geom_point()
 
p1 <- p + theme(legend.position = "none")
 
p2 <- ggplot(mtcars, aes(x=mpg, group=cyl, colour=cyl))
p2 <- p2 + stat_density(fill = NA, position="dodge")
p2 <- p2 + theme(legend.position = "none", axis.title.x=element_blank(),
axis.text.x=element_blank())
 
p3 <- ggplot(mtcars, aes(x=hp, group=cyl, colour=cyl))
p3 <- p3 + stat_density(fill = NA, position="dodge") + coord_flip()
p3 <- p3 + theme(legend.position = "none", axis.title.y=element_blank(),
axis.text.y=element_blank())
 
g_legend<-function(a.gplot){
tmp <- ggplot_gtable(ggplot_build(a.gplot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
return(legend)
}
 
legend <- g_legend(p)
 
grid.arrange(arrangeGrob(p2, legend, p1, p3, widths=unit.c(unit(0.75, "npc"), unit(0.25, "npc")), heights=unit.c(unit(0.25, "npc"), unit(0.75, "npc")), nrow=2))