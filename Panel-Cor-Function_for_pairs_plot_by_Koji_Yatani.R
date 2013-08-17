panel.cor <- function(x, y, digits=3, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,use="complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  prefix <- "r = "
  prefix2 <- "\nCI lower = "
  prefix3 <- "\nCI upper = "
  prefix4 <- "\np = "
  rc <- cor.test(x,y)
  rci <- rc$conf.int
  rcp <- rc$p.value
  star <- symnum(rcp, corr = FALSE, na = FALSE, 
                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                 symbols = c("***", "**", "*", ".", " ")) 
  txt2 <- format(c(rci, 0.123456789), digits=digits)[1]
  txt3 <- format(c(rci, 0.123456789), digits=digits)[2]
  txt4 <- format(c(rcp, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, prefix2, txt2, prefix3, txt3, prefix4, txt4, " ", star, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1)
}