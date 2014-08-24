rFromWilcox <- function(wilcoxModel, N){
	z <- qnorm(1 - wilcoxModel$p.value / 2)
	r <- z / sqrt(N)
	# cat(wilcoxModel$data.name, "Effect Size, r = ", r, "\nZ-Value", z)
	return(list(r = r, z = z))
}