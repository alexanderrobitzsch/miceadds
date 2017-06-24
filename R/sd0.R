
sd0 <- function(x, na.rm=TRUE){
	sd1 <- stats0( x = x , FUN = stats::sd , na.rm=na.rm )
	return(sd1)
}
